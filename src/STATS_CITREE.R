#/***********************************************************************
# * (C) Copyright Jon K Peck, 2024
# ************************************************************************/

# version 1.0.0

# history
# Dec-2024 Initial version


# helpers
gtxt <- function(...) {
    return(gettext(...,domain="STATS_CTREE"))
}

gtxtf <- function(...) {
    return(gettextf(...,domain="STATS_CTREE"))
}

loadmsg = "The R %s package is required but could not be loaded."
tryCatch(suppressWarnings(suppressPackageStartupMessages(library(partykit, warn.conflicts=FALSE))), error=function(e){
    stop(gtxtf(loadmsg,"partykit"), call.=FALSE)
}
)
# tryCatch(suppressWarnings(library(glmtree, warn.conflicts=FALSE)), error=function(e){
#     stop(gtxtf(loadmsg,"glmtree"), call.=FALSE)
# }
# )
# tryCatch(suppressWarnings(library(lmtree, warn.conflicts=FALSE)), error=function(e){
#     stop(gtxtf(loadmsg,"lmtree"), call.=FALSE)
# }
# )
tryCatch(suppressWarnings(library(strucchange, warn.conflicts=FALSE)), error=function(e){
    stop(gtxtf(loadmsg,"lmtree"), call.=FALSE)
}
)

tryCatch(suppressWarnings(library(vcd, warn.conflicts=FALSE)), error=function(e){
    stop(gtxtf(loadmsg,"vcd"), call.=FALSE)
}
)

mylist2env = function(alist) {
    env = new.env()
    lnames = names(alist)
    for (i in 1:length(alist)) {
        assign(lnames[[i]],value = alist[[i]], envir=env)
    }
    return(env)
}

Warn = function(procname, omsid) {
    # constructor (sort of) for message management
    lcl = list(
        procname=procname,
        omsid=omsid,
        msglist = list(),  # accumulate messages
        msgnum = 0
    )
    # This line is the key to this approach
    lcl = mylist2env(lcl) # makes this list into an environment
    
    lcl$warn = function(msg=NULL, dostop=FALSE, inproc=FALSE) {
        # Accumulate messages and, if dostop or no message, display all
        # messages and end procedure state
        # If dostop, issue a stop.
        
        if (!is.null(msg)) { # accumulate message
            assign("msgnum", lcl$msgnum + 1, envir=lcl)
            # There seems to be no way to update an object, only replace it
            m = lcl$msglist
            m[[lcl$msgnum]] = msg
            assign("msglist", m, envir=lcl)
        } 
        
        if (is.null(msg) || dostop) {
            spssdata.CloseDataConnection()
            lcl$display(inproc)  # display messages and end procedure state

            if (dostop) {
                stop(gtxt("End of procedure"), call.=FALSE)  # may result in dangling error text
            }
        }
    }
    
    lcl$display = function(inproc=FALSE) {
        # display any accumulated messages as a warnings table or as prints
        # and end procedure state, if any
        
        if (lcl$msgnum == 0) {   # nothing to display
            if (inproc) {
                spsspkg.EndProcedure()
                procok = TRUE
            }
        } else {
            procok = inproc
            if (!inproc) {
                procok =tryCatch({
                    StartProcedure(lcl$procname, lcl$omsid)
                    procok = TRUE
                },
                error = function(e) {
                    prockok = FALSE
                }
                )
            }
            if (procok) {  # build and display a Warnings table if we can
                table = spss.BasePivotTable("Warnings ","Warnings", isSplit=FALSE) # do not translate this
                rowdim = BasePivotTable.Append(table,Dimension.Place.row,
                                               gtxt("Message Number"), hideName = FALSE,hideLabels = FALSE)

                for (i in 1:lcl$msgnum) {
                    rowcategory = spss.CellText.String(as.character(i))
                    BasePivotTable.SetCategories(table,rowdim,rowcategory)
                    BasePivotTable.SetCellValue(table,rowcategory,
                                                spss.CellText.String(lcl$msglist[[i]]))
                }
                spsspkg.EndProcedure()   # implies display
            } else { # can't produce a table
                for (i in 1:lcl$msgnum) {
                    print(lcl$msglist[[i]])
                }
            }
        }
    }
    return(lcl)
}

casecorrect = function(vlist, warns) {
    # correct the case of variable names
    # vlist is a list of names, possibly including TO and ALL
    # unrecognized names are returned as is as the GetDataFromSPSS api will handle them

    dictnames = spssdictionary.GetDictionaryFromSPSS()["varName",]
    names(dictnames) = tolower(dictnames)
    dictnames['all'] = "all"
    dictnames['to'] = "to"
    correctednames = list()
    for (item in vlist) {
        lcitem = tolower(item)
        itemc = dictnames[[lcitem]]
        if (is.null(itemc)) {
            warns$warn(gtxtf("Invalid variable name!!!: %s", item), dostop=TRUE)
        }
        correctednames = append(correctednames, itemc)
    }

    return(correctednames)
}




procname=gtxt("Conditional Inference Trees")
warningsprocname = gtxt("Inference Trees Warnings")
omsid="STATSCIT"
warns = Warn(procname=warningsprocname,omsid=omsid)


# main worker
# The procedure can estimate and/or predict from a new tree
# or a saved tree that is reloaded
# The parameters for the model are always displayed if available

# Vignettes
viglist = list(partykit="https://cran.r-project.org/web/packages/partykit/vignettes/partykit.pdf",
    ctree="https://cran.r-project.org/web/packages/partykit/vignettes/ctree.pdf",
    mob="https://cran.r-project.org/web/packages/partykit/vignettes/mob.pdf")

# (ignorethis appears because subcommands are not allowed to be empty by the SPWS parser)
# The inner nodes plotting feature is available but is undocumented, because
# it can fail so must be used cautiously. 

docitree<-function(idvar=NULL, depvars=NULL, indvars=NULL, regrvars=NULL,factormode="labels", 
    estimate=TRUE, predict=FALSE, usefile=NULL,useworkspace=FALSE, labels=FALSE, 
    treestoprint=list(1), confusion=TRUE, keepusermissing=FALSE,
    treestoplot=list(1), mainheight=NULL, mainwidth=NULL, subheight=NULL, subwidth=NULL,
    innerplots=FALSE, fontsize=9, modelfile=NULL, datasetname=NULL,
    predtype="response", quantiles=list(.10, .50, .90), teststat="quadratic",
    testtype=c("Bonferroni"), splitstat="quadratic", alpha=0.05, mincriterion=1-alpha,
    minsplit=20, minbucket=7, minprob=0.01, maxdepth=99999, maxsurrogate=0,
    modeltype="ctree",  sctests=FALSE, termstats=TRUE,
    bonferroni=TRUE, trim=0.1, prune=NULL, ordinal="chisq", minsize=NULL, vignettelist=NULL,
    ignorethis=TRUE
    ) {

    domain<-"STATS_CITREE"
    setuplocalization(domain)
    
    # display selected R vignettes
    if (!is.null(vignettelist)) {
        for (v in vignettelist) {
            if (v %in% names(viglist)){
                #browseURL(viglist[[v]], "?target=\"_blank\"")
                browseURL(paste(viglist[[v]], "?_blank", sep=""))
            } else {
                warns$warn(gtxtf("The specified vignette, %s, is not available", v))
            }
        }
        warns$warn("End of procedure", dostop=TRUE)
    }
    # case correct some settings
    # if (ordinal == "l2") {ordinal="L2"}
    # if (testtype == "bonferroni") {testtype = "Bonferroni"}
    # if (testtype == "montecarlo") {testtype = "MonteCarlo"}
    # if (testtype == "univariate") {testtype = "Univariate"}
    # if (testtype == "teststatistic") {testtype = "Teststatistic"}
    testtype = ttcorrect(testtype, warns)
    
    weightvar = spssdictionary.GetWeightVariable()
    doest = estimate
    dopred = predict
    if (doest && (is.null(depvars) || is.null(indvars))) {
        warns$warn(gtxt("Dependent and Independent variables must be specified if estimating",
            dostop=TRUE))
    }
    
    if (doest && !is.null(usefile)) {
        warns$warn(gtxt("Cannot specify a model file while action is estimate"),
            dostop=TRUE)
    }
    
    printorplot = c(treestoprint, treestoplot)
    # if (any(printorplot > 0) && !doest && is.null(useworkspace) && is.null(usefile)) {
    #     warns$warn(gtxt("If plotting only, a model source must be specified"), dostop=TRUE)
    # }
    if (any(printorplot > 0) && !doest &&  is.null(usefile)) {
        warns$warn(gtxt("A  model source must be specified"), dostop=TRUE)
    }
    if (!doest && is.null(usefile) && !is.null(modelfile)) {
        warns$warn(gtxt("A model file cannot be saved, because no model has been estimated or loaded"),
            dostop=TRUE)
    }
    if (dopred && is.null(datasetname)) {
        warns$warn(gtxt("A dataset must be specified if making predictions"), dostop=TRUE)
    }
    
    if (dopred && !doest && is.null(usefile) && !useworkspace) {
        warns$warn(gtxt("If predicting, a model source must be specified"), dostop=TRUE)
    }
    
    if (dopred) {
        if (datasetname %in% spssdata.GetDataSetList()) {
            warns$warn(gtxt("The prediction dataset specified already exists.  Please close it or choose a different name"), 
                dostop=TRUE)
        }
    }
    
    if (!is.null(usefile) && useworkspace) {
         warns$warn("Cannot specify both file and model workspace as model source", dostop=TRUE)
    }
    if (!is.null(usefile)) {
        idvarcpy = idvar
        load(usefile)
        # might overwrite the id variable, so put it back
        # idvar must be specified in the prediction phase, but with a usefile, a previously specified
        # variable could be used if not overwridden by a newer specification.
        if (is.null(idvar)) {
            idvar = idvarcpy
        }
        if (dopred && is.null(idvar)) {
            warns$warn(gtxt("An id variable must be specified if making predictions"), dostop=TRUE)
        }
        
        ###save(restree, estdate, ncases, file="c:/temp/estdate.rdata")   #dbg
        if (!exists("restree")) {
            warns$warn(gtxtf("The specified file does not contain a tree model: %s",
                             usefile), dostop=TRUE)
        }
        filesource = usefile
        # depvars, indvars, and idvars names come from the usefile.  Data come from active file
    } else {
        filesource = "-- Workspace --"
    }
    if (!exists("restree") && !doest) {
        warns$warn(gtxt("No model is estimated or loaded from a file"), dostop=TRUE)
    }

    if (modeltype == "ctree" && !is.null(regrvars)) {
        warns$warn(gtxt("Regression variables are only for model-based recursive partitioning (MOB) models"), dostop=TRUE)
    }
    

    spsspkg.StartProcedure(gtxt("Conditional Inference Trees"),"STATS CITREE")
    
    # correct variable name case, including the id variable, if any
    # redundant for usefile, since already corrected
    depvars = casecorrect(depvars, warns)
    indvars = casecorrect(indvars, warns)
    if (!is.null(idvar)) {
        idvar = casecorrect(idvar, warns)
    }
    if (!is.null(regrvars)) {
        regrvars = casecorrect(regrvars, warns)
    }
    allvars = c(depvars, indvars, regrvars, weightvar)  # get data api requires case match
    nsplitvars = length(spssdata.GetSplitVariableNames())
    if (nsplitvars > 0) {
        warns$warn(gtxt("Split files is not supported by this procedure"), dostop=TRUE)
    }
    
    if (is.null(idvar)) {
        dta = spssdata.GetDataFromSPSS(allvars, missingValueToNA=TRUE, factorMode=factormode,
                keepUserMissing=keepusermissing)
    } else {
        dta = spssdata.GetDataFromSPSS(allvars, row.label=unlist(idvar), missingValueToNA=TRUE, 
            factorMode=factormode, keepUserMissing=keepusermissing)
    }
    for (v in depvars) {
        if ((modeltype == "moblinear" || modeltype == "moblogit") && is.factor(dta[[v]])) {
            warns$warn("Categorical dependent variables cannot be used with moblinear or moblogit models",
                dostop=TRUE)
        }
    }

    if (is.null(weightvar)) {
        wts = NULL
        ncases = nrow(dta)
    } else {
        wts = dta[[weightvar]]
        ncases = sum(dta[weightvar])
    }

    caption = gtxtf("Computed by R partykit package, version %s, ctree, and mob", packageVersion("partykit"))
    depvarsplus = paste(depvars, collapse="+")
    indvarsplus = paste(indvars, collapse="+")
    regrvarsplus = paste(regrvars, collapse="+")


    if (doest) {
        f = paste(depvarsplus, indvarsplus, sep="~", collapse="")
        if (!is.null(regrvars)) {
            f = paste(f, regrvarsplus, sep="|")
        }
        frml = as.formula(f)
        # control type depends on ctree vs MOB
        if (modeltype == "ctree") {
            controls = ctree_control(
                teststat = teststat,
                splitstat = splitstat,
                testtype = testtype,
                alpha = alpha,
                mincriterion = mincriterion,
                minsplit = minsplit,
                minbucket = minbucket,
                minprob = minprob,
                maxsurrogate=maxsurrogate,
                maxdepth = maxdepth
            )
            ctreecontrols = controls
            mobcontrols = NULL
        # glmtree and similar take mob_control parameters as extra arguments
        # This list will be used for options display, too.
        } else { 
            moblist=list(formula=frml, data = dta, weights=wts)
            #controls = list(
            mlis = list(formula=frml,  data=dta, weights=wts,
                alpha=alpha, bonferroni = bonferroni, maxdepth = maxdepth,
                minsize=minsize, trim=trim, prune=prune, ordinal = ordinal)
            controls = mob_control(
                alpha = alpha, 
                bonferroni = bonferroni,
                maxdepth = maxdepth,
                minsize = minsize,
                trim = trim,
                prune = prune,
                ordinal = ordinal
            )
            mobcontrols = controls
            ctreecontrols = NULL
            moblist=list(formula=frml, data = dta, weights=wts)
        }

        if (modeltype == "ctree") {
            func = ctree
            args = list(frml, control = controls, data = dta, weights=wts)

        } else if (modeltype == "moblinear") {
            func = lmtree
            #save(controls, file="c:/temp/controls.rdata")
            ###args =  (list(formula=frml, data = dta, weights=wts, control=controls))
            args = mlis
        } else {  #logit
            func = glmtree
            args = c(list(formula=frml, data = dta, weights=wts, family="binomial"), 
                control=controls)
        }
        ###tryCatch({if (exists("restree")) {rm(restree)}}, error=function(e) {})
        restree = NULL
        rm(restree)
        #print(control)
        restree = do.call(func, args = args)

        filesource="workspace"  # estimates are in the workspace
        estdate = date()
        if (!exists('ctreecontrol')) {ctreecontrol = NULL}
        if (!exists('mobcontrol')) {mobcontrol = NULL}
        if (!is.null(modelfile)) {
            if (!exists('restree')) {
                warns$warn(gtxt("There is no model file to save"), dostop=TRUE)
            }
            # and maybe a file, too
            save(restree, estdate, modeltype, frml, depvars, indvars, regrvars, idvar, ctreecontrols,
                 mobcontrols, keepusermissing,
            file=modelfile)
        }
    }
    if (!exists("restree")) {
        warns$warn(gtxt("There is no tree from estimation or a model file"), dostop=TRUE)
    }

    displayparameters(restree, modeltype, ctreecontrols, mobcontrols, depvars, indvars, regrvars, 
        factormode, idvar, filesource, estdate, modelfile, ncases, keepusermissing, maxsurrogate, caption,
        alpha, bonferroni, prune, ordinal)
    
    
    if (any(printorplot > 0)) {
        ntrees = length(restree)
        inttreestoprint = as.integer(treestoprint)
        if (max(unlist(inttreestoprint)) > ntrees) {
            treestoprint = inttreestoprint[inttreestoprint <= ntrees]
            warns$warn(gtxtf("There are only %s trees. Higher numbered nodes requested to print are ignored ", ntrees))
        }
        for (t in treestoprint) {
            if (t == 0) {
                break
            } else {
                displaytree(restree, t)
            }
        }
    }
    
    if (confusion) {
        displayconfusion(restree, depvars[[1]], dta, factormode, weightvar)
    }
    if (termstats) {
        mktermstatstable(restree, depvars, weightvar)
    }
    if (sctests) {
        sctable(restree)
    }
    plotfiles = list()
   
    ntrees = length(restree)
    inttreestoplot = as.integer(treestoplot)
    if (max(unlist(inttreestoplot)) > ntrees) {
        treestoplot = inttreestoplot[inttreestoplot <= ntrees]
        warns$warn(gtxtf("There are only %s trees. Higher numbered nodes requested to plot are ignored ", ntrees))
    }

    for (t in treestoplot) {
        if (t == 0) {   # trees up to the zeroth will get plotted
            break
        }
        plotfile = displayplot(restree, t, mainheight, mainwidth, subheight, subwidth,
            fontsize, innerplots, is.factor(dta[[1]]))
        plotfiles = append(plotfiles, plotfile)  # for file cleanup later
    }


    # Insert charts into Viewer.  XML schema does not allow lists of quoted strings,
    # so we have to do this one by one.
    # Insertion in reverse order makes it easier to keep track.
    # The SPSS R api does not provide the api to insert a chart explicitly, so we do it
    # via a tiny extension command written in Python.
    # Submit can't be used within procedure state
    
    
    spsspkg.EndProcedure()
    if (length(plotfiles) > 0) {
        pfilelist = tempfile("treeplots", tmpdir=tempdir(), fileext=".txt")
        f = file(pfilelist, open="w")
        for (line in plotfiles) {
            writeLines(line, f)
        }
        close(f)
        
        outlinelabel = sprintf("Variables: %s.  Chart ", paste(depvars, collapse=" "))
        labelparm = sprintf("'%s'", paste(treestoplot, collapse=" "))
        hidelog = ifelse(length(treestoprint) > 0, "YES", "NO") 
        cmd = sprintf("STATS INSERT CHART CHART='%s' HEADER='Tree' OUTLINELABEL='%s' OUTLINEPARM = %s HIDELOG=%s", 
            pfilelist, outlinelabel, labelparm, hidelog)
        # for (c in 1:length(plotfiles)) {
        #     labelnumber = c  
        #     outlinelabel = sprintf("Variable: %s.  Chart %s", depvars, treestoplot[c])
        #     # Submit api is supposed to supply command terminator but doesn't
        #     cmd = sprintf("STATS INSERT CHART CHART='%s' POSITION=%s, HEADER='Tree', OUTLINELABEL='%s %s'.", plotfiles[c], c - 1, outlinelabel)
        spsspkg.Submit(cmd)

    }
    # Due to a bug in the TextBlock api, tree output may be duplicated in a log block
    # HIDELOG will hide the most recent log block in the Viewer
    if (length(treestoprint) > 0) {
        spsspkg.Submit("STATS INSERT CHART HIDELOG=YES.")
    }

    if (dopred) {
        makedataset(dta, datasetname, depvars, factormode, idvar, restree, predtype, quantiles, warns)
    }
    
    
    # for (f in 1:length(plotfiles)) {
    #     tryCatch(file.remove(plotfiles[c]),
    #              error = function(e) (warns$warn(gtxtf("Plot file %s could not be removed", plotfiles[c])))
    #     )
    # }
    
    warns$display(inproc=FALSE)
}

ttcorrect = function(testtype, warns) {
    # correct test names and check for valid combinations
    newtypes = c()
    for (item in testtype) {
        if (item == "bonferroni") {item = "Bonferroni"}
        if (item == "montecarlo") {item = "MonteCarlo"}
        if (item == "univariate") {item = "Univariate"}
        if (item == "teststatistic") {item = "Teststatistic"}
        newtypes = append(newtypes, item)
    }
    if (length(newtypes) == 2 && !("Bonferroni" %in% newtypes && "MonteCarlo" %in% newtypes)) {
        warns$warn(gtxt("Invalid combination of testtype parameters"), dostop=TRUE)
    }

    return(newtypes)
}


displayparameters = function(restree, modeltype, ccontrols, mcontrols, depvars, indvars, regrvars, factormode, idvar, filesource, 
    estdate, modelfile, ncases, keepusermissing, maxsurrogate, caption,
    alpha, bonferroni, prune, ordinal) {
    # display parameters and input statistics

    if (modeltype == "ctree") {
    labels = list(
        gtxt("Dependent Variatbles"),
        gtxt("Independent Variables"),
        gtxt("Factor Mode"),
        gtxt("Id Variable"),
        gtxt("model Type"),
        gtxt("Number of Cases"),
        gtxt("Number of Tree Levels"),
        gtxt("Number of Terminal Nodes"),
        gtxt("Source File"),
        gtxt("Estimation Date"),
        gtxt("Saved Model File"),
        gtxt("Keep User Missing"),
        
        gtxt("Test Statistic"),
        gtxt("Split Statistic"),
        gtxt("Test Type"),
        # gtxt("Alpha"),  # used to derive criterion
        gtxt("Log Minimum Criterion"),
        gtxt("Minimum Node Cases Allowed"),
        gtxt("Minimum Cases in a Terminal Node"),
        gtxt("Minimum Proportion of Cases For a Terminal Node"),
        gtxt("Maximum Depth"),
        gtxt("Max Surrogates")
    )
    values = list(
        dl(depvars),
        dl(indvars),
        factormode, 
        dl(idvar),
        modeltype,
        ncases,
        depth(restree),
        width(restree),
        dl(filesource),
        estdate,
        dl(modelfile, gtxt("--not saved--")),
        ifelse(keepusermissing, gtxt("treat as valid"), gtxt("treat as missing")),
        paste(ccontrols["teststat"], collapse=", "),
        paste(ccontrols["splitstat"], collapse=", "),
        paste(ccontrols["testtype"], collapse=", "),
        ###controls["alpha"],
        ccontrols[["logmincriterion"]],
        dl(unlist(ccontrols[["minsize"]])),
        dl(unlist(ccontrols[["minbucket"]])),
        dl(unlist(ccontrols[["minprob"]])),
        dl(unlist(ccontrols[["maxdepth"]])),
        dl(unlist(ccontrols[["maxsuurrogate"]]))
    )
    } else {
        labels = list(
            gtxt("Dependent Variables"),
            gtxt("Independent Variables"),
            gtxt("Regression Variables"),
            gtxt("Factor Mode"),
            gtxt("Id Variable"),
            gtxt("Model Type"),
            gtxt("Number of Cases"),
            gtxt("Number of Tree Levels"),
            gtxt("Number of Terminal Nodes"),
            gtxt("Source File"),
            gtxt("Estimation Date"),
            gtxt("Saved Model File"),
            gtxt("Keep User Missing"),
            
            gtxt("Alpha"),
            gtxt("Bonferroni"),
            gtxt("Maximum Depth"),
            gtxt("Minimum Node Cases"),
            gtxt("Trimming"),
            gtxt("Post Pruning"),
            gtxt("Testtype for Ordinal Variables"),
            gtxt("Minimum Node Size Allowed")
        )
        values = list(
            dl(depvars),
            dl(indvars),
            dl(regrvars),
            factormode,
            dl(idvar),
            modeltype,
            ncases,
            depth(restree),
            width(restree),
            dl(filesource),
            estdate,
            dl(modelfile, gtxt("--not saved--")),
            ifelse(keepusermissing, gtxt("treat as valid"), gtxt("treat as missing")),
            mcontrols[["alpha"]],
            ifelse(mcontrols[["bonferroni"]], gtxt("yes"), gtxt("no")),
            mcontrols[["maxdepth"]],
            mcontrols[["minsize"]],
            mcontrols[["trim"]],
            ifelse(is.null(mcontrols[["prune"]]), "--", mcontrols[["prune"]]),
            mcontrols[["ordinal"]],
            ifelse(is.null(unlist(mcontrols[["minsize"]])), "--", unlist(mcontrols[["minsize"]]))
            )
    }
    df = cbind(values)
    df = data.frame(df, row.names=labels)
    spsspivottable.Display(
        df, 
        title=gtxt("Tree Parameters"),
        outline=gtxt("Tree Parameters"),
        templateName="STATSCITREEPARMS",
        caption=caption
    )
}

dl = function(x, nullvalue="--") {
    # return collapsed list or nullvalue if NULL
    if (is.null(x)) {
        return(nullvalue)
    } else {
        return(paste(x, collapse=", "))
    }
}
    

displaytree = function(result, t) {
    # Display the tree in the Viewer as a textblock
    # result is the output from ctree - could be a subset
    # t is the tree number
    # Assumes that already in a procedure state
    
    # displaying a numbered subtree produces a display
    # with the unweighted counts, so we avoid that for
    # the main tree
    
    if (t == 1) {
        block = capture.output(print(result))
    } else {
        block = capture.output(print(result[1]))
    }
    
    block = paste(block, collapse="\n")
    textBlock = spss.TextBlock(sprintf("Conditional Inference Tree %s", t),
        block)
}

sctable = function(tree) {
    # display pivot table of the structural test statistics and p values
    
    # tree is a ctree or mob result
    
    scdf = NULL
    sc = sctest(tree)
    for (i in 1:length(sc)) {
        df = t(data.frame(sc[i]))
        if (length(df) == 0) {
            next
        }
        scdf = rbind(scdf, df)
    }
    # change Xdigits row names to read "node digits"
    rn = gsub("^X(\\d+)", "node \\1", row.names(scdf))
    row.names(scdf) = rn
    scnames = names(sc)
    spsspivottable.Display(scdf, title=gtxt("Structural Tests"), templateName="treetests",
                           caption="Computed by R strucchange, ctree, mob packages",
                           hiderowdimtitle=TRUE, hidecoldimtitle=TRUE)
}


displayplot = function(result, tree, mainheight, mainwidth, subheight, subwidth, 
    fontsize, innerplots, dvfactor) {
    # display a tree plot
    # tree is the list of trees to display
    # result is the ctree output
    # mainheight, mainwidth are the size parameters for tree #1
    # subheight and subwidth are the size parameters for other trees
    # innerplots is TRUE if inner nodes should be plotted
    # dvfactor is true if the (first) dep var is a factor
    
    reso = 72
    
    if (is.null(mainheight)) {
        height = max(depth(result[1]), 4) * 1
    } else {
        height = mainheight
    }
    if (is.null(mainwidth)) {
        width = max(min(width(result[1]), 10), 4) * 1
    } else{
        width = mainwidth
    }
    
    if (tree != 1) {
           if (!is.null(subheight)) {
               height = subheight
           } else {
               height = max(depth(result[tree]), 4) * 1
           }
            if (!is.null(subwidth)) {
                width = subwidth
            } else {
                width = max(min(width(result[tree]), 10), 4) * 1
            }
    }

    pfile = tempfile("treeplot", tmpdir=tempdir(), fileext=paste(tree,".png", sep=""))
    png(pfile, units="in", res=72, height=height, width=width)

    if (!innerplots) {
        plot(result[tree], gp = gpar(fontsize = fontsize))
    } else {
        if (dvfactor) {
            plot(result[tree], inner_panel=node_barplot, gp = gpar(fontsize = fontsize))
        } else {
            plot(result[tree], inner_panel=node_boxplot, gp = gpar(fontsize = fontsize))
        }
    } 
    dev.off()
    return(pfile)
}


ptypes = list("response", "prob", "node", "quantile")
makedataset = function(dta, ds, depvars, factormode, idvarname, restree, ptype, quantiles, warns) {
    # create new SPSS dataset and populate with predict results

    # dta is the data to predict from
    # ds is the dataset name for predictions (must not already exist)
    # restree is the tree to predict from
    # idvarname is the name of the id variable - guaranteed legal
    # factormode is "levels" or "labels" and controls the predicted response type
    #   for categorical variables
    # ptype is the type of predicted values to create
    # quantiles is a list of quantiles for that output type as decimal values
    # warns is the warnings function
    
    # id var values will always appear as type character but not as a factor.
    # It should be converted back to its original type when written back

    # generate dataset of predictions of specified type
    
    if (length(depvars) > 1 && !(ptype %in% c("node", "response", "prob"))) {
        warns$warn(gtxt("Some prediction typess can only be made for a single dependent variable"),
            dostop = TRUE)
    }

    inputdict = spssdictionary.GetDictionaryFromSPSS(c(idvarname, depvars))

    idinfo = inputdict[, inputdict['varName', ] == idvarname]
    depvardict = inputdict[inputdict['varName',] %in% depvars]
    save(inputdict,  depvardict, depvars, file="c:/temp/depvar.rdata")
    depvartypes = as.integer(depvardict['varType',])
    depvartype = as.integer(depvardict["varType",])
    iscategoricals = depvardict['varMeasurementLevel',] %in% c("nominal", "ordinal")
    anycategorical = any(depvardict['varMeasurementLevel',] %in% c("nominal", "ordinal"))
    anyscale = any(depvardict['varMeasurementLevel',] %in% c("scale"))
    if (anycategorical && anyscale) {
        warns$warn(gtxt("Cannot combine categorical and scale variables in a prediction",
            dostop=TRUE))
    }
    if (ptype == "quantile" && anycategorical) {
        warns$warn(gtxt("Quantile predictions are not available for categorical variables"),
                   dostop=TRUE)
    }
    if (ptype == "prob" && anyscale) {
        warns$warn(gtxt("Class category predictions are not available for scale variables",
            dostop = TRUE))
    }

    if (ptype != "quantile") {
        p2 = predict(restree, type=ptype, newdata=dta)
        preddata = data.frame(predict(restree, type=ptype, newdata=dta))
    }  
    else {
        preddata = data.frame(predict(restree, type=ptype, at=quantiles, newdata=dta))
    }
    if (ptype == "node") {
        colnames(preddata) = "Node"
    }
    save(depvardict, preddata, ptype, restree, dta, file="c:/temp/pred.rdata")
    columnnames = fixnames(preddata, ptype)
    colnames(preddata) = columnnames
    
    # for factors when using the label data option, response will be a string
    # if it is a factor but using levels, it could be either string or numeric
    # so follow the dependent variable types
    
    # it is unclear whether strings will be utf-8 or some other encoding
    # the factor of 2 below is probably not needed
    dvtypes = c()
    depvarfmts = c()
    for (i in 1:length(depvars)) {
        if (factormode == "labels" && iscategoricals[[i]]) {
            dvtypes[[i]] = max(nchar(levels(dta[1,i]), type="bytes", allowNA=TRUE)) * 2
        } else if (factormode == "levels" || !iscategoricals[[i]]) {
            dvtypes[[i]] = depvartypes[[i]]
        } else if (!iscategoricals[i]) {
            dvtypes[i] = depvartypes[[i]]
        }
        
        # numerical result values will be returned in a numeric variable
        if (typeof(dta[[1,i]]) == "double") {
            depvarfmts[i] = "F8.2"
            width = 0
        } else {
            depvarfmts[i] = paste("A", as.character(dvtypes[[i]]), sep="")
            width = dvtypes[[i]]    # utf-8 expansion?
        }
    }
    # name, label, type, format, level
    ##save(inputdict, restree, dta, depvars, idvarname, file="c:/temp/makedataset.rdata")
    dictlist = list()
    dictlist[[1]] = idinfo
    
    # construct prediction variable dictionary according to prediction type
    for (i in 1:length(depvars)) {
        if (ptype == "response") {
            varspec = c(
                paste(gtxt("Response"), depvars[[i]], sep="_"),
                "",
                as.integer(dvtypes[[i]]), 
                depvarfmts[i], 
                "nominal")
        dictlist[[i+1]] = varspec
        # only one variable allowed for quantiles
        } else if (ptype == "quantile") {
            for (q in 1:length(colnames(preddata))) {
                varspec = c(paste(paste(gtxt("quantile"), depvars[[i]], sep="_"), 
                    colnames(preddata)[q], sep=""),
                    "", 
                    0, 
                    "F8.2", 
                    "scale")
                dictlist[[q+1]] = varspec
            }
            save(preddata, varspec, dictlist, ptype, iscategoricals, file="c:/temp/quantiles.rdata")
        } else if (ptype == "prob")  {
            # return df of predicted probabilities for each level of the dv factor
            # one variable
            cnames = colnames(preddata)
            for (p in 1:length(colnames(preddata))) {
                #varspec = c(paste(columnnames[p], depvars[[i]], sep="_"), 
                varspec = c(cnames[p],
                    "", 
                    0,
                    "F8.3", 
                    "scale")
                dictlist[[p + 1]] = varspec
            }
         } else if (ptype == "node") {
                varspec = c(paste(ptype, "Node", sep="_"), 
                "",
                0, 
                "F8.0", 
                "nominal")
            dictlist[[2]] = varspec
            break
         }
    }

    dict = spssdictionary.CreateSPSSDictionary(dictlist)


    tryCatch({
        save(ds, dict, dictlist, dta, preddata, file="c:/temp/newdataset.Rdata")
        spssdictionary.SetDictionaryToSPSS(ds, dict)
        thedata = data.frame(row.names(dta), preddata)
        spssdata.SetDataToSPSS(ds, thedata) #row.names(preddata)?
        spssdictionary.EndDataStep()
    },
    error=function(e) {
        spssdictionary.EndDataStep()
        warns$warn(gtxtf("Failed to create dataset %s, %s", ds, e),
                   dostop=FALSE)
    }
    )
}

displayconfusion = function(tree, dv, dta, factormode, wts) {
    # display confusion tables statistics
    # tree is the estimated tree
    # dv is the name of the (first) dependent variable
    # dta is the data
    # factormode is "levels" or "labels"
    # wts is the name of the weight variable or NULL
    
    if (!is.factor(dta[[dv]])) {
        ###warns$warn(gtxt("Confusion tables are only available for categorical variables"))
        return()
    }

    # value labels have a values part and a labels part
    dvlabels = spssdictionary.GetValueLabels(dv)
    dvpred = predict(tree, newdata=dta)
    
    if (is.factor(dta[[dv]]) && !is.factor(dvpred)) {
        return()
    }
    
    # In some cases, e.g., moblogistic, the glmtree predictions with regression
    # will be values that are not in the domain of the dependent variable

    # labelled categories will appear even if there are no cases for some labels
    if (is.null(wts)) {
        concounts = xtabs(~dta[[dv]] + dvpred)
    } else {
        concounts = xtabs(dta[[wts]] ~ dta[[dv]] + dvpred)
    }
    # map values to value labels (except for the last one)

    rows = rownames(concounts)
    cols = colnames(concounts)
    correct = 0
    for (r in rows) {
        if (r %in% cols) {
            correct = correct + concounts[r, r]
        }
    }

    concounts = addmargins(concounts)
    pctcorrect = correct / concounts["Sum", "Sum"] * 100.
    if (correct > 0) {
        caption = gtxtf("Correct: count: %s, Percentage: %s", correct, round(pctcorrect, 2))
    } else {
        caption = ""
    }
    # replace row and column names with value labels where available
    for (v in 1:(length(rows))) {
        indx = match(rows[[v]], dvlabels$values)
        if (!is.na(indx)) {
            rows[[v]] = dvlabels$labels[indx]
        }
    }
    rows[[length(rows) + 1]] = gtxt("Total")
    rownames(concounts) = rows    
    for (v in 1:(length(cols))) {
        indx = match(cols[[v]], dvlabels$values)
        if (!is.na(indx)) {
            cols[[v]] = dvlabels$labels[indx]
        }
    }
    cols[[length(cols) + 1]] = gtxt("Total")
    colnames(concounts) = cols
    concounts = as.data.frame.matrix(concounts)
    spsspivottable.Display(concounts,
        title=gtxtf("Confusion Matrix - %s", dv),
        rowdim = gtxt("Observed"), coldim = gtxt("Predicted"),
        hiderowdimtitle=FALSE, hidecoldimtitle=FALSE,
        templateName="confusionCounts",
        outline="Confusion Matrix", 
        format=formatSpec.Count,
        caption = caption)
}


fixnames = function(dta, ptype) {
    # return list of legal SPSS variable names for the names in dta
    
    # dta is the data frame to correct
    # this function may not perfectly match SPSS name rules
    
    newnames = list()
    for (name in colnames(dta)) {
        # avoid the regular grep function!
        if (ptype == "quantile" && grepl("^%[0-9.].*%$", name)) {  # quantile test
            newname = paste("Q_", name, sep="")
            newname = gsub("%", "", newname)
        } else {
            newname = gsub("[[:punct:]]", "_", name)   # eliminate punctuation
            newname = gsub("(^[0-9])", "c_\\1)", newname)  # fix names starting with digit
            newname = gsub("\\.$", "_", newname)   # fix names ending with "."
            newname = gsub("^\\.", "_", newname)   # fix names starting with "."
        }
        # ensure that there are no duplicate names
        basename = newname
        for (i in 1:1000) {
            if (!(newname %in% newnames)) {
                break
            } else {
                newname = paste(basename, i, sep="")
            }
        }
        newnames = append(newnames, newname)
    }
    return(newnames)
}

Mode <- function(x, wt){
    #return the weighted mode
    
    # x is the data vector
    # wt is the weight.  NULL if no weight var
    
    # A node could be empty if there are multiple dependent variables
    
    if (length(x) == 0) {
        return(NULL)
    } else{
        if (is.null(wt)) {
            a = xtabs(x) # x     is a vector
        } else {
            a = xtabs(wt~x)
        }
        anames = names(a)
        themax = anames[which.max(a)]
        return(themax)
    }
}

mktermstatstable = function(tree, depvars, wt) {
    # show table of terminal statistics for each dependent variable
    # tree is the estimated tree
    # depvars is a list of dependent variables
    # wt is the name of the weight variable or NULL
    
    for (dv in depvars) {
        df = terminalstats(tree, dv, wt)
        dfnames = c(gtxt("Node"), "Number of Cases", 
            ifelse(is.factor(dta[[dv]]), gtxt("Mode"), gtxt("Mean"))
        )
        colnames(df) = dfnames
        spsspivottable.Display(
            df, 
            title=gtxtf("Terminal Node Statistics: %s", dv),
            outline=gtxt(gtxt("Statistics"),
            templateName="STATSCITREESTATS"),
            hiderowdimtitle=TRUE,
            hidecoldimtitle=TRUE,
            hiderowdimlabel=TRUE,
            caption=gtxt("Even if there are multiple modes, only one is shown per node")
        )
    }
}

terminalstats = function(tree, depvar, wt) {
    # make a table of appropriate terminal node statistics
    # mean for scale and mode for categorical
    # and return two-column data frame
    
    #tree is the estimated tree
    #depvar is the name of the dependent variable
    # if the data are unweighted, the weight variable has values 1
    
    thestats = c()
    ncases = c()
    tnodes = nodeids(tree, terminal=TRUE)
    nodedata = data_party(tree, tnodes)
    save(nodedata, file="c:/temp/nodedata.rdata")
    # mob models may not have a (weights) variable if unweighted, but ctree models do
    cnames = colnames(nodedata[[1]])
    hasweights = "(weights)" %in% cnames

    for (n in 1:length(tnodes)) {
        thevar = nodedata[[n]][[depvar]]
        if (is.factor(thevar)) {
            s = Mode(thevar, nodedata[[n]][["(weights)"]])  # NULL if no weight
        } else {
            if (is.null(nodedata[[n]][["(weights)"]])) {
                s = mean(thevar)
            } else
                s = weighted.mean(thevar, w=nodedata[[n]][["(weights)"]])
        }
        thestats = append(thestats, s)
        if (hasweights) {
            ncases = append(ncases, sum(nodedata[[n]][['(weights)']]))
        } else {
            ncases = append(ncases, nrow(nodedata[[n]]))
        } 
    }
    return(data.frame(node=tnodes, ncases=ncases, statistics=thestats))
}    


setuplocalization = function(domain) {
    # find and bind translation file names
    # domain is the root name of the extension command .R file, e.g., "SPSSINC_BREUSCH_PAGAN"
    # This would be bound to root location/SPSSINC_BREUSCH_PAGAN/lang

    fpath = Find(file.exists, file.path(.libPaths(), paste(domain, ".R", sep="")))
    if (!is.null(fpath)) {
        bindtextdomain(domain, file.path(dirname(fpath), domain, "lang"))
    }
} 


Run<-function(args){

    cmdname = args[[1]]
    args <- args[[2]]
    # print("submitting")
    # spsspkg.Submit("STATS INSERT CHART HIDELOG=YES")
    # stop()

    # variable keywords are typed as varname instead of existingvarlist in
    # order to allow for case correction of names later, since the data fetching apis are
    # case sensitive
    
    oobj <- spsspkg.Syntax(templ=list(
        spsspkg.Template("DEPVARS", subc="", ktype="varname", var="depvars", islist=TRUE),
        spsspkg.Template("INDVARS", subc="", ktype="varname", var="indvars", islist=TRUE),
        spsspkg.Template("IDVAR", subc="", ktype="varname", var="idvar", islist=FALSE),
        spsspkg.Template("REGRVARS", subc="", ktype="varname", var="regrvars", islist=TRUE),
        spsspkg.Template("ESTIMATE", subc="", ktype="bool", var="estimate",  islist=FALSE),
        spsspkg.Template("PREDICT", subc="", ktype="bool", var="predict",  islist=FALSE),
        spsspkg.Template("FACTORMODE", subc="", ktype="str", var="factormode",
            vallist=list("levels", "labels"), islist=FALSE),
        spsspkg.Template("MODELTYPE", subc="", ktype="str", var="modeltype", 
            vallist=list("ctree", "moblinear", "moblogit"), islist=FALSE),
        
        spsspkg.Template("USEWORKSPACE", "", ktype="bool", var="useworkspace", islist=FALSE),
        spsspkg.Template("USEFILE", "", ktype="literal", var="usefile", islist=FALSE),
                
        spsspkg.Template("LABELS", subc="OUTPUT", ktype="bool", var="labels",
            vallist=list(0), islist=FALSE),
        
        spsspkg.Template("TREES", subc="DISPLAY", ktype="str", var="treestoprint", islist=TRUE),
        spsspkg.Template("CONFUSION", subc="DISPLAY", ktype="bool", var="confusion", islist=FALSE), 
        spsspkg.Template("PLOTS", subc="DISPLAY", ktype="int", var="treestoplot", islist=TRUE),
        spsspkg.Template("MAINHEIGHT", subc="DISPLAY", ktype="float", var="mainheight", islist=FALSE),
        spsspkg.Template("MAINWIDTH", subc="DISPLAY", ktype="float", var="mainwidth", islist=FALSE),
        spsspkg.Template("SUBHEIGHT", subc="DISPLAY", ktype="float", var="subheight", islist=FALSE),
        spsspkg.Template("SUBWIDTH", subc="DISPLAY", ktype="float", var="subwidth", islist=FALSE),
        spsspkg.Template("FONTSIZE", subc="DISPLAY", ktype="float", var="fontsize", islist=FALSE),
        spsspkg.Template("SCTESTS", subc="DISPLAY", ktype="bool", var="sctests", islist=FALSE),
        spsspkg.Template("INNERPLOTS", subc="DISPLAY", ktype="bool", var="innerplots", islist=FALSE),
        spsspkg.Template("TERMSTATS", subc="DISPLAY", ktype="bool", var="termstats", islist=FALSE),
        
        spsspkg.Template("MODELFILE", subc="SAVE", ktype="literal", var="modelfile", islist=FALSE),
        spsspkg.Template("DATASET", subc="SAVE", ktype="varname", var="datasetname"), 
        spsspkg.Template("PREDTYPE", subc="SAVE", ktype="str", var="predtype",
            vallist=list("response", "prob", "quantile", "node"), islist=FALSE),
        spsspkg.Template("QUANTILES", subc="SAVE", ktype="float", var="quantiles", islist=TRUE),
        spsspkg.Template("IGNORETHIS", subc="SAVE", ktype="bool", var="ignorethis"),
        
        spsspkg.Template("KEEPUSERMISSING", subc="OPTIONS", ktype="bool", var="keepusermissing",
              islist=FALSE),
        spsspkg.Template("TESTSTAT", subc="OPTIONS", ktype="str", var="teststat"),
        spsspkg.Template("TESTTYPE", subc="OPTIONS", ktype="str", var="testtype",
            vallist=list("bonferroni", "montecarlo", "univariate", "teststatistic"), islist=TRUE),
        spsspkg.Template("SPLITSTAT", subc="OPTIONS", ktype="str", var="splitstat",
            vallist=list("quadratic", "maximum"), islist=FALSE),
        spsspkg.Template("ALPHA", subc="OPTIONS", ktype="float", var="alpha",
            vallist=list(0.,1.), islist=FALSE),
        spsspkg.Template("MINCRITERION", subc="OPTIONS", ktype="float", var="mincriterion",
            islist=FALSE),
        spsspkg.Template("MINSPLITSUM", subc="OPTIONS", ktype="float", var="minsplit",
            vallist=list(1), islist=FALSE),
        spsspkg.Template("MINTERMINAL", subc="OPTIONS", ktype="float", var="minbucket",
            vallist=list(1), islist=FALSE),
        spsspkg.Template("MINPROB", subc="OPTIONS", ktype="float", var="minprob",
            vallist=list(0), islist=FALSE),
       spsspkg.Template("MAXSURROGATES", subc="OPTIONS", ktype="int", var="maxsurrogate",
            islist=FALSE),
            spsspkg.Template("MAXDEPTH", subc="OPTIONS", ktype="float", var="maxdepth",
             vallist=list(1), islist=FALSE),
       spsspkg.Template("BONFERRONI", subc="OPTIONS", ktype="bool", var="bonferroni",
                        islist=FALSE),
       spsspkg.Template("TRIM", subc="OPTIONS", ktype="float", var="trim",
                        islist=FALSE),
       spsspkg.Template("PRUNE", subc="OPTIONS", ktype="str", var="prune",
            vallist=list("aic", "bic"),  islist=FALSE),
       spsspkg.Template("ORDINAL", subc="OPTIONS", ktype="str", var="ordinal",
            vallist=list("chisq", "max","l2"),  islist=FALSE),
       spsspkg.Template("MINSIZE", subc="OPTIONS", ktype="int", var="minsize",
                        islist=FALSE),
       
       spsspkg.Template("VIGNETTELIST", subc="VIGNETTE", ktype="literal", var="vignettelist",
            islist=TRUE)
        ))

    if ("HELP" %in% attr(args,"names"))
        #writeLines(helptext)
        helper(cmdname)
    else {
        res <- spsspkg.processcmd(oobj, args, "docitree")
    }
}


helper = function(cmdname) {
    # find the html help file and display in the default browser
    # cmdname may have blanks that need to be converted to _ to match the file
    
    fn = gsub(" ", "_", cmdname, fixed=TRUE)
    thefile = Find(file.exists, file.path(.libPaths(), fn, "markdown.html"))
    if (is.null(thefile)) {
        print("Help file not found")
    } else {
        browseURL(paste("file://", thefile, sep=""))
    }

    if (exists("spsspkg.helper")) {
        assign("helper", spsspkg.helper)
    }
}
