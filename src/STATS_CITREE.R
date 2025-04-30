#/***********************************************************************
# * (C) Copyright Jon K Peck, 2024
# ************************************************************************/

# version 1.0.0

# history
# Dec-2024    Initial version
# apr-19-2025 Initial release


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
                    spsspkg.StartProcedure(lcl$procname, lcl$omsid)
                    procok = TRUE
                },
                error = function(e) {
                    prockok = FALSE
                }
                )
            }
            if (procok) {  # build and display a Warnings table if we can
                table = spss.BasePivotTable("Warnings and Messages","Warnings", isSplit=FALSE) # do not translate this
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

getextloc = function() {
    # find where extensions are installed
    
    rantag = runif(1, 0.05, 1)
    cmd = sprintf('preserve.
    set olang=english.
    oms select tables /if subtypes=["System Settings"]
    /destination format=oxml xmlworkspce="%s", viewer=no.
    show ext.
    omsend.
    restore.', rantag)

    spsspkg.Submit(cmd)
    
    pth = '//pivotTable//group[@text="EXTPATHS EXTENSIONS"]//category[@text="Setting"]/cell/@*'
    res <- spssxmlworkspace.EvaluateXPath(rantag, context="/", pth)
    spssxmlworkspace.DeleteXmlWorkspaceObject(rantag)
    return(res[1])
}


procname=gtxt("Conditional Inference Trees")
warningsprocname = gtxt("Inference Trees Warnings")
omsid="STATSCIT"
warns = Warn(procname=warningsprocname,omsid=omsid)


# main worker
# The procedure can estimate and/or predict from a new tree
# or a saved tree that is reloaded
# The parameters for the model are always displayed if available



# (ignorethis appears because subcommands are not allowed to be empty by the SPWS parser)
# The inner nodes plotting feature is available but is undocumented, because
# it can fail so must be used cautiously. 

docitree<-function(idvar=NULL, depvars=NULL, indvars=NULL, regrvars=NULL,factormode="labels", 
    estimate=TRUE, predict=FALSE, usefile=NULL,useworkspace=FALSE, labels=FALSE, 
    treestoprint=list(1), confusion=TRUE, missingvalues="exclude", multiway=FALSE,
    treestoplot=list(1), mainheight=NULL, mainwidth=NULL, subheight=NULL, subwidth=NULL,
    innerplots=FALSE, fontsize=10, plotfile=NULL, plotformat="png", plotbg="ivory2", matchbg=TRUE,
    modelfile=NULL, datasetname=NULL,
    predtype=list("response"), quantiles=list(.10, .50, .90), teststat="quadratic",
    testtype=c("Bonferroni"), splitstat="quadratic", alpha=0.05, mincriterion=1-alpha,
    minsplit=20, minbucket=7, minprob=0.01, maxdepth=99999, maxsurrogate=0,
    modeltype="ctree",  sctests=FALSE, termstats=TRUE, nodepaths=FALSE, terminalplots=TRUE,
    bonferroni=TRUE, trim=0.1, prune=NULL, ordinal="chisq", minsize=NULL, vignettelist=NULL,
    ignorethis=TRUE, cores=1
    ) {

    domain<-"STATS_CITREE"
    setuplocalization(domain)
    
    # display any selected vignettes and stop
    if (!is.null(vignettelist)) {
        displayvignettes(vignettelist)
    }
    doest = estimate
    dopred = predict
    # If a temporary workspace copy has been saved, and not estimating or 
    # loading a model file, reload it
    wscopy = getOption("SPSSCITREE")
    idvarcpy = idvar
    if (!doest && dopred && is.null(usefile) && (is.null(wscopy) || !file.exists(wscopy))) {
        warns$warn(gtxt("No tree is available for use in prediction"), dostop=TRUE)
    }
    if (!is.null(wscopy) && file.exists(wscopy) && is.null(usefile)
        && !doest) {
        load(wscopy)
        warns$warn(gtxtf("workspace restored from %s", file.info(wscopy)$mtime), dostop=FALSE)
        if (is.null(idvar)) {
            idvar = idvarcpy
        } 
    }
    # case correct some settings
    testtype = ttcorrect(testtype, warns)
    
    weightvar = spssdictionary.GetWeightVariable()

    if (doest && (is.null(depvars) || is.null(indvars))) {
        warns$warn(gtxt("Dependent and independent variables must be specified if estimating"),
            dostop=TRUE)
    }
    
    if (doest && !is.null(usefile)) {
        warns$warn(gtxt("Cannot specify a model file while action is estimate"),
            dostop=TRUE)
    }
    
    printorplot = c(treestoprint, treestoplot)
    # if (any(printorplot > 0) && !doest && is.null(useworkspace) && is.null(usefile)) {
    #     warns$warn(gtxt("If plotting only, a model source must be specified"), dostop=TRUE)
    # }
    if (any(printorplot > 0) && !doest &&  is.null(usefile) && !exists("restree")) {
        warns$warn(gtxt("A  model source must be specified"), dostop=TRUE)
    }
    if (!doest && is.null(usefile) && !is.null(modelfile) && !exists("restree")) {
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
        datasetlist = spssdata.GetDataSetList()
        if (tolower(datasetname) %in% tolower(datasetlist)) {
            warns$warn(gtxt("The prediction dataset specified already exists.  Please close it or choose a different name"), 
                dostop=TRUE)
        }
        if ("*" %in% datasetlist) {
            warns$warn(gtxt("The input dataset must have a name if doing predictions.  Please assign one and rerun the procedure."),
                dostop=TRUE)
        }
    }
    
    if (!is.null(usefile) && useworkspace) {
         warns$warn("Cannot specify both file and model workspace as model source", dostop=TRUE)
    }
    idvarcpy = idvar
    if (!is.null(usefile)) {
        load(usefile)
        # might overwrite the id variable, so put it back
        # idvar must be specified in the prediction phase, but with a usefile, a previously specified
        # variable could be used if not overwridden by a newer specification.

        if (!exists("restree")) {
            warns$warn(gtxtf("The specified file does not contain a tree model: %s",
                             usefile), dostop=TRUE)
        }
        filesource = usefile
        # depvars, indvars, and idvars names come from the usefile.  Data come from active file
    } else {
        filesource = "-- Workspace --"
    }

    if (is.null(idvar)) {
        idvar = idvarcpy
    }
    if (dopred && is.null(idvar)) {
        warns$warn(gtxt("An id variable must be specified if making predictions"), dostop=TRUE)
    }
    if (!exists("restree") && !doest) {
        warns$warn(gtxt("No model is estimated or loaded from a file"), dostop=TRUE)
    }

    if (modeltype == "ctree" && !is.null(regrvars)) {
        warns$warn(gtxt("Regression variables are only for model-based recursive partitioning (MOB) models"), dostop=TRUE)
    }
    
    # mob models do not support multiple cores on Windows
    # unclear about ctree models, but ctree does not raise error
    if (modeltype != "ctree" && Sys.info()['sysname'] == "Windows") {
        cores = 1
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
    if (length(intersect(depvars, union(indvars, regrvars))) > 0) {
        warns$warn(gtxt("at least one dependent variable appears in the independent or regression variable list"),
            dostop=TRUE)
        }
    nodata = length(depvars) == 0

    # missingValueToNA is set to FALSE so that sysmis can be distinguished from user missing
    # It is useful for trees to be able to exclude cases with sysmis but retain user missing
    # as valid for analytical purposes
    
    # The argument missingValueToNA specifies whether missing values of numeric 
    # variables are converted to the R NA value. The default is FALSE, 
    # which specifies that missing values
    # (system and user) of numeric variables are converted to the R NaN.
    # Categorical variables get the NA value (if those cases are kept).
    # It appears that factors always get NA.
    
    # factorMode doc: The value "levels"
    # specifies that categorical variables are converted to factors whose levels are the values that occur in the
    # data. The value "labels" specifies that categorical variables are converted to factors whose levels are
    # the value labels of the variables. Values in the data for which value labels do not exist have a level equal
    # to the value itself. Value labels whose associated value does not occur in the data are included as empty
    # factor levels.
    # The levels of the resulting R factor are always sorted in ascending order of the data
    # values, even when factorMode="labels".
    tryCatch(
        {if (is.null(idvar)) {
            dta = spssdata.GetDataFromSPSS(allvars, missingValueToNA=TRUE, factorMode=factormode,
                    keepUserMissing=FALSE)
        } else {
            dta = spssdata.GetDataFromSPSS(allvars, row.label=unlist(idvar), missingValueToNA=TRUE, 
                factorMode=factormode, keepUserMissing=FALSE)
        }
        }, error=function(e) {warns$warn(paste(gtxt("error fetching data"), e, sep="\n"), dostop=TRUE)}
    )
    # remove missing values from scale or all variables
    dta = cleannaf(dta, missingvalues, factormode, warns)   # needed or not?
    
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
        if (is.factor(dta[[weightvar]])) {
            warns$warn(gtxt("The weight variable must have a scale measurement level to use this procedure."),
                dostop=TRUE)
        }
        tryCatch({
            wts = dta[[weightvar]]
            ncases = sum(dta[weightvar])
        },
        error = function(e) {
            print(e)
            wts = NULL
            ncases = nrow(dta)
            warns$warn(gtxt("Model was estimated with case weights, but dataset is not
currently weighted or the weight variable is invalid. Ignoring weight"), dostop=FALSE)
        }
        )
    }

    caption = gtxtf("Computed by the R partykit package, version %s", packageVersion("partykit"))
    depvarsplus = paste(depvars, collapse="+")
    indvarsplus = paste(indvars, collapse="+")
    regrvarsplus = paste(regrvars, collapse="+")


    if (doest) {
        if (length(depvars) == 0 || length(indvars) == 0) {
            warns$warn(gtxt("Dependent or independent variable specification missing"), dostop=TRUE)
        }
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
                maxdepth = maxdepth,
                multiway = multiway,
                cores = cores
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
                minsize=minsize, trim=trim, prune=prune, ordinal = ordinal,
                catsplit=ifelse(multiway, "multiway", "binary"))
            controls = mob_control(
                alpha = alpha, 
                bonferroni = bonferroni,
                maxdepth = maxdepth,
                minsize = minsize,
                trim = trim,
                prune = prune,
                ordinal = ordinal,
                catsplit = ifelse(multiway, "multiway", "binary")
            )
            mobcontrols = controls
            ctreecontrols = NULL
            moblist=list(formula=frml, data = dta, weights=wts)
        }

        if (modeltype == "ctree") {
            func = "ctree"
            args = list(frml, control = controls, data = dta, weights=wts)

        } else if (modeltype == "moblinear") {
            func = "lmtree"
            ###args =  (list(formula=frml, data = dta, weights=wts, control=controls))
            args = mlis
        } else {  #logit
            func = "glmtree"
            args = mlis
            # args = c(list(formula=frml, data = dta, weights=wts, family="binomial"), 
            #     control=controls)
        }
        ###tryCatch({if (exists("restree")) {rm(restree)}}, error=function(e) {})
        restree = NULL
        rm(restree)
        tryCatch({
            restree = do.call(func, args = args)
        }, error=function(e) {
            warns$warn(paste(gtxt("error estimating tree"), e, sep="\n"), dostop=TRUE)}
        )
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
                 mobcontrols, missingvalues, factormode,
            file=modelfile)
        }
        # save a temporary copy of the workspace after deleting the previous one if present
        if (!is.null(wscopy)) {
            tryCatch({
                if (file.exists(wscopy)) {
                    file.remove(wscopy)
                }
            },
            error = function(e) {NULL}
            )
        }
        wscopy = tempfile("workspacecopy", tmpdir=tempdir(), fileext=".rdata")
        save(restree, estdate, modeltype, frml, depvars, indvars, regrvars, idvar, ctreecontrols,
             mobcontrols, missingvalues, factormode,
             file=wscopy)
        options(SPSSCITREE=wscopy)
    }
    if (!exists("restree")) {
        warns$warn(gtxt("There is no tree from estimation or a model file"), dostop=TRUE)
    }

    displayparameters(restree, modeltype, ctreecontrols, mobcontrols, depvars, indvars, regrvars, 
        factormode, idvar, filesource, estdate, modelfile, ncases, missingvalues, maxsurrogate, caption,
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
    varimptbl(restree, modeltype)
    
    if (confusion && !nodata) {
        displayconfusion(restree, depvars[[1]], dta, factormode, weightvar, missingvalues)
    }
    if (termstats && !nodata) {
        mktermstatstable(restree, dta, depvars, weightvar, nodepaths)
    }
    # if (nodepaths && !nodata) {
    #     pathrules(restree)
    # }
    if (sctests && !nodata) {
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
        dvfactor = is.factor(dta[[depvars[[1]]]])
        thisplotfile = displayplot(restree, dvfactor, t, mainheight, mainwidth, subheight, subwidth,
            fontsize, innerplots, depvars, 
            terminalplots, plotfile, plotformat, plotbg, matchbg, modeltype)
        plotfiles = append(plotfiles, thisplotfile)  # for file cleanup later
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
        labelparm = sprintf("%s", paste(treestoplot, collapse=" "))
        hidelog = ifelse(length(treestoprint) > 0, "YES", "NO") 
        cmd = sprintf("STATS INSERT CHART CHARTLIST='%s' HEADER='Tree' OUTLINELABEL='%s ' LABELPARM = %s HIDELOG=%s", 
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
    # if (length(treestoprint) > 0) {
    #     spsspkg.Submit("STATS INSERT CHART HIDELOG=YES.")
    # }

    if (dopred) {
        makedataset(dta, datasetname, depvars, factormode, idvar, restree, predtype, 
            quantiles, missingvalues, modeltype, warns)
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
    estdate, modelfile, ncases, missingvalues, maxsurrogate, caption,
    alpha, bonferroni, prune, ordinal) {
    # display parameters and input statistics

    if (modeltype == "ctree") {
    labels = list(
        gtxt("Dependent Variables"),
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
        gtxt("User Missing Values"),
        
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
        ifelse(missingvalues=="include", gtxt("include"), gtxt("exclude")),
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
            gtxt("User Missing Values"),
            
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
            ifelse(missingvalues == "include", gtxt("include"), gtxt("exclude")),
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
        block = capture.output(print(result[t]))
    }
    
    block = paste(block, collapse="\n")
    textBlock = spss.TextBlock(sprintf("Conditional Inference Tree %s", t),
        block)
}


varimptbl = function(tree, modeltype) {
    # display model importance table for tree if modeltype==ctree
    
    if (modeltype != "ctree") {
        return()
    }
    vtbl = varimp(tree)
    vtbl = vtbl[order(vtbl, decreasing=TRUE)]
    spsspivottable.Display(vtbl, title=gtxt("Variable Importance"), 
            templateName="treevarimp", coldim=gtxt("Importance"),
            rowdim=gtxt("Variable"), collabels=gtxt("Log Likelihood"),
             hiderowdimtitle=FALSE, hidecoldimtitle=FALSE)
}


sctable = function(tree) {
    # display pivot table of the structural test statistics and p values
    
    # tree is a ctree or mob result
    
    scdf = NULL
    sc = sctest(tree)
    if (!is.null(sc)) {
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
}


displayplot = function(result, dvfactor, tree, mainheight, mainwidth, subheight, subwidth, 
    fontsize, innerplots, depvars, 
    terminalplots, plotfile, plotformat, plotbg, matchbg, modeltype) {
    # display a tree plot
    # tree is the list of trees to display
    # result is the ctree output
    # dvfactor is TRUE if depvarw are factors
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
    tryCatch(
        png(pfile, units="in", res=72, height=height, width=width, bg=plotbg),
        error = function(e) {warns$warn(e, dostop=TRUE)}
    )
    # cex.main=2 does not work here
    if (dvfactor) {
        innertype = node_barplot
    } else { 
        innertype = node_boxplot
    }
    innercolor = ifelse(matchbg, plotbg, "white")
    lwd = ifelse(fontsize < 10, 1, 2)
    drawtheplot(result, tree, fontsize, depvars, lwd, plotbg, innercolor, terminalplots, innerplots, 
        innertype, modeltype)
    
    if (!is.null(plotfile)) {
        plotformat = toupper(plotformat)
        # build filepath for plot including plot number
        fp = sub("\\.[^.]*?$", "", plotfile, ignore.case=TRUE)
        fp = paste(fp, tree, ".", toupper(plotformat), sep="")

        tryCatch(
            {
            if (plotformat == "PDF") {
                pdf.options(height=height, width=width, pointsize=fontsize)
                pdf(file=fp, height=height, width=width, bg=plotbg)
            } else if (plotformat == "SVG") {
                svg(filename=fp, width=width, height=height, pointsize=fontsize, 
                    bg=plotbg)
            } else {
                png(file=fp, units="in", res=72, height=height, width=width, bg=plotbg)
            }
            # plot(result[tree], gp = gpar(pointsize = fontsize,
            #     height=height, width=width, pointsize=fontsize), tp_args=list(bg=plotbg),
            #     main=paste(depvars, collapse =", "))
            # dev.off()
            drawtheplot(result, tree, fontsize, depvars, lwd, plotbg, innercolor, terminalplots, 
                innerplots, innertype, modeltype)
            warns$warn(gtxtf("Tree plot saved to %s", fp))
            },
            error = function(e) {warns$warn(gtxtf("Cannot write to specified file: %s", fp),
                dostop=FALSE)
            }
        )
    }    
    return(pfile)
}

drawtheplot = function(result, tree, fontsize, depvars, lwd, plotbg, innercolor, terminalplots, 
    innerplots, innertype, modeltype) {

    # make interior  and terminal plot backgrounds slightly brighter (except for white)
    ic = col2rgb(innercolor) / 255 + .05
    innercolor = rgb(min(ic[1], 1), min(ic[2], 1), min(ic[3], 1))

    # mob models don't appear to support inner plots
    if (modeltype %in% c("moblinear", "moblogit")) {
        innerplots=FALSE
    }

    tryCatch(
        {
            if (terminalplots) {
                if (!innerplots) {
                    plot(result[tree], gp = gpar(fontsize = fontsize, lwd=lwd), 
                         main=paste(depvars, collapse =", "), bg=plotbg, tp_args=list(bg=plotbg))
                         ###main=paste(depvars, collapse =", "), bg=plotbg, tp_args=NULL)
                } else {
                    plot(result[tree], gp = gpar(fontsize = fontsize, lwd=lwd), 
                         inner_panel=innertype(result[tree], bg=innercolor),
                         main=paste(depvars, collapse =", "), bg=plotbg, tp_args=list(bg=innercolor))
                }
            }
            else {
                if (!innerplots) {
                    plot(result[tree], gp = gpar(fontsize = fontsize, lwd=lwd), 
                         main=paste(depvars, collapse =", "), terminal_panel = node_id, 
                         bg=plotbg, tnex = 1)
                } else {
                    plot(result[tree], gp = gpar(fontsize = fontsize, lwd=lwd), 
                         main=paste(depvars, collapse =", "), terminal_panel = node_id, 
                         inner_panel=innertype(result[tree], bg=innercolor),
                         bg=plotbg, tnex = 1)
                }
            }
        },
        error = function(e) {print(e)
            warns$warn(gtxt("Incomplete plot due to terminal node issue"), dostop=FALSE)
        }
    )
    dev.off()
}
# code to suppress everything but the node id in terminal nodes
# This function written by Achim Zeileis
node_id <- function(node) {
    id <- format(id_node(node))
    
    node_vp <- viewport(x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                        just = "center", gp = gpar(),
                        width = unit(1, "strwidth", paste0(" ", id, " ")),
                        height = unit(1.2, "lines"),
                        name = paste0("node_terminal", id))
    pushViewport(node_vp)
    
    grid.rect(gp = gpar(fill = "lightgray"))
    grid.text(x = unit(0.5, "npc"), y = unit(0.5, "npc"), id)
    
    upViewport()
}

###plot(ct, terminal_panel = node_id, tnex = 1)


ptypes = list("response", "prob", "node", "quantile")
makedataset = function(dta, ds, depvars, factormode, idvarname, restree, ptype, 
    quantiles, missingvalues, modeltype, warns) {
    # create new SPSS dataset and populate with predict results

    # dta is the data to predict from
    # ds is the dataset name for predictions (must not already exist)
    # restree is the tree to predict from
    # idvarname is the name of the id variable - guaranteed legal
    # factormode is "levels" or "labels" and controls the predicted response type
    #   for categorical variables
    # ptype is the [list of] types of predicted values to create
    # quantiles is a list of quantiles for that output type as decimal values
    # missingvalues indicates whether these values have already been excluded
    # warns is the warnings function
    
    # id var values will always appear as type character but not as a factor.
    # It should be converted back to its original type when written back

    # generate dataset of predictions of specified type
    
    # if (length(depvars) > 1 && !(ptype %in% c("node", "response", "prob"))) {
    #     warns$warn(gtxt("Some prediction typess can only be made for a single dependent variable"),
    #         dostop = TRUE)
    # }

    if (modeltype != "ctree" && length(depvars) > 1) {
        warns$warn(gtxt("MOB models can only predict one dependent variable.
        First variable will be used."), dostop=FALSE)
        depvars = depvars[1]
    }
    inputdict = spssdictionary.GetDictionaryFromSPSS(c(idvarname, depvars))

    idinfo = inputdict[, inputdict['varName', ] == idvarname]
    depvardict = inputdict[inputdict['varName',] %in% depvars]
    depvartypes = as.integer(depvardict['varType',])
    iscategoricals = depvardict['varMeasurementLevel',] %in% c("nominal", "ordinal")
    anycategorical = any(depvardict['varMeasurementLevel',] %in% c("nominal", "ordinal"))
    anyscale = any(depvardict['varMeasurementLevel',] %in% c("scale"))
    if (anycategorical && anyscale) {
        warns$warn(gtxt("Cannot combine categorical and scale variables in a prediction",
            dostop=TRUE))
    }
    if ("quantile" %in% ptype && anycategorical) {
        warns$warn(gtxt("Quantile predictions are not available for categorical variables"),
                   dostop=TRUE)
    }
    if ("prob" %in%ptype && anyscale) {
        warns$warn(gtxt("Class category predictions are not available for scale variables",
            dostop = TRUE))
    }

    if (modeltype %in% c("moblinear", "moblogit") && 
        length(setdiff(ptype, list("response", "node")) > 0)) {
        warns$warn(gtxt('Prediction type for MOB models must be "response" or "node'),
            dostop=TRUE)
    }
    casecount = nrow(dta)
    if (missingvalues != "exclude") {
        dta = cleanna(dta, "exclude")
        casecountpost = nrow(dta)
        if (casecount != casecountpost) {
            warns$warn(gtxtf("%s cases not predicted due to missing data", casecount - casecountpost),
            dostop=FALSE)
        }
    }
    nonq = intersect("quantile", ptype) > 0
    pdatasets = c()
    if ('node' %in% ptype) {
        preddatanode = data.frame(predict(restree, type="node",
             newdata=dta, na.action=na.pass))
        pdatasets = append(pdatasets, "preddatanode")
    }
    
    if ('response' %in% ptype) {
        preddataresponse = data.frame(predict(restree, type='response',
            newdata=dta, na.action=na.pass))
        pdatasets = append(pdatasets, "preddataresponse")
    }
    
    if ("prob" %in% ptype) {
        preddataprob = data.frame(predict(restree, type="prob",
            newdata=dta, na.action=na.pass))
        pdatasets = append(pdatasets, "preddataprob")
        pcolumnnames = fixpnames(dta, colnames(preddataprob), depvars)
    }
    
    if ("quantile" %in% ptype) {
        if (max(unlist(quantiles)) > 1 || min(unlist(quantiles)) < 0) {
            warns$warn(gtxt("Quantile not in [0, 1]"), dostop=TRUE)
        }
        preddataquantile = data.frame(predict(restree, type="quantile", at=unique(unlist(quantiles)), newdata=dta,
            na.action=na.pass))
        # column names are wrong from predict when more than one dv, so correct them
        qcolumnnames = spread(depvars, quantiles)
        pdatasets = append(pdatasets, "preddataquantile")
    }

    allpred = do.call(cbind, sapply(pdatasets, get,
        envir=sys.frame(sys.parent(0)), simplify=FALSE))

    # for factors when using the label data option, response will be a string
    # if it is a factor but using levels, it could be either string or numeric
    # so follow the dependent variable types
    
    # it is unclear whether strings will be utf-8 or some other encoding
    # the factor of 2 below is probably not needed
    dvtypes = c()
    depvarfmts = c()

    for (i in 1:length(depvars)) {
        if (factormode == "labels" && iscategoricals[[i]]) {
            dvtypes[[i]] = max(nchar(levels(dta[[1,depvars[[i]]]]), type="bytes", allowNA=TRUE)) * 2
            if (dvtypes[[i]] == 0 || is.null(dvtypes[[i]])) {
                dvtypes[[i]] = 12   # arbitrary choice.  something haw gone wrong
            }
        } else if (factormode == "levels" || !iscategoricals[[i]]) {
            dvtypes[[i]] = depvartypes[[i]]  # declared SPSS type
        } else if (!iscategoricals[i]) {
            dvtypes[i] = depvartypes[[i]]
        }
        
        # numerical result values will be returned in a numeric variable
        # numeric and not a factor
        if (dvtypes[[i]] == 0 && (!iscategoricals[[i]]
            || factormode == "levels")) {
            depvarfmts[[i]] = "F8.2"
            width = 0
            # character coded as levels
        } else if(dvtypes[[i]] > 0 && factormode == "levels") {
            depvarfmts[[i]] = paste("A", dvtypes[[i]], sep="")
            # factor and labels - use widest 
        } else if (is.factor(dta[[depvars[[i]]]]) && factormode == "labels") {
            width = dvtypes[[i]]
            depvarfmts[i] = paste("A", width, sep="")
        }
    }

    # name, label, type, format, level
    dictlist = list()
    dictlist[[1]] = idinfo
    
    # construct prediction variable dictionary according to prediction type
    npvar = 2
    qsaved = FALSE
    psaved = FALSE

    if ("preddatanode" %in% pdatasets) {
        varspec = c("Node",
            "",
            0, 
            "F8.0", 
            "nominal")
        dictlist[[npvar]] = unlist(varspec)
        npvar = npvar + 1
    }
    
    for (item in pdatasets) {
        for (i in 1:length(depvars)) {
            if (item == "preddataresponse") {# one pred value per dv per case
                varspec = c(
                    paste(gtxt("Response"), depvars[[i]], sep="_"),
                    "",
                    as.integer(dvtypes[[i]]), 
                    depvarfmts[i], 
                    "nominal")
                dictlist[[npvar]] = unlist(varspec)
                npvar = npvar + 1
    
            } else if (item == "preddataquantile") {  # scale only
                if (qsaved) {
                    next
                }
                qsaved = TRUE
                for (q in 1:length(qcolumnnames)) { # k q values per case
                    ###varspec = c(paste(paste(gtxt("quantile"), depvars[[i]], sep="_"), 
                    varspec = c(
                        qcolumnnames[[q]],
                        "", 
                        0, 
                        "F8.3", 
                        "scale")
                    dictlist[[npvar]] = unlist(varspec)
                    npvar = npvar + 1
                }
                
            } else if (item == "preddataprob")  {  # categorical only
                # return df of predicted probabilities for each level of the dv factor
                # one variable
                if (psaved) {
                    next
                }
                psaved = TRUE
                for (p in 1:length(pcolumnnames)) {
                    varspec = c(pcolumnnames[p],
                        "", 
                        0,
                        "F8.3", 
                        "scale")
                    dictlist[[npvar]] = unlist(varspec)
                    npvar = npvar + 1
                }
                
             }
        }
    }

    # add the dependent variables
    for (i in 1:length(depvars)) {
        varspec = c(
            depvars[[i]],
            "",
            as.integer(dvtypes[[i]]),
            depvarfmts[[i]],
            "nominal")
        dictlist[[length(dictlist) + 1]] = unlist(varspec)
    }
    allnames = c()
    for (i in 1:length(dictlist)) {
        allnames[i] = dictlist[[i]][1]
    }

    preddatanames = fixqnames(allnames, depvars) # ensure no duplicate names
    allpred = data.frame(row.names(allpred), allpred, dta[unlist(depvars)])

    # update dictionary
    for (i in 1:length(dictlist)) {
        dictlist[[i]][1] = preddatanames[[i]]  # ovwriting assigned names :-(
    }
    dict = spssdictionary.CreateSPSSDictionary(dictlist)

    # use csv transfer instead of data/dictionary apis for major performance reasons
    csvtospss(ds, dict, allpred)

    # not used. value labels get much too long
    # if ('node' %in% ptype) {
    #     tryCatch(
    #         {
    #         paths = smartquote(unlist(pathrules(restree)))
    #         tnodes = nodeids(restree, terminal=TRUE)
    #         if (length(tnodes) > 0 && max(nchar(paths, type="bytes")) <= 110) {
    #             # make value label list
    #             vls = unlist(c(rbind(tnodes, unlist(paths), "\n")))
    #             cmd = sprintf("VALUE LABELS Node %s", vls)
    #         }
    #     }, error=function(e) {warns$warn(e, dostop=FALSE)}
    #     )
    # }
}

smartquote = function(s) {
    # return smartquoted s in double quotes
    
    s = sub('"', '""', s)
    return(paste('"', s, '"', sep=""))
}

csvtospss = function(preddataset, dict, preds) {
    # save a temporary csv file and read into SPSS
    # preddataset is the datgaset name for the prediction data
    # activedatset is the name of the active dataset
    # dict is the spss dictionary object for the prediction data
    # preds is the data
    
    csvfile = tempfile("csvpred", tmpdir=tempdir(), fileext=".csv")
    write.csv(preds, file=csvfile, row.names=FALSE)
    spsscmd = sprintf('
        PRESERVE.
        SET DECIMAL DOT.
        GET DATA  /TYPE=TXT
        /FILE="%s"
        /ENCODING="UTF8"
        /DELCASE=LINE
        /DELIMITERS=","
        /QUALIFIER=""""
        /ARRANGEMENT=DELIMITED
        /FIRSTCASE=2
        /VARIABLES=', csvfile)
    
    varspecs = list()
    for (v in 1:ncol(dict)) {
        if (!strsplit(dict[["varFormat", v]], "\\d+") %in% c('A', 'F')) {
            dict[["varFormat", v]] = "F"
        }
    }
    for (v in 1:ncol(dict)) {
        varspecs = append(varspecs, paste(dict[["varName", v]], dict[["varFormat", v]], sep=" "))
    }
    varspecs = paste(varspecs, collapse="\n")
    activedataset = getactivedsname()
    cmd = paste(spsscmd, varspecs, ".\n", sprintf("dataset name %s.", preddataset), collapse="\n")
    spsspkg.Submit(cmd)
    spsspkg.Submit("RESTORE.")
    spsspkg.Submit(sprintf("DATASET ACTIVATE %s.", activedataset))
    spsspkg.Submit("EXECUTE")
    unlink(csvfile)
    
    # Can't delete the file - permission denied
    # tryCatch(
    #     {
    #     file.remove(csvfile)
    #     }, error = function(e) {warns$warn(paste(gtxtf("Temporary file could not be deleted: %s", csvfile),
    #             e, collapse="\n"), dostop=FALSE)}
    # )
}

getactivedsname = function() {
    # There is no api for this
    
    ds = spssdata.GetOpenedDataSetList()
    spsspkg.Submit("DATASET NAME X44074_60093_.")  # renames active dataset
    ds2 = spssdata.GetOpenedDataSetList()
    diff = setdiff(ds, ds2)  # find out which one changed
    spsspkg.Submit("DATASET ACTIVATE X44074_60093_.")  # reactivate the previously active one
    cmd = sprintf("DATASET NAME %s.", diff)   # and give it back its name
    spsspkg.Submit(cmd)
    return(diff)
}

fixqnames = function(allnames, depvars) {
    # ensure no duplicate column names.  return all names
    
    # allnames is a vector of variable names without depvar names
    # depvars is the vector of dependent variable names
    # depvars names that duplicate earlier names will be modified
    
    # ensure that names are short enough for SPSS limit
    nondv = length(allnames) - length(depvars)
    for (vindex in 1:nondv) {
        allnames[[vindex]] = substr(allnames[[vindex]], 1, 63)   # could be fooled due to utf-8 bytes vs uicode
    }
    
    # check for duplicate preceding varname
    # earliest name wins

    # regular names are already case corrected, but generated names might
    # match differently cased var names
    allnameslower = sapply(allnames, tolower)
    for (vindex in 2:length(allnames)) {
        basename = allnames[[vindex]]
        newname = basename
        for (i in 1:1000) {
            if (tolower(newname) %in% allnames[1:(vindex-1)])  {
                newname = paste(basename, i, sep="_")
            } else {
                allnames[[vindex]] = newname
                break
            }
        }
    }
    return(allnames)
}
fixpnames = function(dta, pnames, depvars) {
    # return column names for prob prediction
    
    # categorical depvars only
    # pnames is the names generated by predict, type="prob"
    # depvars is a list of the dependent variable names
    
    # if more than one dependent variable is specified, the
    # predict names are wrong.  For now, guessing the categories
    # but going with letter codes if that doesn't work
    
    if (length(depvars) == 1) {
        newnames = paste(depvars[[1]], pnames, sep="_")
    } else {
        newnames = c()
        for (dv in depvars) {
            dvlevels = levels(dta[[dv]])
            newname = paste(dv, dvlevels, sep="_")
            newnames = append(newnames, newname)
        }
        if (length(newnames) == length(pnames)) {
            return(newnames)
        }
        # if the replacement names differ in length from pnames,
        # just use letters as we don't know why.
        
        # generate repeating sequence of letters of total length = number of names
        # downstream code will adjust for duplicates in the rare case when > 26
        newnames = rep_len(LETTERS, length(pnames))
    }
    return(newnames)
    
}

spread = function(varnames, quantiles) {
    # return a list of interleaved names: for each item in varnames concatenate it
    # with all the quantiles
    
    newnames = c()
    
    for (i in 1:length(varnames)) {
        for (j in 1:length(quantiles)) {
            newnames = append(newnames, paste(varnames[[i]], quantiles[[j]], sep="_Q"))
        }
    }
    return(newnames)
}

displayconfusion = function(tree, dv, dta, factormode, wts, missingvalues) {
    # display confusion tables statistics
    # tree is the estimated tree
    # dv is the name of the (first) dependent variable
    # dta is the data
    # factormode is "levels" or "labels"
    # wts is the name of the weight variable or NULL

    if (is.null(dta)) {
        warns$warn(gtxt("No data available.  Confusion table omitted."), dostop=FALSE)
    }
    if (!is.factor(dta[[dv]])) {
        ###warns$warn(gtxt("Confusion tables are only available for categorical variables"))
        return()
    }
    if (missingvalues != "exclude") {
        dta = cleanna(dta, "exclude")
    }
    # value labels have a values part and a labels part
    dvlabels = spssdictionary.GetValueLabels(dv)
    dta = dta[complete.cases(dta),]
    dvpred = data.frame(predict(tree, newdata=dta, na.action=na.pass))
    if (is.factor(dta[[dv]]) && !is.factor(dvpred[[1]])) {
        return()
    }
    if (nrow(dta) != nrow(dvpred)) {
        warns$warn(gtxt("Confusion table not available due to missing prediction(s)"),
            dostop=FALSE)
        return()
    }
    # In some cases, e.g., moblogistic, the glmtree predictions with regression
    # will be values that are not in the domain of the dependent variable

    if (is.null(wts)) {
        concounts = xtabs(~dta[[dv]] + dvpred[[1]])
    } else {
        concounts = xtabs(dta[[wts]] ~ dta[[dv]] + dvpred[[1]])
    }
    # map values to value labels (except for the last one)
    for (i in 1:nrow(concounts)) {
        if (rownames(concounts)[[i]] == "") {rownames(concounts)[[i]] = " "}
    }
    for (i in 1:ncol(concounts)) {
        if (colnames(concounts)[[i]] == "") {colnames(concounts)[[i]] = " "}
    }
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

# subpunct approximates characters invalid in SPSS variable names
subpunct = "[-’‘%&'()*+,/:;<=>?\\^`{|}~’]"
fixnames = function(dta, ptype) {
    # return list of legal, nonduplicative SPSS variable names for the input list
    
    # dta is a list/vector of names to correct
    # this function may not perfectly match SPSS name rules
    
    newnames = c()
    for (name in dta) {
        # avoid the regular grep function!
        # if (ptype == "quantile" && grepl("^%[0-9.].*%$", name)) {  # quantile test
        #     newname = paste("Q_", name, sep="")
        #     newname = gsub("%", "", newname)
        # } else {
        newname = gsub(subpunct, "_", name)   # eliminate disallowed characters
        newname = gsub("(^[0-9])", "c_\\1", newname)  # fix names starting with digit
        newname = gsub("^\\.|\\.$", "_", newname)  # fix names starting or ending with "."
    # }
        # ensure that there are no duplicate names
        basename = newname
        for (i in 1:1000) {
            if (!(newname %in% newnames)) {
                break
            } else {
                newname = paste(basename, i, sep="_")
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
        themax = c(anames[which.max(a)], a[[which.max(a)]]/sum(a) * 100)
        return(themax)
    }
}

mktermstatstable = function(tree, dta, depvars, wt, nodepaths) {
    # show table of terminal statistics for each dependent variable
    
    # tree is the estimated tree
    # dta is the case data (not currently used)
    # depvars is a list of dependent variables
    # wt is the name of the weight variable or NULL
    # nodepaths indicates whether or not to include a column of node paths
    
    for (dv in depvars) {
        df = terminalstats(tree, dv, wt)
        if (is.null(df)) {
            warns$warn(gtxt("Node summary table is not available"), dostop=FALSE)
            return()
        }
        df[[1]] = as.character(df[[1]])
        if (!is.null(df)) {
            if (!is.factor(dta[[dv]])) {
                dfnames = c(gtxt("Node"), gtxt("Number of Cases"), gtxt("Percent of Cases"),
                    ifelse(is.factor(dta[[dv]]), gtxt("Mode"), gtxt("Mean")))
            } else {
                dfnames = c(gtxt("Node"), gtxt("Number of Cases"), gtxt("Percent of Cases"),
                    ifelse(is.factor(dta[[dv]]), gtxt("Mode")), gtxt("Percent at Mode"))
            }
            colnames(df) = dfnames
            if (nodepaths) {
                paths = pathrules(tree)
                df[ncol(df) + 1] = paths
            }
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
}

terminalstats = function(tree, depvar, wt) {
    # make a table of appropriate terminal node statistics
    # mean for scale and mode for categorical
    # and return four or five-column data frame
    
    #tree is the estimated tree
    #depvar is the name of the dependent variable
    # if the data are unweighted, the weight variable has values 1
    
    thestats = c()
    thestatsmode = c()
    thestatspct = c()
    ncases = c()
    tnodes = nodeids(tree, terminal=TRUE)
    nodedata = data_party(tree, tnodes)

    # mob models may not have a (weights) variable if unweighted, but ctree models do
    cnames = colnames(nodedata[[1]])
    if (!is.null(cnames)) {
        hasweights = "(weights)" %in% cnames
        if (length(tnodes > 1)) {
            for (n in 1:length(tnodes)) {
                thevar = nodedata[[n]][[depvar]]
                if (is.factor(thevar)) {
                    s = Mode(thevar, nodedata[[n]][["(weights)"]])  # NULL if no weight  includes pct at mode
                    thestatsmode = append(thestatsmode, s[[1]])
                    thestatspct = append(thestatspct, round(as.numeric(s[[2]]),4))
                } else {
                    if (is.null(nodedata[[n]][["(weights)"]])) {
                        s = mean(thevar)
                    } else
                        s = weighted.mean(thevar, w=nodedata[[n]][["(weights)"]])
                        thestats = append(thestats, s)
                    }
                
                if (hasweights) {
                    ncases = append(ncases, sum(nodedata[[n]][['(weights)']]))
                } else {
                    ncases = append(ncases, nrow(nodedata[[n]]))
                } 
            }
        if (!is.factor(thevar)) {
            return(data.frame(node=tnodes, ncases=ncases, ncases/sum(ncases) * 100, statistics=thestats))
        } else {
            dff = data.frame(node=tnodes, ncases=ncases, ncases/sum(ncases) * 100, 
                statisticsm=thestatsmode, statisticsp=thestatspct)
            return(dff)
        }
    } else {
        return(NULL)
    }
    return(NULL)
    }
}    

# get_path <- function(object) {
#     ## list of kids per node (NULL if terminal)
#     kids <- lapply(as.list(object$node), "[[", "kids")
#     
#     ## recursively add node IDs of children
#     add_ids <- function(x) {
#         ki <- kids[[x[1L]]]
#         if(is.null(ki)) {
#             return(list(x))
#         } else {
#             x <- lapply(ki, "c", x)
#             return(do.call("c", lapply(x, add_ids)))
#         }
#     }
#     add_ids(1L)
# }

#regex = '([^ ]+) %in% c\\('
#repl = "ANY(\\1,"

f = function(s) {gsub('([^ ]+) %in% c\\(', 'ANY(\\1,', s)}
killNA = function(s) {gsub(', "NA"', '', s)}

# not used as partykit:::.list.rules.party does not work with factors :-(
# fixed in partykit update
pathrules <- function(tree, ...)
    # display table listing terminal rules
{
    ## coerce to "party" object if necessary
    if(!inherits(tree, "party")) tree <- as.party(tree)
    
    ## get standard predictions (response/prob) and collect in data frame
    ###rval <- data.frame(response = predict(object, type = "response", ...))
    ###rval$prob <- predict(object, type = "prob", ...)
    
    ## get rules for each node
    rls <- partykit:::.list.rules.party(tree)
    rls = lapply(rls, killNA)
    trules = data.frame(unlist(rls))  # need unlist or get just one row
    ###save(rls, tree, trules, file="c:/temp/rls.rdata")
    tt = data.frame()
    for (i in 1:nrow(trules)) {
        tt[i, 1] = simppath(trules[i, 1])}  # try to simplify
    tt = data.frame(lapply(tt, f))  # convert to SPSS syntax
    colnames(tt)[[1]] = gtxt("Node Path")
    return(tt)
}


simppath = function(path) {
    # simplify and return the path
    
    # path is a string of path-defining segments each as a string of varname, op, criterion
    # with " & " as the separator
    
    pathcopy = path
    path = strsplit(path, " & ")
    path = unique(path[[1]])
    numsegs = length(path)
    newpath = list()
    
    # for each segment, if it is already in the newlist, discard
    # if it is more restrictive than an existing newlist item, replace
    # otherwise, append
    if (numsegs == 0) {
        return(pathcopy)
    }
    for (p in 1:numsegs) {
        seg = path[[p]]   # varname, operator, criteria
        seg1 = strsplit(seg, " ")[[1]]
        # in case there are blanks in the third element, which is a list of categories, put the
        # extra elements back
        if (length(seg1) > 3) {
            seg1[[3]] = paste(seg1[3:length(seg1)], collapse=" ")
            seg1 = seg1[1:3]
        }
        keep = TRUE
        replaceit = FALSE
        # see if exact or stronger version already included
        for (n in 1:length(newpath)) {
            if (length(newpath) == 0) {
                break
            }
            item = newpath[[n]]
            if (item == seg) {  #   seg might have been modified before adding
                keep = FALSE
                break
            }
            item1 = strsplit(item, " ")[[1]]
            if (item1[[1]] != seg1[[1]]) { # different variable
                next
            }
            # stronger condition of same type and scale variable?
            ###if ((seg1[[2]] %in% c("<", "<=") && seg1[[2]] == item1[[2]] && seg1[[3]] < item1[[3]]) ||
            ###    (seg1[[2]] %in% c(">", ">=") && seg1[[2]] == item1[[2]] && seg1[[3]] > item1[[3]])) {
            if (seg1[[2]] %in% c("<", "<=", ">", ">=") && seg1[[2]] == item1[[2]] && 
                 comp(seg1[[3]], item1[[3]], seg1[[2]])) {
                replaceit = TRUE
                break
            } else if (seg1[[2]] == "%in%") {
                cats1 = gsub("c\\((.+?))", "\\1", seg1[[3]])   # more restrictive?
                cats1 = strsplit(cats1, ",")
                cats2 = gsub("c\\((.+?))", "\\1", item1[[3]])
                cats2 = strsplit(cats2, ",")
                # could be fooled, but damage limited to not pruning
                if (length(setdiff(cats1, cats2)) == 0) {
                    keep = FALSE
                    break
                } else {
                    replaceit = TRUE
                    break
                }
            }
        }
        if (keep) {
            if (replaceit) {  # never TRUE if newpath is empty
                newpath[[n]] = seg
            } else {
                newpath[[length(newpath)+ 1]] = seg
            }
        }
    }
    
    newpath = unique(newpath)
    return(paste(newpath, collapse = " & "))
}


comp = function(x, y, op) {
    # compare x and y as numeric if possible else as strings
    # op is the comparison operator to use
    
    f = match.fun(op)
    suppressWarnings({
        xx = as.numeric(x)
        yy = as.numeric(y)
        if (is.na(xx) || is.na(yy)) {
            return(f(x,y))
        } else {
            return(f(as.numeric(x), as.numeric(y)))
        }
    }
    )
}


# Vignettes on CRAN
viglist = list(partykit="https://cran.r-project.org/web/packages/partykit/vignettes/partykit.pdf",
               ctree="https://cran.r-project.org/web/packages/partykit/vignettes/ctree.pdf",
               mob="https://cran.r-project.org/web/packages/partykit/vignettes/mob.pdf")

displayvignettes = function(vignettelist) {
    # display selected R vignettes
    
    if (!is.null(vignettelist)) {
        if ("dialoghelp" %in% vignettelist || "syntaxhelp" %in% vignettelist) {
            helploc = paste(getextloc(), "STATS_CITREE", sep="/")
        }
        for (v in vignettelist) {
            if (v == "dialoghelp") {
                browseURL(paste(helploc, "stats_cit.pdf", sep="/"))
            } else if (v == "syntaxhelp") {
                browseURL(paste(helploc, "markdown.html", sep="/"))
            } else {
                if (v %in% names(viglist)) {
                    #browseURL(viglist[[v]], "?target=\"_blank\"")
                    browseURL(paste(viglist[[v]], sep=""))
            } else {
                warns$warn(gtxtf("The specified vignette, %s, is not available", v))
            }
        }
        warns$warn("End of procedure", dostop=TRUE)
    }
    }
}

rmnan = function(ddf) {
    # remove NaN value rows from a data frame and return it
    
    # ddf is a data frame to process
 
       nanrows = c()
    for (r in 1:nrow(ddf)) {
        nanrows[r] = !any(sapply(ddf[r,], is.nan))
    }
    return(ddf[nanrows,])
}


cleannaf = function(dta, missinghandling, factorMode, warns) {
    # remove missing cases and empty factor levels
    # stop if any variable has more than 30 levels
    # return dta as per missinghandling and factorMode
    
    # the weight variable might be in the variable list, but it can't
    # be a factor, so it would have 0 levels
    
    if (missinghandling == "exclude") {
        # remove all rows with any variable missing (NA or NaN)
        dta = dta[complete.cases(dta),]
    } else {
        # remove cases with any scale variables missing
        dta = dta[complete.cases(Filter(Negate(is.factor), dta)),]
    }
    
    # in "labels" mode, GetDataFromSPSS creates empty factor levels
    # for unused value labels.  Get rid of those.
    if (factorMode == "labels") {
        dta = droplevels(dta)
    }
    cc = c()
    cn = colnames(dta)
    levelcounts = sapply(dta, nlevels)
    for (v in 1:length(cn)) {
        if (levelcounts[[v]] > 30) {
            cc = append(cc, cn[[v]])
        }
    }
    if (length(cc) > 0) {
        warns$warn(gtxtf("Categorical variables cannot have more than 30 categories: %s",
            paste(cc, collapse=" ")), dostop=TRUE)
    }
    return(dta)
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
                
        spsspkg.Template("TREES", subc="DISPLAY", ktype="str", var="treestoprint", islist=TRUE),
        spsspkg.Template("CONFUSION", subc="DISPLAY", ktype="bool", var="confusion", islist=FALSE), 
        spsspkg.Template("PLOTS", subc="DISPLAY", ktype="int", var="treestoplot", islist=TRUE),
        spsspkg.Template("MAINHEIGHT", subc="DISPLAY", ktype="float", var="mainheight", islist=FALSE),
        spsspkg.Template("MAINWIDTH", subc="DISPLAY", ktype="float", var="mainwidth", islist=FALSE),
        spsspkg.Template("SUBHEIGHT", subc="DISPLAY", ktype="float", var="subheight", islist=FALSE),
        spsspkg.Template("SUBWIDTH", subc="DISPLAY", ktype="float", var="subwidth", islist=FALSE),
        spsspkg.Template("FONTSIZE", subc="DISPLAY", ktype="float", var="fontsize", islist=FALSE),
        spsspkg.Template("PLOTFILE", subc="DISPLAY", ktype="literal", var="plotfile", islist=FALSE),
        spsspkg.Template("PLOTFORMAT", subc="DISPLAY", ktype="str", var="plotformat", 
                         vallist=list("png", "svg", "pdf"), islist=FALSE),
        spsspkg.Template("PLOTBG", subc="DISPLAY", ktype="str", var="plotbg", islist=FALSE),
        spsspkg.Template("SCTESTS", subc="DISPLAY", ktype="bool", var="sctests", islist=FALSE),
        spsspkg.Template("INNERPLOTS", subc="DISPLAY", ktype="bool", var="innerplots", islist=FALSE),
        spsspkg.Template("MATCHBG", subc="DISPLAY", ktype="bool", var="matchbg", islist=FALSE),
        spsspkg.Template("TERMSTATS", subc="DISPLAY", ktype="bool", var="termstats", islist=FALSE),
        spsspkg.Template("NODEPATHS", subc="DISPLAY", ktype="bool", var="nodepaths", islist=FALSE),
        spsspkg.Template("TERMINALPLOTS", subc="DISPLAY", ktype="bool", var="terminalplots", islist=FALSE),
        
        spsspkg.Template("MODELFILE", subc="SAVE", ktype="literal", var="modelfile", islist=FALSE),
        spsspkg.Template("DATASET", subc="SAVE", ktype="varname", var="datasetname"), 
        spsspkg.Template("PREDTYPE", subc="SAVE", ktype="str", var="predtype",
            vallist=list("response", "prob", "quantile", "node"), islist=TRUE),
        spsspkg.Template("QUANTILES", subc="SAVE", ktype="float", var="quantiles", islist=TRUE),
        spsspkg.Template("IGNORETHIS", subc="SAVE", ktype="bool", var="ignorethis"),
        
        spsspkg.Template("MISSINGVALUES", subc="OPTIONS", ktype="str", var="missingvalues",
              vallist=list("exclude", "include"), islist=FALSE),
        spsspkg.Template("TESTSTAT", subc="OPTIONS", ktype="str", var="teststat",
            vallist=list("quadratic", "maximum")),
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
       spsspkg.Template("MULTIWAY", subc="OPTIONS", ktype="bool", 
            var="multiway", islist=FALSE),
       spsspkg.Template("CORES", subc="OPTIONS", ktype="int", var="cores"),
       
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
