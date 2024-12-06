# anonymous extension command to insert a list of charts into the Viewer

# history
# 13-Nov-2024  Original vesion

# Author: Jon K. Peck

# No help file is provided for this extension

# USAGE:
# STATS INSERT CHART = filespec POSITION=where to insert HEADER="header text" OUTLINELABEL="text for outline"
# HIDELOG = YES or NO.
 
import spss, SpssClient
from extension import Template, Syntax, processcmd

def doinsertcharts(chart=None, header=None, position=None, outlinelabel=None, hidelog=False):
    """Insert one or more charts into the Viewer
    
    chartlist is the filespec of the chart to insert
    header is the outline title for the charts
    position is the order.  If position is zero, new header is constructed
    outline label is the label for the item
    hidelog specifies that the most recent log block in the Viewer should be closed
    """
    # debugging
            # makes debug apply only to the current thread
    #try:
        #import wingdbstub
        #import threading
        #wingdbstub.Ensure()
        #wingdbstub.debugger.SetDebugThreads({threading.get_ident(): 1})
    #except:
        #pass

    SpssClient.StartClient()
    if chart is not None and (header is None or position is None or outlinelabel is None):
        raise ValueError("Missing keyword value")
    doc = SpssClient.GetDesignatedOutputDoc()
    itemlist = doc.GetOutputItems()
    # Get the root header item
    root = itemlist.GetItemAt(0).GetSpecificType()
    if chart:
        # Create a new header item
        if position == 0:
            theHeader = doc.CreateHeaderItem(header)
            # Append the new header to the root item
            root.InsertChildItem(theHeader,root.GetChildCount())
        # Get the new or in-progress header item
        headerItem = root.GetChildItem(root.GetChildCount()-1)
        if not headerItem.GetType() ==  SpssClient.OutputItemType.HEAD:
            root.RemoveChildItem(root.GetChildCount()-1)
            headerItem =  root.GetChildItem(root.GetChildCount()-1)
        headerItem = headerItem.GetSpecificType()
        ###headerItem = root.GetChildItem(root.GetChildCount()-1).GetSpecificType()
        # Create a new text item
        outitem = doc.CreateImageChartItem(chart,f"{outlinelabel}")
        # Append the new item to the header item
        headerItem.InsertChildItem(outitem, position)
    if hidelog:
        hidethelog(itemlist)
    SpssClient.StopClient()
    
def hidethelog(itemlist):
    """Hide the most recent log block in the Viewer (if any)

    itemlist is the list of items currently in the Viewer"""
    
    itemkt = itemlist.Size()
    for i in range(itemkt-1, 0, -1):
                    item = itemlist.GetItemAt(i)
                    if item.GetType() == SpssClient.OutputItemType.LOG:
                        item.SetVisible(False)
                        break
    
def  Run(args):
    """Execute the STATS INSERT CHARTS command"""

    args = args[list(args.keys())[0]]

    oobj = Syntax([
        Template("CHART", subc="",  ktype="str", var="chart", islist=False),
        Template("POSITION", subc="",  ktype="int", var="position", islist=False),
        Template("HEADER", subc="", ktype="literal", var="header", islist=False), 
        Template("OUTLINELABEL", subc="", ktype="literal", var="outlinelabel", islist=False),
        Template("HIDELOG", subc="", ktype="bool", var="hidelog", islist=False)
    ])
        
    #enable localization
    global _
    try:
        _("---")
    except:
        def _(msg):
            return msg

    # A HELP subcommand overrides all else
    if "HELP" in args:
        #print helptext
        helper()
    else:
        processcmd(oobj, args, doinsertcharts)

def helper():
    """open html help in default browser window
    
    The location is computed from the current module name"""
    
    import webbrowser, os.path
    
    path = os.path.splitext(__file__)[0]
    helpspec = "file://" + path + os.path.sep + \
         "markdown.html"
    
    # webbrowser.open seems not to work well
    browser = webbrowser.get()
    if not browser.open_new(helpspec):
        print(("Help file not found:" + helpspec))
try:    #override
    from extension import helper
except:
    pass        
