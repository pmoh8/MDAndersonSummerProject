library(shiny)
library(DT)
library(ggplot2)
library(RSQLite)
source("common2.R")
################################################################
######################## USER INTERFACE ########################
################################################################
ui <- 
  fluidPage( 
    tags$head(tags$style(HTML("
                              .selectize-input, .selectize-dropdown {
                              font-size: 70%;
                              }
                              "))),
    #Set up tabs
    div(style="display: inline-block;vertical-align:center;",titlePanel("Cool Application")),
    div(style="display: inline-block;vertical-align:center;float:right;",actionButton("loginButt", "Login")),
    div(style="display: inline-block;vertical-align:center;float:right; width: 150px;",passwordInput("inputPass", NULL, value = "", width = NULL, placeholder = "password")),
    div(style="display: inline-block;vertical-align:center;float:right; width: 150px;",textInput("inputId", NULL, value = "", width = NULL, placeholder = "username")),
    sidebarLayout("",
                  mainPanel(width = 12,
                            tabsetPanel(
                              ###########################
                              ####### SCATTERPLOT #######
                              ###########################
                              tabPanel("Scatterplot", 
                                       br(),
                                       fluidRow(
                                         ##############################################
                                         ######## USER CONTROL PANEL (SIDEBAR) ########
                                         ##############################################
                                         column(2,
                                                wellPanel(
                                                  #labels for data selection options
                                                  fluidRow(   column(2),   column(5, "x-axis"),   column(5, "y-axis")   ),
                                                  #row to select data types to view (such as RNAseq or DNA copy number)
                                                  fluidRow(
                                                    column(2, "Data"),
                                                    #input to select which data type is displayed on the x-axis
                                                    column(5, uiOutput("x_datatype_selector")
                                                    ),
                                                    #input to select which data type is displayed on the y-axis
                                                    column(5, uiOutput("y_datatype_selector")
                                                    )
                                                  ),
                                                  #row to select which gene to view
                                                  fluidRow(
                                                    column(2,"Gene"),
                                                    #input to select which gene is displayed on the x-axis
                                                    column(5, uiOutput("gene_x_selector")),
                                                    #input to select which gene is displayed on the y-axis
                                                    column(5, uiOutput("gene_y_selector"))
                                                  )
                                                ),
                                                br(),
                                                "Cancer type(s) to highlight",
                                                uiOutput("cancer_selector"),
                                                checkboxInput("showSelectedOnly", "Show selected cancers only", FALSE),
                                                textOutput("test")
                                         ),
                                         ##################################################
                                         ######## INFORMATION DISPLAY (MAIN PANEL) ########
                                         ##################################################
                                         column(8,
                                                ##############################################################
                                                ######## POINT DETAIL DISPLAY (Cell line, Cancer Type) #######
                                                ##############################################################
                                                fluidRow(
                                                  #display details of the left plot ("zoomed-out" plot)
                                                  column(6, 
                                                         #display details
                                                         fluidRow(column(1),
                                                                  column(10, wellPanel(uiOutput("cell_details")))
                                                         )
                                                  ),
                                                  #display details of the right plot ("zoomed-in" plot)
                                                  column(6,
                                                         #only display IF the user has selected a potion of the left plot
                                                         conditionalPanel(condition = 'output.brushActive',
                                                                          fluidRow(column(1),
                                                                                   column(10, wellPanel(uiOutput("cell_zoom_details")))
                                                                          )
                                                         )
                                                  )
                                                ),
                                                #############################################
                                                ######## POINT DETAIL DISPLAY (Other) #######
                                                #############################################                            
                                                fluidRow(
                                                  column(6,
                                                         fluidRow(column(1),
                                                                  column(10, wellPanel(tableOutput("click_info")))
                                                         )
                                                  ),
                                                  column(6,
                                                         conditionalPanel(condition = 'output.brushActive',
                                                                          fluidRow(column(1),
                                                                                   column(10, wellPanel(tableOutput("zoom_click_info")))
                                                                          )
                                                         )
                                                  )
                                                ),
                                                #####################################
                                                ######## SCATTERPLOT DISPLAY ########
                                                #####################################
                                                fluidRow(
                                                  #Display left ("zoomed-out") scatterplot
                                                  column(6,
                                                         wellPanel(
                                                           plotOutput("plot",
                                                                      click = "plot_click",
                                                                      brush = brushOpts(id="plot_brush")
                                                                      )
                                                           ),
                                                           conditionalPanel(condition = '!output.showTable',
                                                                            actionButton("show_table","Show Data Table")
                                                                            ),
                                                           conditionalPanel(condition = 'output.showTable',
                                                                            actionButton("hide_table","Hide Data Table"),
                                                                            br(),
                                                                            conditionalPanel(condition = 'output.tableHasSelected',
                                                                                             actionButton("clear_table_selection","Clear Selection")
                                                                            ),
                                                                            br(),
                                                                            div(DT::dataTableOutput("data_table"), style = "font-size:80%")
                                                                            )
                                                  ),
                                                  #Display right ("zoomed-in") scatterplot
                                                  column(6,
                                                         #only display IF the user has selected a potion of the left plot
                                                         conditionalPanel(condition = 'output.brushActive',
                                                                          wellPanel(
                                                                            plotOutput("plot_zoom",
                                                                                       click = "plot_zoom_click")),
                                                                          #Allow user to toggle whether ot not they want to see a table with the plotted data
                                                                          conditionalPanel(condition ='!output.showZoomTable',
                                                                                           actionButton("show_zoom_table","Show Data Table")
                                                                                           ),
                                                                          conditionalPanel(condition = 'output.showZoomTable',
                                                                                           actionButton("hide_zoom_table","Hide Data Table"),
                                                                                           br(),
                                                                                           conditionalPanel(condition = 'output.zoomTableHasSelected',
                                                                                             actionButton("clear_zoom_table_selection","Clear Selection")
                                                                                           ),
                                                                                          
                                                                                           br(),
                                                                                           div(DT::dataTableOutput("zoom_table"), style = "font-size:80%")
                                                                                           )
                                                         )
                                                  )
                                                )
                                         ),
                                         ##############################################
                                         ######## USER CONTROL PANEL (SIDEBAR) ########
                                         ##############################################
                                         column(2,
                                                conditionalPanel(condition = 'output.brushActive',
                                                                 wellPanel(
                                                                   #labels for data selection options
                                                                   fluidRow(   column(2),   column(5, "x-axis"),   column(5, "y-axis")   ),
                                                                   #row to select data types to view (such as RNAseq or DNA copy number)
                                                                   fluidRow(
                                                                     column(2, "Data"),
                                                                     #input to select which data type is displayed on the x-axis
                                                                     column(5,
                                                                            selectInput("datatype_zoom_x", NULL,c("DNA Copy Number"), selectize=FALSE)
                                                                     ),
                                                                     #input to select which data type is displayed on the y-axis
                                                                     column(5,
                                                                            selectInput("datatype_zoom_y", NULL, c("DNA Copy Number"),selectize=FALSE)
                                                                     )
                                                                   ),
                                                                   #row to select which gene to view
                                                                   fluidRow(
                                                                     column(2,"Gene"),
                                                                     #input to select which gene is displayed on the x-axis
                                                                     column(5, uiOutput("gene_zoom_x_selector")),
                                                                     #input to select which gene is displayed on the y-axis
                                                                     column(5, uiOutput("gene_zoom_y_selector"))
                                                                   )
                                                                 )
                                                )
                                         )
                                       )
                              ),
                              #######################
                              ####### HEATMAP #######
                              #######################   
                              tabPanel("Heatmap", verbatimTextOutput("summary"))
                            )
                  )
    )
    )
########################################################
######################## SERVER ########################
########################################################
server <- function(input, output, session) {
  
  ###############################
  ######## GENERAL SETUP ########
  ############################### 
  #Set up some aesthetics

  cbPalette <- c("#009E73", "#0072B2", "#CC79A7","#56B4E9","#F0E442")
  
  ptBig = 5
  ptMed = 3
  ptSmol = 2
  transp = 0.3
  transpMed = 0.7
  colDefault = "#999999"
  colRed = "#D55E00"
  colOrn = "#E69F00"

  #intialize reactive values for points the user can choose to highlight by clicking on each plot
  points <- reactiveValues(pnt = NULL, pnt_zoom=NULL)
  plotdata <- reactiveValues(zoomdata=(NULL), highlightDF=(NULL), highlightzoomDF=(NULL))
  plottingdata <- NULL
  ##################################
  #for the left ("zoomed"-out") plot
  ##################################
  #Read in data for the x and y coordinates of the plot
  database_dataframe <- readRDS("database_table")
  datatype_options <- reactive({
    return(database_dataframe[,1])
  })
  inputdata_x <- reactive({
    return(readRDS(toString(database_dataframe[database_dataframe[,"Label"]==toString(x_datatype_sel()),2])))
  })
  inputdata_y <- reactive({
    return(readRDS(toString(database_dataframe[database_dataframe[,"Label"]==toString(y_datatype_sel()),2])))
  })
  
  x_datatype_sel <- reactive ({
    clearPoints()
    return(input$x_datatype)
  })
  y_datatype_sel <- reactive ({
    clearPoints()
    return(input$y_datatype)
  })
  #Have the user select which genes to display on the x and y axis respectively
  x_sel <- reactive({
    #when the user changes what gene to display, clear the highlighted points the user selected by clicking on the plot
    clearPoints()
    return(input$gene_x)
  })
  y_sel <- reactive({
    #when the user changes what gene to display, clear the highlighted points the user selected by clicking on the plot
    clearPoints()
    return(input$gene_y)
  }) 
  
  #Have the user select which datatypes to go with the genes to display on the x and y axis respectively
  x_datatype <- reactive({return(input$datatype_x)})
  y_datatype <- reactive({return(input$datatype_y)})
  
  #################################
  #For the right ("zoomed-in") plot
  #################################
  #Have the user select which genes to display on the x and y axis respectively
  x_zoom_sel <- reactive({
    #when the user changes what gene to display, clear the highlighted points the user selected by clicking on the plot
    return(input$gene_zoom_x)
  })
  y_zoom_sel <- reactive({
    #when the user changes what gene to display, clear the highlighted points the user selected by clicking on the plot
  
    return(input$gene_zoom_y)
  })
  #Have the user select which datatypes to go with the genes to display on the x and y axis respectively
  x_zoom_datatype <- reactive({return(input$datatype_zoom_x)})
  y_zoom_datatype <- reactive({return(input$datatype_zoom_y)})

  ##################
  #Read in RDS files
  ##################
  #Read in the selection options for x_sel, y_sel, x_zoom_sel, and y_zoom_sel
  DNAcopy_options <- reactive({
    file <- readRDS("DNAcopy_options_CCLE_copynumber_byGene_2013-12-03")
    return(file)
  })
  
  #Read in the options for the user to select and highlight specific cancer type(s)

  cancer_options <- readRDS("cancerTypes_CCLE_copynumber_byGene_2013-12-03")
  
  #Holds the list of cancer types the user has selected to highlight
  cancer_highlights <- reactive({
    cat(file=stderr(), "cancer_highlights reactive", "\n")
    return(input$cancer_selection)
  })
  
  ###################################
  ######## CHECK EVENT FLAGS ########
  ###################################
  #values for event flags
  flags <- reactiveValues(brushActive = FALSE,  showTable = FALSE, tableHasSelected = FALSE, showZoomTable = FALSE, zoomTableHasSelected = FALSE, defaultZoomView = TRUE)
  output$brushActive <- reactive({ return(flags$brushActive) })
  output$showTable <- reactive({ return(flags$showTable)} )
  output$tableHasSelected <- reactive({ return(flags$tableHasSelected) })
  output$showZoomTable <- reactive({ return(flags$showZoomTable)})
  output$zoomTableHasSelected <- reactive({ return(flags$zoomTableHasSelected) })
  outputOptions(output, name = "brushActive", suspendWhenHidden = FALSE)
  outputOptions(output, name = "showTable", suspendWhenHidden = FALSE)
  outputOptions(output, name = "tableHasSelected", suspendWhenHidden = FALSE)
  outputOptions(output, name = "showZoomTable", suspendWhenHidden = FALSE)
  outputOptions(output, name = "zoomTableHasSelected", suspendWhenHidden = FALSE)
  
  #Check to see if the brush is active
  observeEvent(plotdata$zoomdata, {
    cat(file=stderr(), "ObserveEvent:  plotdata$zoomdata", "\n")
    if(is.data.frame(plotdata$zoomdata) && nrow(plotdata$zoomdata)==0) {
      flags$brushActive <- FALSE
      flags$defaultZoomView <- TRUE
      print("brush is not active")
    }
    else{
      if(!flags$brushActive){
        flags$defaultZoomView <- FALSE
        #Reset the selectinput options on the right/"zoomed-in" control panel
        updateSelectInput(session, "gene_zoom_x",selected=x_sel())
        updateSelectInput(session, "gene_zoom_y",selected=y_sel())
        print("zoom plot has been reset to default")
      }
      flags$brushActive <- TRUE
      print("brush is active!")
    }
  })
  
  #If the user has selected data points with the brush, save them to the reactiveValue "zoomdata"
  observeEvent(input$plot_brush,{plotdata$zoomdata <- brushedPoints(defaultplotdata,input$plot_brush)})
  
  observeEvent(input$showSelectedOnly,{
    if (input$showSelectedOnly){
      makeTable(plotdata$highlightDF,x_sel(),y_sel())
      makeZoomTable(plotdata$highlightzoomDF,x_zoom_sel(),y_zoom_sel())
    }
    else{
      makeTable(defaultplotdata,x_sel(),y_sel())
      makeZoomTable(plottingdata,x_zoom_sel(),y_zoom_sel())
    }
  })
  observeEvent(input$cancer_selection,{
    if (input$showSelectedOnly){
      makeTable(plotdata$highlightDF,x_sel(),y_sel())
      makeZoomTable(plotdata$highlightzoomDF,x_zoom_sel(),y_zoom_sel())
    }
    else{
      makeTable(defaultplotdata,x_sel(),y_sel())
      makeZoomTable(plottingdata,x_zoom_sel(),y_zoom_sel())
    }
  })
  #Observe whether the user has decided to toggle the data table on the left/"zoomed-out" plot for viewing
  observeEvent(input$show_table,{
    cat(file=stderr(), "observeEvent:  input$show_table", "\n")
    flags$showTable <- TRUE
    makeTable(defaultplotdata,x_sel(),y_sel())
  })
  
  observeEvent(input$hide_table,{
    cat(file=stderr(), "observeEvent:  input$hide_table", "\n")
    flags$showTable <- FALSE
  })
  
  observeEvent(input$data_table_rows_selected, {
    cat(file=stderr(), "observeEvent:  input$data_table_rows_selected", "\n")
    if(length(input$data_table_rows_selected)){
      flags$tableHasSelected <- TRUE
    }
    else{
      flags$tableHasSelected <- FALSE
    }
  })
  
  #Observe whether the user has decided to toggle the data table on the right/"zoomed-in" plot for viewing
  observeEvent(input$show_zoom_table,{
    cat(file=stderr(), "observeEvent:  input$show_zoom_table", "\n")
    flags$showZoomTable <- TRUE
    makeZoomTable(plottingdata,x_zoom_sel(),y_zoom_sel())
  })
  
  observeEvent(input$hide_zoom_table,{
    cat(file=stderr(), "observeEvent:  input$hide_zoom_table", "\n")
    flags$showZoomTable <- FALSE
  })
  
  observeEvent(input$zoom_table_rows_selected, {
    cat(file=stderr(), "observeEvent:  input$zoom_table_rows_selected", "\n")
    if(length(input$zoom_table_rows_selected)){
      flags$zoomTableHasSelected <- TRUE
    }
    else{
      flags$zoomTableHasSelected <- FALSE
    }
  })
  
  #If appropriate, make the table to display the data in the left/"zoomed-out" plot
  makeTable <- function(data,x_axis,y_axis){
    
    if(flags$showTable){
      output$data_table <- DT::renderDataTable({
        cat(file=stderr(), "output$data_table", "\n")
        displayTable <- datatable(data[,1:2],colnames = c(x_axis,y_axis))
        return(displayTable)
      })
    }

  } 
  
  defaultProxy = dataTableProxy('data_table')
  observeEvent(input$clear_table_selection, {
    cat(file=stderr(), "observeEvent:  input$clear_table_selection", "\n")
    print("clearing selection")
    defaultProxy %>% selectRows(NULL)
    print(input$table_rows_selected)
    flags$tableHasSelected <- FALSE
  })
  
  #If appropriate, make the table to display the data in the right/"zoomed-in" plot
  makeZoomTable <- function(data,x_axis,y_axis){
    
    if(flags$showZoomTable){
      output$zoom_table <- DT::renderDataTable({
        cat(file=stderr(), "output$zoom_table", "\n")
        displayTable <- datatable(data[,1:2],colnames = c(x_axis,y_axis))
        return(displayTable)
      })
    }
    
  }
  
  zoomProxy = dataTableProxy('zoom_table')
  observeEvent(input$clear_zoom_table_selection, {
    cat(file=stderr(), "observeEvent:  input$clear_zoom_table_selection", "\n")
    print("clearing selection")
    zoomProxy %>% selectRows(NULL)
    print(input$zoom_table_rows_selected)
    flags$zoomTableHasSelected <- FALSE
  })
  
  
  #######################################
  ####### RENDERING USER OPTIONS ########
  #######################################
  #Test render
  output$test <- renderText({
  })
  
  #Actual rendering
  output$cancer_selector <- renderUI({
    cat(file=stderr(), "output$cancer_selector", "\n")
    selectizeInput("cancer_selection", NULL, cancer_options, selected=NULL, multiple=TRUE,options = list(maxItems = 5))
  })
  
  output$x_datatype_selector <- renderUI({
    cat(file=stderr(), "output$x_datatype_selector", "\n")
    selectInput("x_datatype", NULL, datatype_options(), selectize=FALSE)
  })
  
  output$y_datatype_selector <- renderUI({
    cat(file=stderr(), "output$y_datatype_selector", "\n")
    selectInput("y_datatype", NULL, datatype_options(), selectize=FALSE)
  })
  
  output$gene_x_selector <- renderUI({
    cat(file=stderr(), "output$gene_x_selector", "\n")
    selectInput("gene_x", NULL, DNAcopy_options(), selectize=FALSE)
  })
  
  output$gene_y_selector <- renderUI({
    cat(file=stderr(), "output$gene_y_selector", "\n")
    selectInput("gene_y", NULL, DNAcopy_options(), selectize=FALSE)
  })
  
  output$gene_zoom_x_selector <- renderUI({
    cat(file=stderr(), "output$gene_zoom_x_selector", "\n")
    y_sel()
    return(selectInput("gene_zoom_x", NULL, DNAcopy_options(),selected=x_sel(), selectize=FALSE))
  })

  output$gene_zoom_y_selector <- renderUI({
    cat(file=stderr(), "output$gene_zoom_y_selector", "\n")
    x_sel()
    return(selectInput("gene_zoom_y", NULL, DNAcopy_options(),selected=y_sel(), selectize=FALSE))
  })
  

  
  ##################################
  ####### RENDERING OUTPUTS ########
  ##################################

  #output the left/"zoomed-out" plot
  output$plot<- renderPlot({ 
    cat(file=stderr(), "output$plot", "\n")
    #Select the appropriate rows from our database table based on user input
      #Get the x and y values of interest and save them to a data frame
       x <- sapply(inputdata_x()[inputdata_x()[,"SYMBOL"]==x_sel(),-1], as.numeric)
       y <- sapply(inputdata_y()[inputdata_y()[,"SYMBOL"]==y_sel(),-1], as.numeric)
      
      #make the dataframe to hold the information we're displaying in the scatterplot
      defaultplotdata <<- data.frame(x = x, y = y)
      
      #make columns for cancer type and cell line name
      underscore <- regexpr("_",rownames(defaultplotdata))
      line <- substring(rownames(defaultplotdata), 1, underscore-1)
      cancer <- substring(rownames(defaultplotdata), underscore+1)
      defaultplotdata <<- cbind(defaultplotdata,cancer)
      defaultplotdata <<- cbind(defaultplotdata,line)
      
      #if the user has the "Show selected cancers only" box checked off, only show the cancers of interest
      defaultplot <<- ggplot() + labs(x = paste(x_sel()," - ",x_datatype()), y = paste(y_sel()," - ",y_datatype()), title = "Test Scatter")
      
      #if the user does not have that option checked, add in the rest of the points, too
      if(!input$showSelectedOnly){
        defaultplot <<- defaultplot + geom_point(data = defaultplotdata, aes(x,y), alpha=transp, color=colDefault)
      }
      
      #make a dataframe to hold the cancer cell lines the user wants to highlight (based on cancer type)
      plotdata$highlightDF <- defaultplotdata[defaultplotdata$cancer %in% cancer_highlights(),]
      defaultplot <<- defaultplot + geom_point(data = plotdata$highlightDF, aes(x,y, color=cancer), alpha = transpMed) + scale_colour_manual(values=cbPalette)
      
      #Add the tiny point representing the point selected in the other ("zoomed-in") plot
      if(!is.null(points$pnt_zoom) && (flags$brushActive != 0)){
        matched_pnt_zoom <- updatePoint(points$pnt_zoom,defaultplotdata)
        defaultplot <<- plotPointOnClick(defaultplot, input$plot_zoom_click, matched_pnt_zoom, colOrn, ptMed, updatePoint = FALSE)
      }
      
      #Add in user-selected highlights from the printed data table
      tableHighlights = input$data_table_rows_selected
      #print("tableHighlights")
      #print(tableHighlights)
      if(length(tableHighlights)){
        defaultplot <<- defaultplot + geom_point(data = defaultplotdata[tableHighlights,], aes(x,y), color=colRed, size = ptSmol)
      }
      
      #Add user-clicked highlighted points
      defaultplot <<- plotPointOnClick (defaultplot, input$plot_click, points$pnt, colRed, ptBig, updatePoint = TRUE)
      
      #update the stored point of interest if needed
      if(!(is.null(input$plot_click) && is.null(points$pnt)) && !is.null(input$plot_click)){
        if(input$showSelectedOnly){
          points$pnt <<- nearPoints(plotdata$highlightDF, input$plot_click, maxpoints = 1) 
        }
        else{
          points$pnt <<- nearPoints(defaultplotdata, input$plot_click, maxpoints = 1)
        }
      }
      
      #print the plot
      defaultplot
    
  }) 
  
  #output details (cell line, cancer type) of the point on the left/"zoomed-out" plot
  output$cell_details <- renderUI({
    cat(file=stderr(), "output$cell_details", "\n")
    if(!is.null(input$plot_click)){}
    printInfo(points$pnt)
  })

  #output data (dna copy number, rnaexpression, etc) associated with the selected point on the left/"zoomed-out" plot 
  output$click_info <- renderTable({
    cat(file=stderr(), "output$click_info", "\n")
    if(!is.null(input$plot_click)){}
    printPoint(points$pnt,x_sel(),y_sel())
  },bordered = TRUE, spacing = 'xs', rownames = TRUE,colnames = TRUE)

  #output the right/"zoomed-in" plot
  output$plot_zoom<- renderPlot({
    cat(file=stderr(), "output$plot_zoom", "\n")
    if(flags$brushActive){
      #build the plot
      #Get selected points from the left/"zoomed-out" plot and save them
      plotdata$zoomdata <- brushedPoints(defaultplotdata,input$plot_brush)
      oldplottingdata <<- plottingdata
      plottingdata<<-plotdata$zoomdata
      
      #Perform manipulations on the selected data points from the previous block of code based on user input
      
      if(!flags$defaultZoomView){
        #Get the x and y values of interest and save them to a data frame
        x <- sapply(inputdata_x()[inputdata_x()[,"SYMBOL"]==x_zoom_sel(),-1], as.numeric)
        y <- sapply(inputdata_y()[inputdata_y()[,"SYMBOL"]==y_zoom_sel(),-1], as.numeric)
        #make the dataframe to hold the information we're displaying in the scatterplot
        cancersOfInterest <- row.names(plotdata$zoomdata)
        
        newDF <<- data.frame(x = x, y = y)[cancersOfInterest,]
        #make columns for cancer type and cell line name
        underscore <- regexpr("_",rownames(plotdata$zoomdata))
        line <- substring(rownames(plotdata$zoomdata), 1, underscore-1)
        cancer <- substring(rownames(plotdata$zoomdata), underscore+1)
        newDF <- cbind(newDF, cancer)
        newDF <- cbind(newDF,line)
        plottingdata<<-newDF
      
        #Determine if the new plotting dataset and the old one are the same or not.  If they are different, re-rendeer the displayed data table
        if(!isTRUE(all.equal(plottingdata,oldplottingdata))){
          makeZoomTable(plottingdata,x_zoom_sel(),y_zoom_sel())
        }
      }
      
      #if the user has the "Show selected cancers only" box checked off, only show the cancers of interest
      zoomplot<<-ggplot() + labs(x = paste(x_zoom_sel()," - ", x_datatype()), y = paste(y_zoom_sel()," - ",y_datatype()), title = "Test Scatter") 
      
      #if the user does not have that option checked, add in the rest of the points, too
      if(!input$showSelectedOnly){
        zoomplot <<- zoomplot + geom_point(data = plottingdata, aes(x,y), alpha=transp, color=colDefault)
      }
      #Add in highlighted cancer types
      plotdata$highlightzoomDF <- plottingdata[plottingdata$cancer %in% cancer_highlights(),]
      zoomplot <<- zoomplot + geom_point(data = plotdata$highlightzoomDF, aes(x,y, color=cancer), alpha = transpMed) + scale_colour_manual(values=cbPalette)      
      
      #Add in user-selected highlights from the printed data table
      tableHighlights = input$zoom_table_rows_selected
      #print("zoomtablehighlights")
      #print(tableHighlights)
      if(length(tableHighlights)){
        zoomplot <<- zoomplot + geom_point(data = plottingdata[tableHighlights,], aes(x,y), color=colOrn, size = ptSmol)
      }
      
      #make sure the same point is saved between axis changes
      if(!is.null(points$pnt_zoom)){
        points$pnt_zoom <- isolate(updatePoint(points$pnt_zoom,plottingdata))
      }
      
      #plot the user-click highlighted points
      matched_pnt <- updatePoint(points$pnt,plottingdata)
      zoomplot <- plotPointOnClick(zoomplot, input$plot_click, matched_pnt, colRed, ptMed, updatePoint = FALSE)
      zoomplot <- plotPointOnClick(zoomplot, input$plot_zoom_click, points$pnt_zoom, colOrn, ptBig, updatePoint = TRUE)

      #update the stored point of interest if needed
      if(!(is.null(input$plot_zoom_click) && is.null(points$pnt_zoom) ) && !is.null(input$plot_zoom_click)){
        if(input$showSelectedOnly){
          points$pnt_zoom <<- nearPoints(plotdata$highlightzoomDF, input$plot_zoom_click, maxpoints = 1) 
        }
        else{
          points$pnt_zoom <<- nearPoints(plottingdata, input$plot_zoom_click, maxpoints = 1)
        }
      }
      
      #actually output the plot
      zoomplot
    }
  }) 
  
  #print details (cell line, cancer type) of the selected point on the right/"zoomed-in" plot

  output$cell_zoom_details <- renderUI({
    cat(file=stderr(), "output$cell_zoom_details", "\n")
    if(!is.null(input$plot_zoom_click)){}
    printInfo(points$pnt_zoom)
  })    
  
  #output data (dna copy number, etc) associated with the selected point on the right/"zoomed-in" plot
  output$zoom_click_info <- renderTable({
    cat(file=stderr(), "output$zoom_click_info", "\n")
    if(!is.null(input$plot_zoom_click)){}
    printPoint(points$pnt_zoom,x_zoom_sel(),y_zoom_sel())
  },bordered = TRUE, spacing = 'xs', rownames = TRUE,colnames = TRUE)
  
  #################################
  ######## VARIOUS FUNTIONS #######
  #################################
  
  #Add a highlighted point to the passed in plot
  plotPointOnClick <- function(plot,inputClick,point,ptColor, ptSize, updatePoint){
    cat(file=stderr(), "plotPointOnClick", "\n")
    plotPoint <- point
    if(is.null(inputClick) && is.null(plotPoint)){
      plot
    }
    else{ 
      if(!is.null(inputClick) && (updatePoint == TRUE)){
        plotPoint <- nearPoints(defaultplotdata, inputClick, maxpoints = 1)
      }
      plot<-plot + geom_point(data = plotPoint, aes(x,y), color = ptColor, size = ptSize, shape = 17)
    }
    return (plot)
  }
  
  #Print the cancer line and cancer type associated with the passed in point
  printInfo <- function(point){
   
    print("pointprintnfo")
    str1 <- paste("Cell Line:  ", point$line)
    str2 <-paste("Cancer Type:  ", point$cancer)

    HTML(paste(str1,str2,sep='<br/>'))
  }
  
  #Print data associated with the point (such as DNA copy number, mRNA, etc)
  printPoint <- function(point,x_axis,y_axis){
    cat(file=stderr(), "printPoint", "\n")

    #Make sure the point is valid before trying to do anything
    if(is.data.frame(point) && nrow(point)!=0) {
      #See if the x and y-axis are the same so that we don't mess up and have duplicate row names and make R throw a fit
      if(x_axis != y_axis){
        pointDetail <- data.frame(c(point$x, point$y))
        row.names(pointDetail)<-c(x_axis,y_axis)
      }
      else{
        pointDetail <- data.frame(c(point$x))
        row.names(pointDetail)<-c(x_axis) 
      }
      
      #add column names
      names(pointDetail)<-c("DNA Copy #")
      cat(file=stderr(), "printPoint done", "\n")
      return (pointDetail)
    }
    cat(file=stderr(), "printPoint done?", "\n")

  }
  
  #Reset the values of the stored points to NULL
  clearPoints <- function(){
    cat(file=stderr(), "clearPoints", "\n")
    points$pnt <- NULL
    points$pnt_zoom <- NULL
  }
  
  #take a "point" from a plot and return the correpsonding point in "data"
  updatePoint <- function(point, data){
    cat(file=stderr(), "updatePoint", "\n")
    return(data[row.names(point),])
  }
  
}

shinyApp(ui = ui, server = server)

