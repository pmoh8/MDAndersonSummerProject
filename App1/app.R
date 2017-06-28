
library(shiny)
library(DT)
library(ggplot2)
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
    titlePanel("Cool Application"),
    
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
                                                    column(5,
                                                           selectInput("datatype_x", NULL,c("DNA Copy Number"))
                                                    ),
                                                    #input to select which data type is displayed on the y-axis
                                                    column(5,
                                                           selectInput("datatype_y", NULL, c("DNA Copy Number"))
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
                                                         conditionalPanel(condition = 'output.brushActive != 0',
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
                                                         conditionalPanel(condition = 'output.brushActive != 0',
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
                                                                            br(),
                                                                            div(DT::dataTableOutput("data_table"), style = "font-size:80%")
                                                                            )
                                                  ),
                                                  #Display right ("zoomed-in") scatterplot
                                                  column(6,
                                                         #only display IF the user has selected a potion of the left plot
                                                         conditionalPanel(condition = 'output.brushActive != 0',
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
                                                conditionalPanel(condition = 'output.brushActive != 0',
                                                                 wellPanel(
                                                                   #labels for data selection options
                                                                   fluidRow(   column(2),   column(5, "x-axis"),   column(5, "y-axis")   ),
                                                                   #row to select data types to view (such as RNAseq or DNA copy number)
                                                                   fluidRow(
                                                                     column(2, "Data"),
                                                                     #input to select which data type is displayed on the x-axis
                                                                     column(5,
                                                                            selectInput("datatype_zoom_x", NULL,c("DNA Copy Number"))
                                                                     ),
                                                                     #input to select which data type is displayed on the y-axis
                                                                     column(5,
                                                                            selectInput("datatype_zoom_y", NULL, c("DNA Copy Number"))
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
  cbPalette <- c("#009E73", "#F0E442", "#0072B2", "#CC79A7","#56B4E9")
  
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
  plotdata <- reactiveValues(zoomdata=(NULL))
  
  ##################################
  #for the left ("zoomed"-out") plot
  ##################################
  #Read in data for the x and y coordinates of the plot
  inputdata_x <- readRDS("CCLE_copynumber_byGene_2013-12-03")
  inputdata_y <- inputdata_x 
  
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
    points$pnt_zoom <- NULL
    return(input$gene_zoom_x)
  })
  y_zoom_sel <- reactive({
    #when the user changes what gene to display, clear the highlighted points the user selected by clicking on the plot
    points$pnt_zoom <- NULL
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
    return(readRDS("DNAcopy_options_CCLE_copynumber_byGene_2013-12-03"))
  })
  
  #Read in the options for the user to select and highlight specific cancer type(s)
  cancer_options <- readRDS("cancerTypes_CCLE_copynumber_byGene_2013-12-03")
  #Holds the list of cancer types the user has selected to highlight
  cancer_highlights <- reactive({
    return(input$cancer_selection)
  })
  
  ###################################
  ######## CHECK EVENT FLAGS ########
  ###################################
  #values for event flags
  flags <- reactiveValues(brushActive = 0, showZoomTable = FALSE, showTable = FALSE)
  output$brushActive <- reactive({ return(flags$brushActive) })
  output$showZoomTable <- reactive({ return(flags$showZoomTable)})
  output$showTable <- reactive({ return(flags$showTable)} )
  outputOptions(output, name = "brushActive", suspendWhenHidden = FALSE)
  outputOptions(output, name = "showZoomTable", suspendWhenHidden = FALSE)
  outputOptions(output, name = "showTable", suspendWhenHidden = FALSE)
  
  #Check to see if the brush is active
  observeEvent(plotdata$zoomdata, {
    if(is.data.frame(plotdata$zoomdata) && nrow(plotdata$zoomdata)==0) {
      flags$brushActive <- 0
      flags$defaultZoomView <- 1
      #print("brush is not active")
    }
    else{
      if(flags$brushActive == 0){
        flags$defaultZoomView <- 0
        #Reset the selectinput options on the right/"zoomed-in" control panel
        updateSelectInput(session, "gene_zoom_x",selected=x_sel())
        updateSelectInput(session, "gene_zoom_y",selected=y_sel())
        #print("zoom plot has been reset to default")
      }
      flags$brushActive <- 1
      #print("brush is active!")
    }
  })
  #If the user has selected data points with the brush, save them to the reactiveValue "zoomdata"
  observeEvent(input$plot_brush,{plotdata$zoomdata <- brushedPoints(defaultplotdata,input$plot_brush)})
  
  #Observe whether the user has decided to toggle the data table on the right/"zoomed-in" plot for viewing
  observeEvent(input$show_zoom_table,{
    flags$showZoomTable <- TRUE
    makeZoomTable(plottingdata,x_zoom_sel(),y_zoom_sel())
  })
  observeEvent(input$hide_zoom_table,{
    print(flags$showZoomTable)
  })
  
  #Observe whether the user has decided to toggle the data table on the left/"zoomed-out" plot for viewing
  observeEvent(input$show_table,{
    flags$showTable <- TRUE
    makeTable(defaultplotdata,x_sel(),y_sel())
  })
  observeEvent(input$hide_zoom_table,{
    flags$showTable <- FALSE
  })
  
  #If appropriate, make the table to display the data in the right/"zoomed-in" plot
  makeZoomTable <- function(data,x_axis,y_axis){
    if(flags$showZoomTable){
      output$zoom_table <- DT::renderDataTable({
        displayTable <- datatable(data[,1:2],colnames = c(x_axis,y_axis))
        return(displayTable)
      })
    }
  }
  
  makeTable <- function(data,x_axis,y_axis){
    if(flags$showTable){
      output$data_table <- DT::renderDataTable({
        displayTable <- datatable(data[,1:2],colnames = c(x_axis,y_axis))
        return(displayTable)
      })
    }
  } 
  #######################################
  ####### RENDERING USER OPTIONS ########
  #######################################
  #Test render
  output$test <- renderText({
  })
  
  #Actual rendering
  output$cancer_selector <- renderUI({
    selectInput("cancer_selection", NULL, cancer_options, multiple = TRUE)
  })
  output$gene_x_selector <- renderUI({
    selectInput("gene_x", NULL, DNAcopy_options())
  })
  output$gene_y_selector <- renderUI({
    selectInput("gene_y", NULL, DNAcopy_options())
  })
  output$gene_zoom_x_selector <- renderUI({
    y_sel()
    return(selectInput("gene_zoom_x", NULL, DNAcopy_options(),selected=x_sel()))
  })
  output$gene_zoom_y_selector <- renderUI({
    x_sel()
    return(selectInput("gene_zoom_y", NULL, DNAcopy_options(),selected=y_sel()))
  })
  
  ##################################
  ####### RENDERING OUTPUTS ########
  ##################################
  
  #output the left/"zoomed-out" plot
  output$plot<- renderPlot({ 
    
    #Select the appropriate rows from our database table based on user input
    if(TRUE){
      #Get the x and y values of interest and save them to a data frame
      x <- sapply(inputdata_x[inputdata_x[,"SYMBOL"]==x_sel(),-c(1:5)], as.numeric)
      y <- sapply(inputdata_y[inputdata_y[,"SYMBOL"]==y_sel(),-c(1:5)], as.numeric)
      
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
      highlightDF <<- defaultplotdata[defaultplotdata$cancer %in% cancer_highlights(),]
      defaultplot <<- defaultplot + geom_point(data = highlightDF, aes(x,y, color=cancer), alpha = transpMed) + scale_colour_manual(values=cbPalette)
      
      #Add the tiny point representing the point selected in the other ("zoomed-in") plot
      if(!is.null(points$pnt_zoom) && (flags$brushActive != 0)){
        matched_pnt_zoom <- updatePoint(points$pnt_zoom,defaultplotdata)
        defaultplot <<- plotPointOnClick(defaultplot, input$plot_zoom_click, matched_pnt_zoom, colOrn, ptMed, updatePoint = FALSE)
      }
      
      #Add in user-selected highlights from the printed data table
      tableHighlights = input$data_table_rows_selected
      print(tableHighlights)
      if(length(tableHighlights)){
        defaultplot <<- defaultplot + geom_point(data = defaultplotdata[tableHighlights,], aes(x,y), color=colRed, size = ptSmol)
      }
      
      #Add user-clicked highlighted points
      defaultplot <<- plotPointOnClick (defaultplot, input$plot_click, points$pnt, colRed, ptBig, updatePoint = TRUE)
      
      #update the stored point of interest if needed
      if(!(is.null(input$plot_click) && is.null(points$pnt)) && !is.null(input$plot_click)){
        if(input$showSelectedOnly){
          points$pnt <<- nearPoints(highlightDF, input$plot_click, maxpoints = 1) 
        }
        else{
          points$pnt <<- nearPoints(defaultplotdata, input$plot_click, maxpoints = 1)
        }
      }
      
      #print the plot
      defaultplot
    }
  }) 
  
  #output details (cell line, cancer type) of the point on the left/"zoomed-out" plot
  output$cell_details <- renderUI({
    if(!is.null(input$plot_click)){}
    printInfo(points$pnt)
  })
  
  #output data (dna copy number, rnaexpression, etc) associated with the selected point on the left/"zoomed-out" plot 
  output$click_info <- renderTable({
    if(!is.null(input$plot_click)){}
    printPoint(points$pnt,x_sel(),y_sel())
  },bordered = TRUE, spacing = 'xs', rownames = TRUE,colnames = TRUE)
  
  
  #output the right/"zoomed-in" plot
  output$plot_zoom<- renderPlot({
    if(flags$brushActive != 0){
      
      #build the plot
      #Get selected points from the left/"zoomed-out" plot and save them
      plotdata$zoomdata <- brushedPoints(defaultplotdata,input$plot_brush)
      oldplottingdata <<- plottingdata
      plottingdata<<-plotdata$zoomdata
      
      #Perform manipulations on the selected data points from the previous block of code based on user input
      
      if(flags$defaultZoomView==0){
        #Get the x and y values of interest and save them to a data frame
        x <- sapply(inputdata_x[inputdata_x[,"SYMBOL"]==x_zoom_sel(),-c(1:5)], as.numeric)
        y <- sapply(inputdata_y[inputdata_y[,"SYMBOL"]==y_zoom_sel(),-c(1:5)], as.numeric)
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
      highlightzoomDF <<- plottingdata[plottingdata$cancer %in% cancer_highlights(),]
      zoomplot <<- zoomplot + geom_point(data = highlightzoomDF, aes(x,y, color=cancer), alpha = transpMed) + scale_colour_manual(values=cbPalette)      
      
      #Add in user-selected highlights from the printed data table
      tableHighlights = input$zoom_table_rows_selected
      print(tableHighlights)
      if(length(tableHighlights)){
        zoomplot <<- zoomplot + geom_point(data = plottingdata[tableHighlights,], aes(x,y), color=colOrn, size = ptSmol)
      }
      
      #plot the user-click highlighted points
      matched_pnt <- updatePoint(points$pnt,plottingdata)
      zoomplot <- plotPointOnClick(zoomplot, input$plot_click, matched_pnt, colRed, ptMed, updatePoint = FALSE)
      zoomplot <- plotPointOnClick (zoomplot, input$plot_zoom_click, points$pnt_zoom, colOrn, ptBig, updatePoint = TRUE)
      
      #update the stored point of interest if needed
      if(!(is.null(input$plot_zoom_click) && is.null(points$pnt_zoom) ) && !is.null(input$plot_zoom_click)){
        if(input$showSelectedOnly){
          points$pnt_zoom <<- nearPoints(highlightzoomDF, input$plot_zoom_click, maxpoints = 1) 
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
    if(!is.null(input$plot_zoom_click)){}
    printInfo(points$pnt_zoom)
  })    
  
  #output data (dna copy number, etc) associated with the selected point on the right/"zoomed-in" plot
  output$zoom_click_info <- renderTable({
    if(!is.null(input$plot_zoom_click)){}
    printPoint(points$pnt_zoom,x_zoom_sel(),y_zoom_sel())
  },bordered = TRUE, spacing = 'xs', rownames = TRUE,colnames = TRUE)
  
  #################################
  ######## VARIOUS FUNTIONS #######
  #################################
  
  #Add a highlighted point to the passed in plot
  plotPointOnClick <- function(plot,inputClick,point,ptColor, ptSize, updatePoint){
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
    print(point)
    str1 <- paste("Cell Line:  ", point$line)
    str2 <-paste("Cancer Type:  ", point$cancer)
    HTML(paste(str1,str2,sep='<br/>'))
  }
  
  #Print data associated with the point (such as DNA copy number, mRNA, etc)
  printPoint <- function(point,x_axis,y_axis){
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
      
      return (pointDetail)
    }
  }
  
  #Reset the values of the stored points to NULL
  clearPoints <- function(){
    points$pnt <- NULL
    points$pnt_zoom <- NULL
  }
  
  #take a "point" from a plot and return the correpsonding point in "data"
  updatePoint <- function(point, data){
    return(data[row.names(point),])
  }
  
}

shinyApp(ui = ui, server = server)

