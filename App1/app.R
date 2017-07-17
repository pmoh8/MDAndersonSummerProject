library(shiny)
library(DT)
library(ggplot2)
library(RSQLite)
library(bcrypt)
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
    #Application title
    div(style="display: inline-block;vertical-align:center;",titlePanel("Cool Application")),
    
    #Login option:  display only when user is not logged in
    conditionalPanel(condition = '!output.loggedIn',
                     fluidRow(
                       div(style="display: inline-block;vertical-align:center;float:right;",column(1,"")),
                        div(style="display: inline-block;vertical-align:center;float:right;",
                            div(style="display: inline-block;vertical-align:center;",textInput("inputUser", NULL, value = "", width = 150, placeholder = "username")),
                            div(style="display: inline-block;vertical-align:center;",passwordInput("inputPass", NULL, value = "", width = 150, placeholder = "password")),
                            div(style="display: inline-block;vertical-align:center;",actionButton("loginButt", "Login"))
                            )
                       ),
                     fluidRow(
                       div(style="display: inline-block;vertical-align:center;float:right;",column(1,"")),
                       div(style="vertical-align:center;float:right;color:red;", textOutput("loginError")) 
                     ),
                     fluidRow(
                       div(style="display: inline-block;vertical-align:center;float:right;",column(1,"")),
                       div(style="vertical-align:center;float:right;", actionLink("registerButt","Don't have an account?  Click here to register!"))
                     )
    ),
    
    #Logout option:  display only when user is logged in
    conditionalPanel(condition = 'output.loggedIn',
                     div(style="display: inline-block;vertical-align:center;float:right;",actionButton("logoutButt", "Logout")),
                     div(style="display: inline-block;vertical-align:center;float:right; width: 250px;", textOutput("userinfo"))
    ),
    
    #set up tabs
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
                                                                     column(5, uiOutput("x_zoom_datatype_selector")
                                                                     ),
                                                                     #input to select which data type is displayed on the y-axis
                                                                     column(5, uiOutput("y_zoom_datatype_selector")
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
                                       ),
                                       br(),
                                       br(),
                                       br()
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

#connect to the database of registered users
#Database structure:  username  |   hashed password   |   access level
mydb <- dbConnect(RSQLite::SQLite(), "users_db.sqlite")

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

  currentUser <- reactiveValues(username = "NULL", access_lvl = 1)
  
  #intialize reactive values for points the user can choose to highlight by clicking on each plot
  points <- reactiveValues(pnt = NULL, pnt_zoom=NULL)
  plotdata <- reactiveValues(zoomdata=(NULL), highlightDF=(NULL), highlightzoomDF=(NULL))
  plottingdata <- NULL
  
  ##################################
  #for the left ("zoomed"-out") plot
  ##################################
  #Read in data for the x and y coordinates of the plot
  database_dataframe <- readRDS("database_table")
  datatype_options <- reactive({ return(database_dataframe[database_dataframe$access_lvl<=currentUser$access_lvl,1]) })
  
  inputdata_x <- reactive({ return(readRDS(toString(database_dataframe[database_dataframe[,"Label"]==toString(x_datatype_sel()),2]))) })
  inputdata_y <- reactive({ return(readRDS(toString(database_dataframe[database_dataframe[,"Label"]==toString(y_datatype_sel()),2]))) })
  
  x_datatype_sel <- reactive ({
    #when the user changes what gene to display, clear the highlighted points the user selected by clicking on the plot
    clearPoints()
    return(input$datatype_x)
  })
  y_datatype_sel <- reactive ({
    #when the user changes what gene to display, clear the highlighted points the user selected by clicking on the plot
    clearPoints()
    return(input$datatype_y)
  })
  
  #Have the user select which genes to display on the x and y axis respectively
  x_sel <- reactive({ return(input$gene_x) })
  y_sel <- reactive({ return(input$gene_y) }) 
  
  optionsA <- reactive({
    common <- sort(intersect(inputdata_x()$SYMBOL, inputdata_y()$SYMBOL))
    return(common)
  })
  
  #################################
  #For the right ("zoomed-in") plot
  #################################
  inputdata_zoom_x <- reactive({ return(readRDS(toString(database_dataframe[database_dataframe[,"Label"]==toString(x_zoom_datatype()),2]))) })
  inputdata_zoom_y <- reactive({ return(readRDS(toString(database_dataframe[database_dataframe[,"Label"]==toString(y_zoom_datatype()),2]))) })
  
  #Have the user select which datatypes to go with the genes to display on the x and y axis respectively
  x_zoom_datatype <- reactive({return(input$datatype_zoom_x)})
  y_zoom_datatype <- reactive({return(input$datatype_zoom_y)})
  
  #Have the user select which genes to display on the x and y axis respectively
  x_zoom_sel <- reactive({ return(input$gene_zoom_x) })
  y_zoom_sel <- reactive({ return(input$gene_zoom_y) })
  
  optionsB <- reactive({
    common <- sort(intersect(inputdata_x()$SYMBOL, inputdata_y()$SYMBOL))
    return(common)
  })

  ##################
  #Read in RDS files
  ##################
  #Read in the selection options for x_sel, y_sel, x_zoom_sel, and y_zoom_sel

  
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
  flags <- reactiveValues(brushActive = FALSE,  showTable = FALSE, tableHasSelected = FALSE, showZoomTable = FALSE, zoomTableHasSelected = FALSE, defaultZoomView = TRUE, loggedIn = FALSE)
  
  output$brushActive <- reactive({ return(flags$brushActive) })
  output$showTable <- reactive({ return(flags$showTable)} )
  output$tableHasSelected <- reactive({ return(flags$tableHasSelected) })
  output$showZoomTable <- reactive({ return(flags$showZoomTable)})
  output$zoomTableHasSelected <- reactive({ return(flags$zoomTableHasSelected) })
  output$loggedIn <- reactive({ return(flags$loggedIn) })
  
  outputOptions(output, name = "brushActive", suspendWhenHidden = FALSE)
  outputOptions(output, name = "showTable", suspendWhenHidden = FALSE)
  outputOptions(output, name = "tableHasSelected", suspendWhenHidden = FALSE)
  outputOptions(output, name = "showZoomTable", suspendWhenHidden = FALSE)
  outputOptions(output, name = "zoomTableHasSelected", suspendWhenHidden = FALSE)
  outputOptions(output, name = "loggedIn", suspendWhenHidden = FALSE)
  
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
  
  toListenCancerSelectionChange <- reactive({
    list(input$showSelectedOnly,input$cancer_selection)
  })
  observeEvent(toListenCancerSelectionChange(),{
    if (input$showSelectedOnly){
      makeTable(plotdata$highlightDF)
      makeZoomTable(plotdata$highlightzoomDF)
    }
    else{
      makeTable(defaultplotdata)
      makeZoomTable(plottingdata)
    }
  })
  
  toListenTable <- reactive({
    list(input$datatype_x, input$gene_x, input$datatype_y, input$gene_y)
  })
  observeEvent(toListenTable(),{
    if (input$showSelectedOnly){
      makeTable(plotdata$highlightDF)
    }
    else{
      makeTable(defaultplotdata)
    }
  })
  
  toListenZoomTable <- reactive({
    list(input$datatype_zoom_x, input$gene_zoom_x, input$datatype_zoom_y, input$gene_zoom_y)
  })
  observeEvent(toListenZoomTable(),{
    if (input$showSelectedOnly){
      makeZoomTable(plotdata$highlightzoomDF)
    }
    else{
      makeZoomTable(plottingdata)
    }
  })
  
  #Observe whether the user has decided to toggle the data table on the left/"zoomed-out" plot for viewing
  observeEvent(input$show_table,{
    cat(file=stderr(), "observeEvent:  input$show_table", "\n")
    flags$showTable <- TRUE
    if (input$showSelectedOnly){
      makeTable(plotdata$highlightDF)
    }
    else{
      makeTable(defaultplotdata)
    }
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
    makeZoomTable(plottingdata)
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
  makeTable <- function(data){
    if(flags$showTable){
      output$data_table <- DT::renderDataTable({
        cat(file=stderr(), "output$data_table", "\n")
        displayTable <- datatable(data[,1:2],colnames = c(paste(x_sel(), x_datatype_sel(), sep="-"),paste(y_sel(), y_datatype_sel(), sep="-")))
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
  makeZoomTable <- function(data){
    if(flags$showZoomTable){
      output$zoom_table <- DT::renderDataTable({
        cat(file=stderr(), "output$zoom_table", "\n")
        displayTable <- datatable(data[,1:2],colnames = c(paste(x_zoom_sel(), x_zoom_datatype(), sep="-"),paste(y_zoom_sel(), y_zoom_datatype(), sep="-")))
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
  

  ##################################
  ######## LOGGING IN / OUT ########
  ##################################
  #If the user clicks the login button, check to see if their credientials are valid and log the user in if they are
  observeEvent(input$loginButt,{
    #get user inputs
    username <- isolate(input$inputUser)
    
    #send a parameterized query to the database and see if a user with the inputted name exists in the users table
    query <- dbSendQuery(mydb, "SELECT * FROM users WHERE user = ? COLLATE NOCASE")
    dbBind(query,list(username))
    userfromtable <- dbFetch(query)
    dbClearResult(query)
    
    #If the entered username exists in the database, compare the entered password and stored password using the hashpw() function
    if(nrow(userfromtable) !=0 ){
      
      #Get the entered password and the stored hashed password
      passwordguess <- isolate(input$inputPass)
      password <- userfromtable$password
      
      #check if the entered password and the stored password match.  If they do, log the user in
      flags$loggedIn <- (hashpw(passwordguess, password)==password)
      
      #If the login was successful...
      if(flags$loggedIn){
        #clear the input boxes and error message
        updateTextInput(session, "inputUser", value = "") 
        updateTextInput(session, "inputPass", value = "") 
        output$loginError <- renderText({""})
        
        #Store the user's username and access level in the currentUser variable, and then welcome the user by name
        currentUser$username <- userfromtable$user
        currentUser$access_lvl <- userfromtable$access_lvl
        output$userinfo <- renderText({
          HTML(paste("Currently logged in as ",as.character(currentUser$username)))
        })
      }
      
      #If the login was unsuccessful, clear the password input box and display an error
      else{
        updateTextInput(session, "inputPass", value = "")
        output$loginError <- renderText({
          "Error logging in.  Please check your username and password."
        })
      }
    }
    
    #if the entered user doesn't exist in the database, set the flag to show that the current user is not logged in. clear the password entry box, and display an error
    else{ 
      flags$loggedIn <- FALSE 
      updateTextInput(session, "inputPass", value = "")
      output$loginError <- renderText({
        "Error logging in.  Please check your username and password."
        })
      }
  })
  
  #If the user clicks the logout button, log them out and clear the welcome text field.
  observeEvent(input$logoutButt,{
    flags$loggedIn <- FALSE
    currentUser$username <- "NULL"
    currentUser$access_lvl <- 1
    output$userinfo <- renderText({""})
  })
  
  #If the user clicks the register action link, bring up a modal form to let them make an account
  #Requirements for a new account:  Unique username of at least 4 characters, unique e-mail, and password of at least 6 characters
  observeEvent(input$registerButt,{
    #bring up the modal with entry fields for usename, e-mail, and password as well as error fields
    showModal(modalDialog(
      title = "New User Registration",
      "Please fill out the fields below",
      br(), br(),
      textInput("newUser", "Username (at least 4 characters)", placeholder = 'username'),
      textInput("newEmail", "E-mail", placeholder = 'e-mail'),
      passwordInput("newPass1", "Password (at least 6 characters)", placeholder = 'password'),
      passwordInput("newPass2", "Confirm Password", placeholder = 'password'),
      br(),
      div(style="color:red;",textOutput("regError")),
      div(style="color:red;",textOutput("emailError")),
      br(),
      div(style="color:red;",textOutput("passError1")),
      div(style="color:red;",textOutput("passError2")),
      easyClose = TRUE,
      
      #Submit and exit buttons
      footer = tagList(
        actionButton("newUserButt", "Submit"),
        modalButton("Cancel")
      )
    ))
    
    #clear error outputs upon opening modal
    output$regError <- renderText({ "" })
    output$emailError <- renderText({ "" })
    output$passError1 <- renderText({ "" })
    output$passError2 <- renderText({ "" })
  })
  
  #Verify the entry fields when the user clicks the submit button on the user registration modal
  observeEvent(input$newUserButt,{
    #Assume everything is horribly wrong to start off until the user leads us to believe otherwise
    #Requirements for a new account:  Unique username of at least 4 characters, unique e-mail, and password of at least 6 characters
    nameOK <- FALSE
    namelengthOK <- FALSE
    passOK <- FALSE
    passlengthOK <- FALSE
    emailOK <- FALSE
    emailformatOK <- FALSE
    
    #Get user inputs from the modal
    newusername <- isolate(input$newUser)
    newemail <- isolate(input$newEmail)
    newpass <- isolate(input$newPass1)
    
    #Check if the enteres usernames is ok (correct minimum length and is not already taken)
    namelengthOK <- (nchar(newusername) >= 4)
    
    #If the length is ok, then let us check if the username is already taken
    if(namelengthOK){
      #clear the error message to start off with
      output$regError <- renderText({ "" })
      
      #check to see if the username is already taken using a prepared query
      query <- dbSendQuery(mydb, "SELECT * FROM users WHERE user = ? COLLATE NOCASE")
      dbBind(query,list(newusername))
      returnUser <- dbFetch(query)
      
      #If a dataframe with no rows is returned by the above query, it means that the username is not taken
      nameOK <- (nrow(returnUser) == 0)
      #If the name is not taken, clear the error message.  If it is, let the user know the name is taken.
      if(nameOK){
        output$regError <- renderText({ "" })
      }
      else{
        output$regError <- renderText({ "That username is taken.  Please choose a new username." })
      }
      dbClearResult(query)
    }
    #If the length of the username is too short, let the user know
    else{
      output$regError <- renderText({ "Your username is too short.  Please choose something longer." })
    }
    
    #Check if a valid e-mail format is entered by seeing if the '@' symbol is in the email.
    emailformatOK <- grepl("@",newemail)
    
    #If the e-mail format is ok, check and make sure that the e-mail is not already taken
    if(emailformatOK){
      #parameterized query to see if a user with the same e-mil already exists in the database
      output$emailError <- renderText({ "" })
      emailquery <- dbSendQuery(mydb, "SELECT * FROM users WHERE email = ? COLLATE NOCASE")
      dbBind(emailquery,list(newemail))
      returnemail <- dbFetch(emailquery)
      
      #If a dataframe with no rows is returned by the above query, it means that the e-mail is not taken
      emailOK <- (nrow(returnemail) == 0 )
      #If the e-mail is not taken, clear the error message.  If it is, let the user know the e-mail is taken.
      if(emailOK){
        output$emailError <- renderText({ "" })
      }
      else{
        output$emailError <- renderText({ "That e-mail is already taken.  Please choose a new e-mail." })
      }
      dbClearResult(emailquery)
    }
    #If the e-mail format is not ok, let the user know
    else{
      output$emailError <- renderText({ "Please enter a valid e-mail" })
    }
    
    #See if the passwords entered match and if they are of the correct length
    passOK <- (input$newPass1 == input$newPass2)
    passlengthOK <- (nchar(input$newPass1) >= 6)
    #output an error message about password length to the user if need be
    if(passlengthOK){
      output$passError1 <- renderText({ "" })
    }
    else{
      output$passError1 <- renderText({ "Your password is too short.  Please choose something longer." })
    }
    
    #output an error message about not matching passwords to the user if need be
    if(passOK){
      output$passError2 <- renderText({ "" })
    }
    else{
      output$passError2 <- renderText({ "Please ensure that both passwords match." })
    }
    
    #If ther user's entered info passes all of the checks, add them to the user database
    if(namelengthOK && nameOK && passlengthOK && passOK && emailformatOK && emailOK){
      statement <- dbSendStatement(mydb, "INSERT INTO users (user, email, password, access_lvl) VALUES (?, ?, ?, 1)")
      dbBind(statement,list(newusername, newemail, hashpw(newpass)))
      returnUser <- dbFetch(statement)
      cat(file=stderr(), "New user successfully created", "\n")
      
      #Automatically log in the user
      output$loginError <- renderText({""})
      flags$loggedIn <- TRUE
      #Store the user's username and access level in the currentUser variable, and then welcome the user by name
      currentUser$username <- newusername
      currentUser$access_lvl <- 1
      output$userinfo <- renderText({
        HTML(paste("Currently logged in as ",as.character(currentUser$username)))
      })
      cat(file=stderr(), "New user successfully logged in", "\n")
      #Close the modal
      removeModal()
      dbClearResult(statement)
    }
    
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
    selectInput("datatype_x", NULL, datatype_options(), selectize=FALSE)
  })
  
  output$y_datatype_selector <- renderUI({
    cat(file=stderr(), "output$y_datatype_selector", "\n")
    selectInput("datatype_y", NULL, datatype_options(), selectize=FALSE)
  })
  
  output$gene_x_selector <- renderUI({
    cat(file=stderr(), "output$gene_x_selector", "\n")
    selectInput("gene_x", NULL, optionsA(), selected=x_sel(), selectize=FALSE)
  })
  
  output$gene_y_selector <- renderUI({
    cat(file=stderr(), "output$gene_y_selector", "\n")
    selectInput("gene_y", NULL, optionsA(), selected=y_sel(), selectize=FALSE)
  })
  
  output$x_zoom_datatype_selector <- renderUI({
    cat(file=stderr(), "output$x_datatype_selector", "\n")
    selectInput("datatype_zoom_x", NULL, datatype_options(), selected=x_datatype_sel(), selectize=FALSE)
  })
  
  output$y_zoom_datatype_selector <- renderUI({
    cat(file=stderr(), "output$y_datatype_selector", "\n")
    selectInput("datatype_zoom_y", NULL, datatype_options(), selected=y_datatype_sel(), selectize=FALSE)
  })

  output$gene_zoom_x_selector <- renderUI({
    cat(file=stderr(), "output$gene_zoom_x_selector", "\n")
    y_sel()
    return(selectInput("gene_zoom_x", NULL, optionsB(),selected=x_sel(), selectize=FALSE))
  })

  output$gene_zoom_y_selector <- renderUI({
    cat(file=stderr(), "output$gene_zoom_y_selector", "\n")
    x_sel()
    return(selectInput("gene_zoom_y", NULL, optionsB(),selected=y_sel(), selectize=FALSE))
  })
  

  
  ##################################
  ####### RENDERING OUTPUTS ########
  ##################################
  
  #output user info

  #output the left/"zoomed-out" plot
  output$plot<- renderPlot({ 
    cat(file=stderr(), "output$plot", "\n")
    #Select the appropriate rows from our database table based on user input
      #Get the x and y values of interest and save them to a data frame
       x <- data.frame(sapply(inputdata_x()[inputdata_x()[,"SYMBOL"]==x_sel(),-1], as.numeric))
       y <- data.frame(sapply(inputdata_y()[inputdata_y()[,"SYMBOL"]==y_sel(),-1], as.numeric))
      
      #make the dataframe to hold the information we're displaying in the scatterplot
      defaultplotdata <<- merge(x,y, by="row.names")
      colnames(defaultplotdata) <<- c("cell","x","y")
      rownames(defaultplotdata) <<- defaultplotdata[,1]
      defaultplotdata$cell <<- NULL
      
      #make columns for cancer type and cell line name
      underscore <- regexpr("_",rownames(defaultplotdata))
      line <- substring(rownames(defaultplotdata), 1, underscore-1)
      cancer <- substring(rownames(defaultplotdata), underscore+1)
      defaultplotdata <<- cbind(defaultplotdata,cancer)
      defaultplotdata <<- cbind(defaultplotdata,line)
      
      #if the user has the "Show selected cancers only" box checked off, only show the cancers of interest
      defaultplot <<- ggplot() + labs(x = paste(x_sel()," - ",x_datatype_sel()), y = paste(y_sel()," - ",y_datatype_sel()), title = "Test Scatter")
      
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
      #make sure the same point is saved between axis changes
      if(!is.null(points$pnt)){
        points$pnt <- isolate(updatePoint(points$pnt,defaultplotdata))
      }
      #Add in user-selected highlights from the printed data table
      tableHighlights = input$data_table_rows_selected
      print("tableHighlights")
      print(tableHighlights)
      if(length(tableHighlights)){
        print(defaultplotdata[tableHighlights,])
        print(plotdata$highlightDF[tableHighlights,])
        if (input$showSelectedOnly){
          defaultplot <<- defaultplot + geom_point(data = plotdata$highlightDF[tableHighlights,], aes(x,y), color=colRed, size = ptSmol)
        }
        else{
          defaultplot <<- defaultplot + geom_point(data = defaultplotdata[tableHighlights,], aes(x,y), color=colRed, size = ptSmol)
        }
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
    printPoint(points$pnt,x_sel(),y_sel(),x_datatype_sel(),y_datatype_sel())
  },bordered = TRUE, spacing = 'xs', rownames = TRUE,colnames = TRUE,sanitize.text.function=function(x){x})

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
        x2 <- data.frame(sapply(inputdata_zoom_x()[inputdata_zoom_x()[,"SYMBOL"]==x_zoom_sel(),-1], as.numeric))
        y2 <- data.frame(sapply(inputdata_zoom_y()[inputdata_zoom_y()[,"SYMBOL"]==y_zoom_sel(),-1], as.numeric))
        
        #make the dataframe to hold the information we're displaying in the scatterplot
        cancersOfInterest <- row.names(plotdata$zoomdata)
        
        newDF <<-  merge(x2,y2, by="row.names")
        colnames(newDF) <<- c("cell","x","y")
        rownames(newDF) <<- newDF[,1]
        newDF$cell <<- NULL
        newDF <<- newDF[cancersOfInterest,]
        
        #make columns for cancer type and cell line name
        underscore <- regexpr("_",rownames(plotdata$zoomdata))
        line <- substring(rownames(plotdata$zoomdata), 1, underscore-1)
        cancer <- substring(rownames(plotdata$zoomdata), underscore+1)
        newDF <- cbind(newDF, cancer)
        newDF <- cbind(newDF,line)
        plottingdata<<-newDF
      
        #Determine if the new plotting dataset and the old one are the same or not.  If they are different, re-rendeer the displayed data table
        # if(!isTRUE(all.equal(plottingdata,oldplottingdata))){
        #   makeZoomTable(plottingdata,x_zoom_sel(),y_zoom_sel())
        # }
      }
      
      #if the user has the "Show selected cancers only" box checked off, only show the cancers of interest
      zoomplot<<-ggplot() + labs(x = paste(x_zoom_sel()," - ", x_zoom_datatype()), y = paste(y_zoom_sel()," - ",y_zoom_datatype()), title = "Test Scatter") 
      
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
        if (input$showSelectedOnly){
          zoomplot <<- zoomplot + geom_point(data = plotdata$highlightzoomDF[tableHighlights,], aes(x,y), color=colOrn, size = ptSmol)
          }
        else{
          zoomplot <<- zoomplot + geom_point(data = plottingdata[tableHighlights,], aes(x,y), color=colOrn, size = ptSmol)
        }
        #zoomplot <<- zoomplot + geom_point(data = plottingdata[tableHighlights,], aes(x,y), color=colOrn, size = ptSmol)
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
    printPoint(points$pnt_zoom,x_zoom_sel(),y_zoom_sel(),x_zoom_datatype(),y_zoom_datatype())
  },bordered = TRUE, spacing = 'xs', rownames = TRUE,colnames = TRUE,sanitize.text.function=function(x){x})
  
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
    if(!is.null(point) && (nrow(point)>0)){
      print(point)
      str1 <- paste("Cell Line:  ", point$line)
      str2 <-paste("Cancer Type:  ", point$cancer) 
      
      HTML(paste(str1,str2,sep='<br/>'))
    }
  }
  
  #Print data associated with the point (such as DNA copy number, mRNA, etc)
  printPoint <- function(point,x_axis,y_axis,x_dtype,y_dtype){
    cat(file=stderr(), "printPoint", "\n")

    #Make sure the point is valid before trying to do anything
    if(is.data.frame(point) && nrow(point)!=0) {
      #See if the x and y-axis are the same so that we don't mess up and have duplicate row names and make R throw a fit
      if((x_axis == y_axis) && (x_dtype == y_dtype)){
        pointDetail <- data.frame(c(point$x))
        row.names(pointDetail)<-c(x_axis) 
        names(pointDetail)<-c(x_dtype)
      }
      else if ((x_axis != y_axis) && (x_dtype == y_dtype)){
        pointDetail <- data.frame(c(point$x, point$y))
        row.names(pointDetail)<-c(x_axis, y_axis)
        names(pointDetail)<-c(x_dtype)
      }
      else if((x_axis == y_axis) && (x_dtype != y_dtype)){
        pointDetail <- data.frame(point$x, point$y)
        row.names(pointDetail)<-c(x_axis)
        names(pointDetail)<-c(x_dtype, y_dtype)
      }
      else{
        p1 <- sapply(inputdata_y()[inputdata_y()[,"SYMBOL"]==x_axis,row.names(point)[1]], as.numeric)
        p2 <- sapply(inputdata_x()[inputdata_x()[,"SYMBOL"]==y_axis,row.names(point)[1]], as.numeric)
        cat(file=stderr(), as.character(p1), "\n")
        pointDetail <- data.frame(c(paste("<strong>",point$x,"</strong>"), p2),c(p1,paste("<strong>",point$y,"</strong>")))
        row.names(pointDetail)<-c(x_axis, y_axis)
        names(pointDetail)<-c(x_dtype, y_dtype)
      }
      
      #add column names
      return (pointDetail)
    }

  }
  
  #Reset the values of the stored points to NULL
  clearPoints <- function(){
    cat(file=stderr(), "clearPoints", "\n")
    points$pnt <- NULL
    points$pnt_zoom <- NULL
  }
  
  #take a "point" from a plot and return the correpsonding row in "data"
  updatePoint <- function(point, data){
    cat(file=stderr(), "updatePoint", "\n")
    return(data[row.names(point),])
  }
  
}

shinyApp(ui = ui, server = server)

