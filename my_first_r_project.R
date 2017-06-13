# library(shiny)
# ui <- fluidPage(
#   "hello world",
#   sliderInput(inputId = "mynum",
#               label = "choose some integer",
#               value = 25, min = 1, max = 100),
#   textInput(inputId="mytitle",
#             label="type a title"),
#   actionButton(inputId="clicks",label="click me"),
#   plotOutput("myhist"),
#   verbatimTextOutput("stats"),
#   textOutput("numofclicks")
# )
# 
# server <-function(input, output){
#   myvector <- reactive({as.integer(10*rnorm(input$mynum))})
#   output$myhist <- renderPlot({
#     hist(myvector(),
#          breaks = 20,
#          xlim=c(-30,30),
#          ylim=c(0,15),
#          sub = isolate(input$mytitle),
#         main = paste(input$mynum, "random normally distributed variables lol"))
#   }) 
#   output$stats <- renderPrint({
#     summary(myvector())
#   })
#   observeEvent(input$clicks,{print(as.numeric(input$clicks))})
#   output$numofclicks <- renderText(paste(as.numeric(input$clicks), "clicks so far"))
# }
# shinyApp(ui = ui, server = server)
library(shiny) 
ui <- 
  fluidPage( 
    titlePanel("Cool Application"),
    
    sidebarLayout("",
      mainPanel(width = 12,
        tabsetPanel(
          tabPanel("Scatterplot", 
                   br(),
                   fluidRow(
                     column(3,
                            wellPanel(
                              fluidRow(
                                column(2),
                                column(5, "x-axis"),
                                column(5, "y-axis")
                                ),
                              fluidRow(
                                column(2,
                                       "Data"),
                                column(5,
                                       selectInput("variable", NULL,
                                                   c("Data1" = "cyl",
                                                     "Data2" = "am",
                                                     "Data3" = "gear"))
                                       ),
                                column(5,
                                       selectInput("variable", NULL,
                                                   c("Data1" = "cyl",
                                                     "Data2" = "am",
                                                     "Data3" = "gear")))
                                ),
                              fluidRow(
                                column(2,"Gene"),
                                column(5,
                                       selectInput("variable", NULL,
                                                   c("Gene1" = "cyl",
                                                     "Gene2" = "am",
                                                     "Gene3" = "gear"))
                                       ),
                                column(5,
                                       selectInput("variable", NULL,
                                                   c("Gene1" = "cyl",
                                                     "Gene2" = "am",
                                                     "Gene3" = "gear")))
                                )
                              ),
                            br(),
                            "Select a cancer type to highlight",
                            selectInput("variable", NULL,
                                        c("Cancer1" = "cyl",
                                          "Cancer2" = "am",
                                          "Cancer3" = "gear")),
                            checkboxInput("somevalue", "Show selected cancers only?", FALSE)
                            ),
                     column(9,
                            fluidRow(
                              column(6, 
                                     wellPanel("Details1")),
                              column(6,
                                     wellPanel("Details2"))
                              ),
                            fluidRow(
                              column(6,
                                     wellPanel(plotOutput("plot")),
                                     "plot1"),
                              column(6,
                                     wellPanel(plotOutput("plot")),
                                     "plot2")
                              ),
                            "hi"
                            )
                     )
                   ),
                   
          tabPanel("Heatmap", verbatimTextOutput("summary"))
        )
      )
    )
    
  )

server <- function(input, output) { 
  output$hist<- renderPlot({ 
    hist(rnorm(input$n)) 
  }) 
}
shinyApp(ui = ui, server = server)