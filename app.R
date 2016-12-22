# app.R
library("shiny")
library('igraph')
library('dplyr')

source("helpers.R")
flavor.list <- c()

ui <- fluidPage(
  # *Input() functions,
  actionButton("minus", "-"),actionButton("plus", "+"),
  selectizeInput(inputId="vertex", label="Choose Flavor", 
                 choices=vertexNames, options=list(maxOptions=2500)),
  # *Output() functions,
  verbatimTextOutput("nText"),
  
  # more *Input() fuctions,
  checkboxInput("chart", "Check to View Intersection"),
  conditionalPanel(
    condition = "input.chart == true",
    dataTableOutput("flavorIntersection")
  ),
  
  checkboxInput("plotCheck", "Check to Plot Graph"),
  conditionalPanel(
    condition = "input.plotCheck == true",
    plotOutput("plot")
  )
)


server <- function(input, output, session){
#  updateSelectizeInput(session, inputID="vertex", choices=vertexNames)
  # render*() function creates the type of output youwish to make
  observeEvent(input$plus, {
    flavor.list <<- unique(append(flavor.list, last(input$vertex)))
  } )
  
 observe( {
   input$plus
   vertexNames <- getIntersectionNames(food.graph, vertexNames=my_flavor.list())
   updateSelectizeInput(session, inputId="vertex",
                        choices=vertexNames)
 } )
 
 

  observe({
    input$minus
    flavor.list <<- flavor.list[-length(flavor.list)]
  })
  
  my_flavor.list <- reactive( {
    if(input$minus==0){
      flavor.list
    }
    isolate({
      input$minus
      flavor.list 
    })
    if(input$plus > 0){
      flavor.list 
    }
  } )
  
  output$nText <- renderText( {
    paste(my_flavor.list(), " | ")
  } ) 
  output$flavorIntersection <- renderDataTable( {
    getDegree(getIntersection(food.graph, my_flavor.list()))
  } )

  output$plot <- renderPlot( {
    plotGetIntersection(food.graph, my_flavor.list()) }, width=1800, height=1100)
}


shinyApp(ui=ui, server=server)


# Use observeEvent whenever you want to perform an action in response to an
# event. (Note that "recalculate a value" does not generally count as performing
# an action–see eventReactive for that.) The first argument is the event you
# want to respond to, and the second argument is a function that should be
# called whenever the event occurs.