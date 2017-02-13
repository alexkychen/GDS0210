library(shiny)

ui <- fluidPage(
  #Input
  sidebarLayout(
    sidebarPanel(
      sliderInput("popsize", label = "Population Size", value=10, min=1, max=100),
      selectInput("popunit", label = "", choices = c("one"=1,"ten"=10,"hundred"=100,"thousand"=1000,"million"=1000000)),
      #fileInput(inputId = "fileIn", label = "Upload your file"),
      selectInput("repro.mode", label ="Reproduction mode", choices = c("Semelparity" = "semelparity","Iteroparity" = "iteroparity")),
      actionButton("go", label="Make Plot")
      
    ),
    mainPanel(
      #Output
      plotOutput(outputId = "plot1")
    )
  )
  
)
  
server <- function(input, output){
  data <- eventReactive(input$go, {
    size <- input$popsize * as.numeric(input$popunit)
    title <- paste("Population size",input$popunit)
    hist(rnorm(size), main=title)
  })
  output$plot1 <- renderPlot({
    data()
  })
}

shinyApp(ui = ui, server = server)
  

  
  