library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Gene drive simulator"),
  helpText("Simulate how many generations a gene drive would take to be spread out in a population. This model assumes: 1) Male and female contributions are equal, 2) species is semelparous, and 3) mortality or survival is equal between wild and gene-drive individuals."),
  helpText(" Enter a population size and a number of gene-drive individuals to be released at F0."),
  #Input
  fluidRow(
    column(3,
      sidebarPanel( width = 14,
        tabsetPanel(
          tabPanel(title = "Basic",
            helpText(""),
            numericInput(inputId = "popsize",label = "Wild population size (individual)", 100),
            numericInput(inputId = "release",label = "F0 gene drive (individual)", 20),
            selectInput(inputId = "drawline", label = "Draw regression", choices = c("None"="none","Logistic regression"="logistic","Linear regression"="linear")),
            conditionalPanel(condition = "input.drawline == 'logistic'",
                             checkboxInput(inputId = "se1", label = "Include 95% confidence interval")),
            conditionalPanel(condition = "input.drawline == 'linear'",
                            checkboxInput(inputId = "se2", label = "Include 95% confidence interval")),
            actionButton("go", label="Simulate"),
            helpText("*Computing time will increase with the increasing ratio of population size to F0 gene drive.")
          ),
          tabPanel(title = "Advanced",
            helpText("Under construction..."),
            numericInput(inputId = "popsize",label = "Wild population size (individual)", 100),
            numericInput(inputId = "release",label = "F0 gene drive (individual)", 20)
          )
        )
      )
    ),
    column(7,
      #Output plot
      plotOutput(outputId = "plot1")
    ),
    column(2,
      #output table
      tableOutput('tbl')     
    )
  )
)
  
server <- function(input, output){
  data <- eventReactive(input$go, {
    wild <- round(input$popsize / input$release)
    drive <- 1
    if(wild > 90){
        showModal(modalDialog(
          title="Important message",
          "The ratio of wild population to gene drive is large. Simulation might take a few minutes or longer. Please be patient!", easyClose=T
        ))
    }

    #define a function to get the next gene drive generation
    driveNextGen <- function(wild=NULL, drive=NULL){
      F0 <- c(rep("W", wild*2), rep("G", drive*2)) #F0 gametes
      F1 <- NULL
      mixindex <- sample(length(F0))#create sampling index
      for (i in seq(1, length(mixindex), 2)){
        pair <- F0[mixindex[c(i, i+1)]] #Get every two individuals as a pair
        if("W" %in% pair & "G" %in% pair){
          pair <- c("G","G")
          F1 <- c(F1, pair)
        }else{
          F1 <- c(F1, pair)
        }
      }
      return(F1)
    }
    #Estimate the initial gene drive frequency and create a data frame to save results
    driveFreq <- drive / (drive + wild)
    df <- data.frame(0, driveFreq)
    gen <- 0
    #Simulate gene drive freq across generations until drive freq = 1
    while(!is.na(wild)){ #keep looping until wild becomes NA
      gen <- gen + 1
      showNotification(ui=paste0("Simulating F",gen," generation..."), id = paste0("F",gen), duration = NULL)
      res <- driveNextGen(wild=wild, drive=drive)
      restb <- table(res)
      wild <- restb[2]
      drive <- restb[1]
      driveFreq <- restb[1]/sum(restb) #restb[1] is restb[names(restb)=="G]
      data <- c(gen, driveFreq)
      df <- rbind(df, data)
      removeNotification(id = paste0("F",gen))
    }
    #Create plot
    gplot <- ggplot(data=df, aes(x=X0, y=driveFreq))+
      geom_point()+
      xlab("Generation")+ylab("Gene drive frequency")+
      ylim(0,1)+
      scale_x_continuous(breaks=seq(0, nrow(df), 1))+
      theme_bw()+
      theme(axis.title = element_text(size=14),
            axis.text = element_text(size=12))+
      if(input$drawline == "logistic"){
        if(input$se1){
          stat_smooth(method="glm", method.args=list(family="binomial"), se=T)
        }else {
          stat_smooth(method="glm", method.args=list(family="binomial"), se=F)
        }
      } else if(input$drawline == "linear"){
        if(input$se2){
          stat_smooth(method = "lm", se=T)
        }else {
          stat_smooth(method = "lm", se=F)
        }
      }
    #change table column names
    names(df) <- c("Gen.", "Freq.")
    df$Gen. <- as.integer(df$Gen.)
    list(df = df, gplot = gplot)
  })
  output$plot1 <- renderPlot({
    data()$gplot
  })
  output$tbl <- renderTable({
    data()$df
  })
}

shinyApp(ui = ui, server = server)
  

  
  