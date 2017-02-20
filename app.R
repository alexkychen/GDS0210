library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Gene drive simulator"),
  #Input
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "popsize",label = "Wild population size (individual)", 100),
      numericInput(inputId = "release",label = "Released gene drive (individual)", 20),
      selectInput(inputId = "drawline", label = "Draw regression", choices = c("None"="none","Logistic regression"="logistic","Linear regression"="linear")),
      actionButton("go", label="Simulate")
  
    ),
    mainPanel(
      #Output
      plotOutput(outputId = "plot1")
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
          "The ratio of wild population to gene drive is large. Simulation will take a few minutes or longer. Please be patient!", easyClose=T
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
    ggplot(data=df, aes(x=X0, y=driveFreq))+
      geom_point()+
      ggtitle("Gene Drive Frequency Changes across Generations")+
      xlab("Generation")+ylab("Gene drive frequency")+
      ylim(0,1)+
      scale_x_continuous(breaks=seq(0, nrow(df), 1))+
      theme_bw()+
      theme(axis.title = element_text(size=14),
            axis.text = element_text(size=12))+
      if(input$drawline == "logistic"){
        stat_smooth(method="glm", method.args=list(family="binomial"), se=F)
      } else if(input$drawline == "linear"){
        stat_smooth(method = "lm", se = F)
      }
    
  })
  output$plot1 <- renderPlot({
    data()
  })
}

shinyApp(ui = ui, server = server)
  

  
  