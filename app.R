library(shiny)
library(ggplot2)

#Define global functions

#1.Basic gene drive reproduction
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

#2.Check wild to drive ratio in basic model/Pop out warning message
checkRatioBasic <- function(wild=NULL, drive=NULL){
  Wild <- round(wild / drive)
  if(Wild >= 90){
    showModal(modalDialog(
      title="Important message", "The ratio of wild population to gene drive is large. Simulation might take a few minutes or longer. Please be patient!", easyClose=T))
  }
}

#3.Simulate basic gene drive freq.
simulateBasicDrive <- function(wild=NULL, drive=NULL){
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
  names(df) <- c("Gen.", "Freq.")
  df$Gen. <- as.integer(df$Gen.)
  return(df)
}



### create UI
ui <- fluidPage(
  titlePanel("Gene drive simulator"),
  helpText("Simulate how many generations a gene drive construct would take to spread through a population. The basic model assumes: 1) Male and female contributions are equal, 2) species is semelparous, and 3) mortality or survival is equal between wild and gene-drive individuals."),
  #Input
  fluidRow(
    column(4,
      sidebarPanel( width = 14,
        tabsetPanel(
          tabPanel(title = "Basic",
            helpText("Enter a population size and a number of gene-drive individuals to be released at F0."),
            numericInput(inputId = "popsize",label = "Wild population size (individual)", 100),
            numericInput(inputId = "release",label = "F0 gene drive (individual)", 20),
            verbatimTextOutput(outputId = "text2"),
            helpText("*Ratio greater than 125:1 may fail to simulate due to timeout."),
            selectInput(inputId = "drawline", label = "Draw regression", choices = c("None"="none","Logistic regression"="logistic","Linear regression"="linear")),
            conditionalPanel(condition = "input.drawline == 'logistic'",
                             checkboxInput(inputId = "se1", label = "Include 95% confidence interval")),
            conditionalPanel(condition = "input.drawline == 'linear'",
                            checkboxInput(inputId = "se2", label = "Include 95% confidence interval")),
            actionButton("go", label="Simulate"),
            helpText("*Computing time will increase with an increasing ratio of population size to F0 gene-drive individuals.")
          ),
          tabPanel(title = "Advanced",
            helpText("This section is under construction..."),
            numericInput(inputId = "popsizeAd",label = "Wild population size (individual)", 100),
            radioButtons(inputId = "sexratio", label = "Sex ratio in wild population", choices = c("Female:Male = 1:1"="F=M","Female > Male"="F>M", "Male > Female"="M>F")),
            conditionalPanel(condition = "input.sexratio == 'F>M'",
                             sliderInput(inputId = "FmM", label = "Male:Female =", min=0.1, max=1, value=0.5)),
            conditionalPanel(condition = "input.sexratio == 'M>F'",
                             sliderInput(inputId = "MmF", label = "Female:Male =", min=0.1, max=1, value=0.5)),
            numericInput(inputId = "releaseM",label = "Male F0 gene drive (individual)", 20),
            numericInput(inputId = "releaseF",label = "Female F0 gene drive (individual)", 20),
            selectInput(inputId = "reprod", label = "Reproductive mode", choices = c("Semelparity"="semelparous","Iteroparity"="iteroparous")),
            conditionalPanel(condition = "input.reprod == 'iteroparous'",
                             sliderInput(inputId = "lifespan", label = "Life span (yr)", min=1, max=50, value=10)),
            sliderInput(inputId = "agematurM", label = "Male maturity age (yr)", min=1, max=10, value=3),
            sliderInput(inputId = "agematurF", label = "Female maturity age (yr)", min=1, max=10, value=3),
            selectInput(inputId = "drawlineAd", label = "Draw regression", choices = c("None"="none","Logistic regression"="logistic","Linear regression"="linear")),
            conditionalPanel(condition = "input.drawlineAd == 'logistic'",
                             checkboxInput(inputId = "se3", label = "Include 95% confidence interval")),
            conditionalPanel(condition = "input.drawlineAd == 'linear'",
                             checkboxInput(inputId = "se4", label = "Include 95% confidence interval")),
            actionButton("go2", label="Simulate"),
            helpText("*Computing time will increase with an increasing ratio of population size to F0 gene-drive individuals.")
          )
        )
      ),
      p("The source code of this page can be found on Github via the ", a(href="https://github.com/alexkychen/GDS0210/blob/master/app.R", "link here"),".")
    ),
    column(6,
      #Output plot
      plotOutput(outputId = "plot1"),
      textOutput(outputId = "text1")
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
    #Check if input ratio (wild:drive) greater than 90. If so, pop out warning message 
    checkRatioBasic(wild = wild, drive = drive)

    #Simulate gene drive frequency and create a data frame to save results
    df <- simulateBasicDrive(wild = wild, drive = drive)

    #Create plot
    gplot <- ggplot(data=df, aes(x=Gen., y=Freq.))+
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
    list(df = df, gplot = gplot)
  })
  #Output some text message when simulate button on Advanced tab was hit
  text <- eventReactive(input$go2, {
    showModal(modalDialog(
      title="Simulation not available ",
      "Sorry, we are still working on the Advanced gene drive model.", easyClose=T
    ))
  })
  
  #render outputs
  output$plot1 <- renderPlot({
    data()$gplot
  })
  output$tbl <- renderTable({
    data()$df
  })
  output$text1 <- renderText({
    text()
  })
  output$text2 <- renderText({
    wildperdrive <- round(input$popsize / input$release)
    paste0("Wild:Drive = ",wildperdrive,":1")
  })
}

shinyApp(ui = ui, server = server)
  

  
  