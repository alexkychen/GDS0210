popsize <- 20
release <- 1
NoGen <- 0
F0 <- c(rep("W", popsize*2), rep("G",release*2)) #F0 gametes
F1 <- NULL
mixindex <- sample(length(F0))#create sampling index

driveNextGen <- function(wild=10, drive=1){
  F0 <- c(rep("W", wild*2), rep("G",drive*2)) #F0 gametes
  F1 <- NULL
  mixindex <- sample(length(F0))#create sampling index
  for (i in seq(1, length(mixindex), 2)){ #for loop through mixindex
    pair <- F0[mixindex[c(i, i+1)]] #Get every two as a pair
    if ("W" %in% pair & "G" %in% pair){
      pair <- c("G","G")
      F1 <- c(F1, pair)
    } else{
      F1 <- c(F1, pair)
    }
  }
  return(F1)
} 

#Get gene drive frequency (individuals that carry gene drive in the population)
NoGenSp <- NULL
NoGen <- 0
## get gene drive freq. across 10 generations
popsize <- 80
release <- 1
driveFreq <- release / (release + popsize)
df <- data.frame(0, driveFreq)
gen <- 1

while(!is.na(popsize)){ #keep running the following script until popsize is NA
  gen <- gen + 1
  res <- driveNextGen(wild=popsize, drive=release)
  restb <- table(res)
  popsize <- restb[2]
  release <- restb[1]
  driveFreq <- restb[1]/sum(restb) #restb[1] is restb[names(restb)=="G]
  data <- c(gen, driveFreq)
  df <- rbind(df, data)
}

plot(df$X0, df$driveFreq)





for (gen in seq(1,10)){
  if(is.na(popsize)){
    break
  }
  else if(popsize > 0){
    res <- driveNextGen(wild=popsize, drive=release)
    restb <- table(res)
    popsize <- restb[2]
    release <- restb[1]
    driveFreq <- restb[1]/sum(restb) #restb[1] is restb[names(restb)=="G]
    data <- c(gen, driveFreq)
    df <- rbind(df, data)
  }
}

plot(df$X0, df$driveFreq)


