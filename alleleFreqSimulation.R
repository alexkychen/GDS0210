popsize <- 20
release <- 1
NoGen <- 0
F0 <- c(rep("W", popsize*2), rep("G",release*2)) #F0 gametes
F1 <- NULL
mixindex <- sample(length(F0))#create sampling index

driveNextGen <- function(popsize=10, release=1){
  F0 <- c(rep("W", popsize*2), rep("G",release*2)) #F0 gametes
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
tb <- table(F1)

tb[names(tb)=="G"] / sum(tb)


