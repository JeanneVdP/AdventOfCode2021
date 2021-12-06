# advent of code Day 1
# Jeanne van de Put

rm(list=ls())

fileDay1 <- 'C:/Users/van de Put/Documents/AdventOfCode2021/InputDay1.txt'
depthMeasurements <- strtoi(readLines(fileDay1))

##########
# part 1 # 
##########
determineDepth <- function(x){
  dataSize <- length(x)
  
  measurementsLargerIndicator <- integer(0)
  
  for (m in 1:dataSize){
    
    if(m == 1){
      measurementsLargerIndicator <- 0
    }
    
    else{
      isNextMeasurementLarger <- x[m] > x[m-1]
      measurementsLargerIndicator <- c(measurementsLargerIndicator, as.integer(isNextMeasurementLarger))
    }
    
  }
  
  return(measurementsLargerIndicator)
}

results <- determineDepth(depthMeasurements)
sum(results, na.rm = TRUE)

##########
# part 2 #
##########

#W.I.P!!

threeWindowMeasurementMatrix <- matrix(0, nrow = length(depthMeasurements), ncol = 4)

fillThreeWindowMatrix <- function(winMat){
  for (index in 1:nrow(winMat)){
    if (index %% 4 = 0){
      winMat[i,] <- c()
    }
  }
}

#TIP: %*%!!!

threeWindowMeasurements <- depthMeasurements %*% threeWindowMeasurementMatrix 

indexesToExclude <- which(rowsum(threeWindowMeasurementMatrix) != 3)