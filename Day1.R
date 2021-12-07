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

# niet generiek, maar het werkt...
threeWindowMeasurements <- matrix(0, nrow = 2000, ncol = 2)

for (window in 3:1998){
  threeWindowMeasurements[window, 1] <- sum(depthMeasurements[(window-2) : window])
  threeWindowMeasurements[window, 2] <- as.integer(threeWindowMeasurements[window, 1] > threeWindowMeasurements[(window-1), 1])
}

sum(threeWindowMeasurements[,2]) #levert het goede antwoord op


#W.I.P. om het generieker/mooier te maken (lukt nog niet zo goed)

timesLetters <- floor(2000 / 26)
remainingLetterAmount <- 2000 %% 26 

threeWindowLetters <- rep(letters, each = 3, times = timesLetters)
threeWindowLetters <- c(threeWindowLetters, rep(letters[1:remainingLetterAmount], each = 3, times = 1))

#vanaf hier verder gaan: 
threeWindowMeasurementMatrix <- matrix(0, nrow = length(depthMeasurements), ncol = 4)

fillThreeWindowMatrix <- function(winMat){
  for (index in 1:nrow(winMat)){
    if (index %% 4 = 0){
      winMat[i,] <- c()
    }
  }
}

#TIP: do not use matrix multiplication %*%!!!

indexesToExclude <- which(rowsum(threeWindowMeasurementMatrix) != 3)