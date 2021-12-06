rm(list=ls())

fileDay3 <- 'C:/Users/van de Put/Documents/AdventOfCode2021/InputDay3.txt'
diagnosticReport <- readLines(fileDay3)

nPositions <- nchar(diagnosticReport[1])
nInputs <- length(diagnosticReport)

createBinaryCharacters <- function(x){
  return(unlist(strsplit(diagnosticReport[x], split="")))
}

for(i in 1:nInputs){
  vectorOfChars <- createBinaryCharacters(i)
  
  reportMatrix <- if (i == 1) {vectorOfChars} else rbind(matDing, vectorOfChars)
}

getRates <- function(reportData, nInputs, nPositions){
  cutOff = nInputs / 2
  
  gammaRateBinary <- vector(length = nPositions)
  epsilonRateBinary <- vector(length = nPositions)
  
  for(i in 1:nPositions){
    binaryInput <- as.integer(reportData[,i])
    
    mostCommonBit <- as.integer(sum(binaryInput) > cutOff)
    leastCommonBit <- 1 - mostCommonBit
    
    gammaRateBinary[i] <- mostCommonBit
    epsilonRateBinary[i] <- leastCommonBit
  }
  
  binaryRatesCombined <- rbind(gammaRateBinary, epsilonRateBinary)
  return(binaryRatesCombined)
}

binaryRates <- getRates(reportMatrix, nInputs, nPositions)

getRateFromBinary <- function(binaryRow){
  binRowLength <- length(binaryRow)
  maxPower <- binRowLength - 1
  
  numericValue <- 0
  
  for (i in 0:maxPower){
    numericValue <- numericValue + binaryRow[binRowLength - i] * 2^i
  }
  
  return(numericValue)
}

gammaRate <- getRateFromBinary(binaryRates[1,]) 
epsilonRate <- getRateFromBinary(binaryRates[2,]) 

gammaRate * epsilonRate
