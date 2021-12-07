rm(list = ls())

library(purrr)

boards <- read.csv('C:/Users/van de Put/Documents/AdventOfCode2021/InputDay4_boards.txt', 
                   sep = "", header = FALSE, strip.white = TRUE, blank.lines.skip = TRUE)

numbersDrawn <- as.integer(read.table('C:/Users/van de Put/Documents/AdventOfCode2021/InputDay4_sequence.txt', 
                                      sep = ",", header = FALSE))

boardSizeHorizontal <- 5
boardSizeVertical <- 5

nBoards <- length(numbersDrawn)

scoreBoard <- matrix(0, nrow = nrow(boards), ncol = ncol(boards))


isBingo <- function(scoreIndicatorCard){
  any(colSums(scoreIndicatorCard) == 5) || any(rowSums(scoreIndicatorCard) == 5)
}

bingoNumbers <- integer(100)

determineWhenBingo <- function(bingoBoards, nBoards, indicatorScoreBoard, allNumbersDrawn){
    
   browser()
    
    for (board in 1:nBoards){
      
      for (n in 1:length(allNumbersDrawn)){
        currentNumber <- allNumbersDrawn[n]
        
        for (index in 1:ncol(bingoBoards)){
          indexNumber <- which(bingoBoards[index, ] == currentNumber)
          
          if (length(indexNumber) >= 1) {
            indicatorScoreBoard[board * index, indexNumber] <-  1
          }
        }
        
        if(isBingo(scoreBoard[(board * 1) : (board * 5), ])){
          bingoNumbers[board] <- n # save index of drawn numbers to determine when board had bingo
          
          break;
        }
      }
    }
    
  #browser()
    return(bingoNumbers)
}

testboards <- boards[1:10, ]
testScoreBoard <- scoreBoard[1:10, ]

resultingNumbers <- determineWhenBingo(testboards, 2, testScoreBoard, numbersDrawn)


bingoNumbers

#which min gebruiken voor bepalen laatste bingonummer

determineTotalScore <- function(ballNumber, board){
  
}