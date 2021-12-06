rm(list = ls())

boards <- read.csv('C:/Users/van de Put/Documents/AdventOfCode2021/InputDay4_boards.txt', 
                   sep = "", header = FALSE, strip.white = TRUE, blank.lines.skip = TRUE)

numbersDrawn <- as.integer(read.table('C:/Users/van de Put/Documents/AdventOfCode2021/InputDay4_sequence.txt', 
                                      sep = ",", header = FALSE))

boardSizeHorizontal <- 5
boardSizeVertical <- 5

nBoards <- length(numbersDrawn) #coincidentally also the total amount of bingo cards

scoreBoard <- matrix(0, nrow = nrow(boards), ncol = ncol(boards))


isBingo <- function(scoreIndicatorCard){
  any(colSums(scoreIndicatorCard) == 5) || any(rowSums(scoreIndicatorCard) == 5)
}

bingoNumbers <- integer(100)

for (i in 1:nBoards){
  
  browser()
  
  saveNumber <- 0
  
  while(!isBingo(scoreBoard[1*i : 5*i, ])){
    
    for (j in 1:boardSizeHorizontal){
      currentNumber <- numbersDrawn[i]
      
      indexNumber <- which(boards[1*i : 5*1, ] == currentNumber)
      
      if (length(indexNumber == 1)){
        scoreBoard[1*i : 5*1, indexNumber] <- 1
      }
    }
    
    saveNumber <- currentNumber
  }
  
  bingoNumbers[i] <- saveNumber
}

#which min gebruiken voor bepalen laatste bingonummer

determineTotalScore <- function(ballNumber, board){
  
}