rm(list=ls())

fileDay2 <- 'C:/Users/van de Put/Documents/AdventOfCode2021/InputDay2.txt'
positions <- as.data.frame(read.table(fileDay2, header = FALSE, sep = " "))
colnames(positions) <- c("direction", "distance")

##########
# part 1 #
##########
forward <- positions[positions$direction == "forward", ]$distance
up <-  positions[positions$direction == "up", ]$distance
down <-  positions[positions$direction == "down", ]$distance

horizontalDistance <- sum(forward)
verticalDistance <- sum(down) - sum(up)

endPosition <- horizontalDistance * verticalDistance
endPosition

##########
# part 2 #
##########
positions$horizPosition <- integer(1000)
positions$aim <- integer(1000)
positions$corrDepth <- integer(1000)

# do first row (forward)
positions$horizPosition[1] <- positions$distance[1]

for (index in 2:nrow(positions)){
  if (positions$direction[index] == "forward"){
    positions$horizPosition[index] <- positions$horizPosition[index - 1] + positions$distance[index]
    positions$corrDepth[index] <- (positions$distance[index] * positions$aim[index - 1]) 
                                  + positions$corrDepth[index - 1]
    positions$aim[index] <- positions$aim[index - 1]
  }
  else if (positions$direction[index] == "down"){
    positions$aim[index] <- positions$aim[index - 1] + positions$distance[index]
    positions$horizPosition[index] <- positions$horizPosition[index - 1]
    positions$corrDepth[index] <- positions$corrDepth[index - 1]
  }
  else{
    positions$aim[index] <- positions$aim[index - 1] - positions$distance[index]
    positions$horizPosition[index] <- positions$horizPosition[index - 1]
    positions$corrDepth[index] <- positions$corrDepth[index - 1]
  }
}

# final position
positions$horizPosition[1000] * positions$corrDepth[1000]
