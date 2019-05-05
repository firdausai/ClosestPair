library(tidyverse)
library(ggplot2)

source(functions.R)

#Read Data
rawData <- read_csv2("C:\\Users\\Firdaus\\Documents\\ClosestPair\\data-raw\\kordinat.csv")
sortedX <- rawData[order(rawData$X),]
middleLine <- average(sortedX,"X")

LPoints <- filter(sortedX, X < middleLine)
RPoints <- filter(sortedX, X > middleLine)

if((nrow(LPoints) == 1) && (nrow(RPoints == 1))) {
  finalSmallerDistance <- calculateDistance(LPoints$X[[1]],RPoints$X[[2]],LPoints$Y[[1]],RPoints$Y[[2]])
  coordinates <- data_frame("Distance" = c(distance,distance), "X" = c(LPoints$X[[1]],RPoints$X[[2]]), "Y" = c(LPoints$Y[[1]],RPoints$Y[[2]]))
  #SKIP LANGSUNG KE PEMETAAN GRAPH
} else if (nrow(LPoints) == 1) {
  shortestRight <- bruteForce(RPoints)
  shortestRightDistance <- shortestRight$Distance[[1]]
  minDistance <- shortestRightDistance
  coordinates <- shortestRight
} else if (nrow(RPoints) == 1) {
  shortestLeft <- bruteForce(LPoints)
  shortestLeftDistance <- shortestLeft$Distance[[1]]
  minDistance <- shortestLeftDistance
  coordinates <- shortestLeft
} else {
  #Calculate shortest distance left & right side
  shortestLeft <- bruteForce(LPoints)
  shortestLeftDistance <- shortestLeft$Distance[[1]]
  shortestRight <- bruteForce(RPoints)
  shortestRightDistance <- shortestRight$Distance[[1]]
  
  #Determine which side has the shortest distance between points
  if(shortestLeftDistance < shortestRightDistance) {
    minDistance <- shortestLeftDistance
    coordinates <- shortestLeft
  } else {
    minDistance <- shortestRightDistance
    coordinates <- shortestRight
  }
}

if (!((nrow(LPoints) == 1) && (nrow(RPoints == 1)))) {
  #Collect points within shortest distance range from the middle
  MPoints <- filter(sortedX, X < (middleLine+minDistance))
  MPoints <- filter(MPoints, X > (middleLine-minDistance))
  
  #Calculate shortest distance from the middle
  if (nrow(MPoints) > 1) {
    shortestMiddle <- bruteForce(MPoints)
    shortestMiddleDistance <- shortestMiddle$Distance[[1]]
  } else {
    shortestMiddleDistance <- NULL
  }
  
  #Determine shortest distance (Compare shortest distance from the middle and left or right)
  if (nrow(MPoints) > 1) {
    if(minDistance > shortestMiddleDistance) {
      finalSmallestDistance <- shortestMiddleDistance
      coordinates <- shortestMiddle
    } else {
      finalSmallestDistance <- minDistance
    }
  } else {
    finalSmallestDistance <- minDistance
  }
}

graph <- ggplot(data = rawData, aes(x = X, y = Y)) +
  geom_point() +
  geom_vline(xintercept = middleLine)
graph

if(shortestLeftDistance < shortestRightDistance) {
  graph <- graph + geom_point(aes(x = shortestLeft$X[[1]],y = shortestLeft$Y[[1]]),color = "red")
  graph <- graph + geom_point(aes(x = shortestLeft$X[[2]],y = shortestLeft$Y[[2]]),color = "red")
} else if (shortestLeftDistance > shortestRightDistance){
  graph <- graph + geom_point(aes(x = shortestRight$X[[1]],y = shortestRight$Y[[1]]),color = "red")
  graph <- graph + geom_point(aes(x = shortestRight$X[[2]],y = shortestRight$Y[[2]]),color = "red")
} else {
  graph <- graph + geom_point(aes(x = coordinates$X[[1]],y = coordinates$Y[[1]]),color = "red")
  graph <- graph + geom_point(aes(x = coordinates$X[[2]],y = coordinates$Y[[2]]),color = "red")
}

graph
