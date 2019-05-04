library(tidyverse)
library(ggplot2)

source(functions.R)

#Read Data
rawData <- read_csv2("C:\\Users\\Firdaus\\Documents\\ClosestPair\\data-raw\\kordinat.csv")
sortedX <- rawData[order(rawData$X),]
sortedY <- rawData[order(rawData$Y),]

#Display initial data
graph <- ggplot(data = rawData, aes(x = X, y = Y)) +
  geom_point()

#Determine Middle Line
middleLine <- average(sortedX,"X")

#Add middle line to graph
graph <- graph + geom_vline(xintercept = middleLine)
graph

#Sort points based on their location (to the left & right of middle line)
LPoints <- filter(sortedX, X < middleLine)
RPoints <- filter(sortedX, X > middleLine)

#Calculate shortest distance left & right side
shortestLeft <- bruteForce(LPoints)
shortestLeftDistance <- shortestLeft$Distance[[1]]
shortestRight <- bruteForce(RPoints)
shortestRightDistance <- shortestRight$Distance[[1]]

#Determine which side has the shortest distance between points
minDistance <- compareMinValue(shortestLeftDistance,shortestRightDistance)

#Collect points within shortest distance range from the middle
  #Has to be able to compare like value1 < x < value2
  #check if filter() has parameters like above
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
  finalSmallestDistance <- compareMinValue(minDistance,shortestMiddleDistance)
  finalSmallestDistance
} else {
  finalSmallestDistance <- minDistance
  minDistance
}

if(shortestLeftDistance < shortestRightDistance) {
  graph <- graph + geom_point(aes(x = shortestLeft$X[[1]],y = shortestLeft$Y[[1]]),color = "red")
  graph <- graph + geom_point(aes(x = shortestLeft$X[[2]],y = shortestLeft$Y[[2]]),color = "red")
  graph
} else {
  graph <- graph + geom_point(aes(x = shortestRight$X[[1]],y = shortestRight$Y[[1]]),color = "red")
  graph <- graph + geom_point(aes(x = shortestRight$X[[2]],y = shortestRight$Y[[2]]),color = "red")
  graph
}
