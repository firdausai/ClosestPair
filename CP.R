library(tidyverse)
library(ggplot2)

average <- function(table,row) {
  return((sum(table[[row]]))/nrow(table))
}

calculateDistance <- function(x1,x2,y1,y2) {
  return(sqrt(((x1-x2)^2)+((y1-y2)^2)))
}

bruteForce <- function(table) {
  i = 2
  n = i + 1
  min = calculateDistance(table[1,1],table[2,1],table[1,2],table[2,2])
  while(i < nrow(table)) {
    n = i + 1
    while(n < nrow(table)) {
      x1 <- table[i,1]
      x2 <- table[n,1]
      y1 <- table[i,2]
      y2 <- table[n,2]
      comparedValue <- calculateDistance(x1,x2,y1,y2)
      if(min > comparedValue) {
        min <- comparedValue
      }
      n = n + 1
    }
    i = i + 1
  }
  return(min)
}

compareMinValue <- function(val1,val2) {
  if(val1 <= val2) {
    return(val1)
  } else {
    return(val2)
  }
}


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
shortestLeft <- shortestLeft$X[[1]]
shortestRight <- bruteForce(RPoints)
shortestRight <- shortestRight$X[[1]]

#Determine which side has the shortest distance between points
minDistance <- compareMinValue(shortestLeft,shortestRight)

#Collect points within shortest distance range from the middle
  #Has to be able to compare like value1 < x < value2
  #check if filter() has parameters like above
MPoints <- filter(sortedX, X < (middleLine+minDistance))
MPoints <- filter(MPoints, X > (middleLine-minDistance))

#Calculate shortest distance from the middle
if (nrow(MPoints) != 1) {
  shortestMiddle <- bruteForce(MPoints)
  shortestMiddle <- shortestMiddle$X[[1]]
}


#Determine shortest distance (Compare shortest distance from the middle and left or right)
if (nrow(MPoints) != 1) {
  finalSmallestDistance <- compareMinValue(minDistance,shortestMiddle)
  finalSmallestDistance
} else {
  finalSmallestDistance <- minDistance
  minDistance
}