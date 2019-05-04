average <- function(table,row) {
  return((sum(table[[row]]))/nrow(table))
}

calculateDistance <- function(x1,x2,y1,y2) {
  return(sqrt(((x1-x2)^2)+((y1-y2)^2)))
}

bruteForce <- function(table) {
  i = 2
  n = i + 1
  min = calculateDistance(table$X[[1]],table$X[[2]],table$Y[[1]],table$Y[[2]])
  dataFrame = data_frame("Distance" = c(min,min), "X" = c(table$X[[1]],table$X[[2]]), "Y" = c(table$Y[[1]],table$Y[[2]]))
  while(i < nrow(table)) {
    n = i + 1
    while(n < nrow(table)) {
      x1 <- table$X[[i]]
      x2 <- table$X[[n]]
      y1 <- table$Y[[i]]
      y2 <- table$Y[[n]]
      
      comparedValue <- calculateDistance(x1,x2,y1,y2)
      if(min > comparedValue) {
        min <- comparedValue
        dataFrame = data_frame("Distance" = c(min,min), "X" = c(x1,x2), "Y" = c(y1,y2))
      }
      n = n + 1
    }
    i = i + 1
  }
  return(dataFrame)
}

compareMinValue <- function(val1,val2) {
  if(val1 <= val2) {
    return(val1)
  } else {
    return(val2)
  }
}