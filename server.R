library(shiny)
library(tidyverse)
library(ggplot2)
# Define server logic ----
server <- function(input, output) {
  
  data <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    read.csv(inFile$datapath,
             sep = input$sep)
  })
  
  output$text <- renderText({
    paste("Shortest Distance :")
  })
  
  output$Point1 <- renderText({
    paste("Point 1 :")
  })
  
  output$Point2 <- renderText({
    paste("Point 2 :")
  })
  
  output$scatterPlot <- renderPlot({
    
    #Check if data exist
    validate(need(input$file1,"Need Data!"))
    
    #Convert to dataframe
    rawData <- data()
    
    #Remove NULL values
    rawData <- na.omit(rawData)
    
    #Sort data based on x values ascending
    sortedX <- rawData[order(rawData$X),]
    
    #create middle line based on average X values
    middleLine <- average(sortedX,"X")
    
    #plot graph
    graph <- ggplot(data = rawData, aes(x = X, y = Y)) +
      geom_point() +
      geom_vline(xintercept = middleLine, linetype = "dotted")
    
    #create data frame for values on the left and right side of the middle line
    LPoints <- filter(sortedX, X < middleLine)
    RPoints <- filter(sortedX, X > middleLine)
    
    
    if((nrow(LPoints) == 1) && (nrow(RPoints == 1))) { #one coordinate on each side
      finalSmallestDistance <- calculateDistance(LPoints$X[[1]],RPoints$X[[1]],LPoints$Y[[1]],RPoints$Y[[1]])
      coordinates <- data_frame("Distance" = c(finalSmallestDistance,finalSmallestDistance), "X" = c(LPoints$X[[1]],RPoints$X[[1]]), "Y" = c(LPoints$Y[[1]],RPoints$Y[[1]]))
    } else if (nrow(LPoints) == 1) { #one coordinate on the left side
      shortestRight <- bruteForce(RPoints)
      shortestRightDistance <- shortestRight$Distance[[1]]
      minDistance <- shortestRightDistance
      coordinates <- shortestRight
    } else if (nrow(RPoints) == 1) { #one coordinate on the right side
      shortestLeft <- bruteForce(LPoints)
      shortestLeftDistance <- shortestLeft$Distance[[1]]
      minDistance <- shortestLeftDistance
      coordinates <- shortestLeft
    } else { #else
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
      #Collect points within shortest distance in relative to the middle
      MPoints <- filter(sortedX, X < (middleLine+minDistance))
      MPoints <- filter(MPoints, X > (middleLine-minDistance))
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
    
    graph <- graph + geom_point(aes(x = coordinates$X[[1]],y = coordinates$Y[[1]]),color = "red")
    graph <- graph + geom_point(aes(x = coordinates$X[[2]],y = coordinates$Y[[2]]),color = "red")
    
    output$text <- renderText({
      paste("Shortest Distance :", finalSmallestDistance)
    })
    
    output$Point1 <- renderText({
      paste("Point 1 :",coordinates$X[[1]],",",coordinates$Y[[1]])
    })
    
    output$Point2 <- renderText({
      paste("Point 2 :",coordinates$X[[2]],",",coordinates$Y[[2]])
    })
    
    graph
  })
}