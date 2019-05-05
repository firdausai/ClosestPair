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
    
    #convert to dataframe
    rawData <- data()
    sortedX <- rawData[order(rawData$X),]
    middleLine <- average(sortedX,"X")
    
    LPoints <- filter(sortedX, X < middleLine)
    RPoints <- filter(sortedX, X > middleLine)
    
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
    
    graph <- ggplot(data = rawData, aes(x = X, y = Y)) +
      geom_point() +
      geom_vline(xintercept = middleLine)
    
    if(shortestLeftDistance < shortestRightDistance) {
      graph <- graph + geom_point(aes(x = shortestLeft$X[[1]],y = shortestLeft$Y[[1]]),color = "red")
      graph <- graph + geom_point(aes(x = shortestLeft$X[[2]],y = shortestLeft$Y[[2]]),color = "red")
    } else {
      graph <- graph + geom_point(aes(x = shortestRight$X[[1]],y = shortestRight$Y[[1]]),color = "red")
      graph <- graph + geom_point(aes(x = shortestRight$X[[2]],y = shortestRight$Y[[2]]),color = "red")
    }
    graph
    
    output$text <- renderText({
      paste("Shortest Distance :", finalSmallestDistance)
    })
    
    output$Point1 <- renderText({
      paste("Point 1 :",coordinates$X[[1]],",",coordinates$Y[[1]])
    })
    
    output$Point2 <- renderText({
      paste("Point 2 :",coordinates$X[[2]],",",coordinates$Y[[2]])
    })
  })
}