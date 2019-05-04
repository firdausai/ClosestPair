library(shiny)
library(tidyverse)
library(ggplot2)

source(functions.R)

# Define UI ----
ui <- fluidPage(
  titlePanel("Closest Pair with Devide and Conquere Method"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))
    ),
    mainPanel(
      plotOutput("scatterPlot")
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  
  output$scatterPlot <- renderPlot({
    
    data <- reactive({
      inFile <- input$file1
      if (is.null(inFile)) return(NULL)
      read_csv2(inFile$datapath)
    })
    
    #Check if data exist
    validate(need(input$file1,"need filename"))
    
    #convert to dataframe
    rawData <- data()
    sortedX <- rawData[order(rawData$X),]
    middleLine <- average(sortedX,"X")
    
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
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)