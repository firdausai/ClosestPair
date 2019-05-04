library(shiny)


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
    
    
    
    validate(need(input$file1,"need filename"))
    df <- data()
    #plot(df)
    graph <- ggplot(data = df, aes(x = X, y = Y)) +
      geom_point()
    graph
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)