library(shiny)
library(tidyverse)
library(ggplot2)

source("functions.R")

# Define UI ----
ui <- fluidPage(
  titlePanel("Closest Pair with Devide and Conquere Method"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      p("*make sure your CSV file only X and Y columns (all capital)"),
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ";"),
      textOutput("text"),
      textOutput("Point1"),
      textOutput("Point2")
    ),
    mainPanel(
      plotOutput("scatterPlot")
      
    )
  )
)