#Importing dataset
library(tidyverse)
library(xts)
library(dygraphs)
library(shiny)
library(shinydashboard)
library(httpuv)
crypto <- read_csv("crypto-markets.csv")


#ui
ui <- dashboardPage(
  dashboardHeader(title = "Crypto Currency Pattern", titleWidth= 350),
  dashboardSidebar(
    title = "Select Currency",
                   selectInput('cryptosym', label = NULL, choices= unique(crypto$symbol))),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(dygraphOutput("dygraph", width=900, height = 400))
      
    )
  )
)


#Changing Date to posixct format 
crypto$date <- as.POSIXct(crypto$date)

#converting to xts object using xts package for plotting dygraph
crypto<- as.xts(crypto[,-1], order.by = crypto$date)





#server
server <- function(input, output) {
  #filter based on cryptocurrency
  df <- reactive({crypto[crypto$symbol== input$cryptosym]})
  
  output$dygraph <- renderDygraph({df()[,c("open","high","close","low")] %>%
    dygraph() %>%
    dyCandlestickGroup(c('open', 'high', 'low', 'close')) %>%
    dyCandlestick(compress= FALSE)}) #compress=TRUE to see compressed chart
}

shinyApp(ui, server)
