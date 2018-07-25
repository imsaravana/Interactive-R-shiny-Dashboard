#Importing dataset
library(tidyverse)
library(xts)
library(dygraphs)
library(shiny)
library(shinydashboard)
library(httpuv)
crypto <- read_csv("crypto-markets.csv")
#Changing Date to posixct format 

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



crypto$date <- as.POSIXct(crypto$date)

#converting to xts object
crypto<- as.xts(crypto[,-1], order.by = crypto$date)





#server
server <- function(input, output) {
  #filter based on cryptocurrency
  df <- reactive({crypto[crypto$symbol== input$cryptosym]})
  
  output$dygraph <- renderDygraph({df()[,c("open","high","close","low")] %>%
    dygraph() %>%
    dyCandlestickGroup(c('open', 'high', 'low', 'close')) %>%
    dyCandlestick(compress= TRUE)})
}

shinyApp(ui, server)
