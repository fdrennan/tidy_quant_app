#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidyquant)
library(lubridate)
library(stringr)
library(plotly)

## Only run this example in interactive R sessions

# Basic dashboard page template
library(shiny)
shinyApp(
  
  ui = dashboardPage(    
    dashboardHeader(title = 'tidyquant'),
    dashboardSidebar(collapsed = FALSE,
                     selectInput(
                       "data_type", 
                       "Select Data",
                       c("Stock Prices", 
                         "Stock Prices Japan",
                         "Financials",
                         "Key Stats",
                         "Key Ratios",
                         "Dividends",
                         "Splits",
                         "Economic Data",
                         "Exchange Rates",
                         "Metal Prices",
                         "Quandl",
                         "Quandl Datatable")
                     ),
                     conditionalPanel(
                       # DATASET VISUALIZATION
                       condition = "input.data_type == 'Stock Prices'",
                       textInput("tickers", "Tickers", value = "AAPL, AMZN"),
                       dateRangeInput("daterange_tickers", "Date range:",
                                      start  = Sys.Date()-365*2,
                                      end    = Sys.Date() -1)
                       
                     )

    ),
    dashboardBody(
      fluidRow(
        tabsetPanel(
          tabPanel(
            "Dataset",
            conditionalPanel(
              # DATASET VISUALIZATION
              condition = "input.data_type == 'Stock Prices'",
              # In ui.R:
              downloadLink('downloadData', 'Download'),
              dataTableOutput('dataset')
            )
          ),
          tabPanel(
            "Plots",
            conditionalPanel(
              # DATASET VISUALIZATION
              condition = "input.data_type == 'Stock Prices'",
              # In ui.R:
              plotlyOutput('stock_plot')
            )
          )
        )
      )

    ),
    title = "tidyquant"
    
  ),
  
  server = function(input, output) { 
    
############## STOCK DATA
    stock_data = reactive({
      query = input$tickers %>% 
        str_split(.,",") %>% 
        unlist %>% 
        str_trim %>% 
        as.vector
    
        tq_get(
          query, 
          get   = 'stock.prices',
          from  = min(input$daterange_tickers),
          to    = max(input$daterange_tickers)
        ) 
    
      
    })
    
    output$dataset = renderDataTable({
      stock_data() 
    })
    
    output$stock_plot = renderPlotly({
      stock_plot_data <- 
      stock_data() %>% 
        ggplot() +
        aes(x = date, y = adjusted, colour = symbol) +
        geom_line() +
        xlab("Date") +
        ylab("Adjusted Closing Price") +
        guides(colour=guide_legend(title="Tickers")) 
      
        ggplotly(stock_plot_data)
    })

  
    # In server.R:
    output$downloadData <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(stock_data(), con)
      }
    )
    
    
    ############## STOCK DATA ENDING
  }
  
)

