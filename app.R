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
library(formattable)

## Only run this example in interactive R sessions

# Basic dashboard page template
library(shiny)
shinyApp(
  
  ui = dashboardPage(    
    dashboardHeader(title = 'tidyquant'),
    dashboardSidebar(collapsed = FALSE,
                     actionButton("run", "Run Me"),
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
            "Plots",
            conditionalPanel(
              # DATASET VISUALIZATION
              condition = "input.data_type == 'Stock Prices'",
              uiOutput("ggplot_stock_controls"),
              # In ui.R:
              plotlyOutput('stock_plot_ts'),
              uiOutput("ggplot_scatter_controls"),
              plotlyOutput('stock_plot_scatter')
            )
          ),
          tabPanel(
            "Dataset",
            conditionalPanel(
              # DATASET VISUALIZATION
              condition = "input.data_type == 'Stock Prices'",
              # In ui.R:
              downloadLink('downloadData', 'Download'),
              dataTableOutput('dataset')
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
      input$run
      
      isolate({
        query = input$tickers %>% 
          str_split(.,",") %>% 
          unlist %>% 
          str_trim %>% 
          as.vector
        
        tq_get_output <- 
          tq_get(
            query, 
            get   = 'stock.prices',
            from  = min(input$daterange_tickers),
            to    = max(input$daterange_tickers)
          ) %>% 
          mutate(
            date = ymd(date),
            change = (close-lag(close))/lag(close)
          )
        
        
        
        if(length(query) == 1) {
          tq_get_output$symbol = query
        }
        
        tq_get_output 
      })

    })
    
    output$ggplot_stock_controls <- renderUI({

      tickers_for_graph <-
      stock_data()$symbol %>% 
        unique

      tagList(
        selectizeInput('chosen_tickers_for_plot',
                       'Choose two tickers from the tickers you submitted.',
                       tickers_for_graph,
                       tickers_for_graph,
                       multiple = TRUE)
      )

    })
    
    output$stock_plot_ts = renderPlotly({
      
      stock_plot_data <- 
      stock_data() %>% 
        filter(symbol %in% input$chosen_tickers_for_plot) %>% 
        ggplot() +
        aes(x = date, y = adjusted, colour = symbol) +
        geom_line() +
        xlab("Date") +
        ylab("Adjusted Closing Price") +
        guides(colour=guide_legend(title="Tickers")) 
      
        ggplotly(stock_plot_data)
    })
    
    output$ggplot_scatter_controls <- renderUI({
      
      tickers_for_graph <-
        stock_data()$symbol %>% 
        unique
      
      tagList(
        selectizeInput('chosen_tickers_for_scatter',
                       'Remove/Add Stocks',
                       tickers_for_graph,
                       tickers_for_graph[c(1,2)],
                       multiple = TRUE)
      )
      
    })
    
    output$stock_plot_scatter = renderPlotly({
      
      data_plot = stock_data() 

      x <- 
        data_plot %>% 
        filter(
          symbol == input$chosen_tickers_for_scatter[1]
        )
      y <- 
        data_plot %>% 
        filter(
          symbol == input$chosen_tickers_for_scatter[2]
        )
      
      x = x %>% 
        filter(
          date %in% y$date
        )
      
      y = y %>% 
        filter(
          date %in% x$date
        )
      


      stock_plot_data <- 
        ggplot() +
        aes(x = x$change,
            y = y$change) +
        geom_point() +
        geom_smooth(method = lm) +
        xlab("Date") +
        ylab("Adjusted Closing Price") +
        guides(colour=guide_legend(title="Tickers")) +
        xlim(-.1, .1) +
        ylim(-.1, .1) +
        xlab(input$chosen_tickers_for_scatter[1]) +
        ylab(input$chosen_tickers_for_scatter[2]) +
        ggtitle(
          paste0("Correlation = ",
                 cor(x$change,
                     y$change,
                     use = "complete.obs") %>% 
                   percent)
          )
        
      
      ggplotly(stock_plot_data, height = 800)
    })
    

    output$dataset = renderDataTable({
      stock_data() 
    })
    
    ############## STOCK DATA ENDING
    
    
    ## DOWNLOAD DATA LOCATION
    output$downloadData <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(stock_data(), con)
      }
    )
  }
  
)

