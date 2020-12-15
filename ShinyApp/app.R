## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(tidyquant)

# import the data
data <- read.csv('all_stock_data.csv')
data$date <- as.Date(data$date)
Sys.setlocale("LC_TIME", "English")
before <- data %>% filter(date <= '2020-06-30') #historical data
after <- data %>% filter(date >= '2020-07-01') #validation_data

# UI 
header <- dashboardHeader(title = "Investment Helper")
sider <- dashboardSidebar(
    sidebarMenu(
        #tabName should not include space
        menuItem("Visualization",icon = icon("bar-chart-o"),startExpanded = TRUE,
                 menuSubItem("Stock Price", tabName = "HistoricalPrice"),
                 menuSubItem("Trading Volume", tabName = "Volumn")
                 ),
        menuItem("Price Forcast", tabName = "PriceForcast", icon = icon("calendar")),
        menuItem("Portfolio Management", icon = icon("cog"), startExpanded = TRUE,
                 menuSubItem("Quantitative Trading", tabName = "QuantitativeTrading"),
                 menuSubItem("Manual Trading", tabName = "ManualTrading")
                 ),
        menuItem('Data Table', tabName = 'Table', icon = icon('book'))
        )
    )
    
body <- dashboardBody(
    tabItems(
        # 1st sub 1
        tabItem(tabName = "HistoricalPrice",
                fluidRow(
                    column(width = 8,
                           plotOutput('plot1')
                           ),
                    
                    column(width = 4,
                           box(width = NULL, status = "warning",
                               selectInput('plot_symbol','Stocks:',data$symbol,
                                           selected = 'AAPL', multiple = TRUE),
                               dateRangeInput("daterange", "Date Range:",
                                              start  = "2019-01-01",
                                              end    = "2020-07-01",
                                              min    = "2019-01-01",
                                              max    = "2020-07-01",
                                              format = "yyyy/mm/dd/",
                                              separator = " - ")#,
                               #actionButton(inputId = "go", label = "Go!")
                           )
                    )
                ),
                br(),
                fluidRow(column(width = 8,plotOutput('plot2')))
        ),
        
        #1st sub 2
        tabItem(tabName = "Volumn",
                fluidRow(
                    column(width = 8,
                           plotOutput('plot3')
                    ),
                    
                    column(width = 4,
                           box(width = NULL, status = "warning",
                               selectInput('plot3_symbol','Stocks:',data$symbol,
                                            selected = 'FB',multiple = TRUE),
                               dateRangeInput("daterange3", "Date Range:",
                                              start  = "2019-01-01",
                                              end    = "2020-07-01",
                                              min    = "2019-01-01",
                                              max    = "2020-07-01",
                                              format = "yyyy/mm/dd/",
                                              separator = " - ")#,
                               #actionButton(inputId = "go", label = "Go!")
                           )
                    )
                )
        ),
        
        # 2nd tab content
        tabItem(tabName = "PriceForcast",
                h2("Widgets tab content")
        ),
        
        # 3rd subtab 1 content
        tabItem(tabName = "QuantitativeTrading",
                fluidRow(
                    box(
                        title = "$250,000", width = 4, background = "light-blue",
                        "Initial position in cash"
                    ),
                    
                    box(
                        title = "CAPM Model", width = 4, background = "light-blue",
                        "How to choose the stocks?"
                    ),
                    
                    box(
                        title = "Maximize SharpeRatio", width = 4, background = "light-blue",
                        "How to decide portfolio weights?"
                    )#,
                    
                    #h5("Performance Analysis")
                    
                ),
                
                fluidRow(
                    
                    
                    h5("Performance Analysis")
                    
                )

        ),
        
        # 3rd subtab 2 content
        tabItem(tabName = "ManualTrading",
                fluidRow(
                    box(
                        title = "$250,000", width = 4, background = "light-blue",
                        "Initial position in cash"
                    ),
                    
                    box(
                        title = "CAPM Model", width = 4, background = "light-blue",
                        "How to choose the stocks?"
                    ),
                    
                    box(
                        title = "Maximize SharpeRatio", width = 4, background = "light-blue",
                        "How to decide portfolio weights?"
                    ),
                    h2("Widgets tab content")
                    
                )
        ),
        
        # 4th tab content
        tabItem(tabName = "Table",
                collapsible = FALSE,
                solidHeader = TRUE,
                width = 12,
                selectInput('table_symbol','Stock Name',data$symbol,
                            selected = 'AAPL', multiple = TRUE),
                DT::dataTableOutput("table"),
                h4("Data Source: Yahoo Finance - Daily stock data")
        )
        
    )
    )
        
# Put together
ui <- dashboardPage(
    header,
    sider,
    body
)

#server
server <- function(input, output) {
    #eventReactive(input$go,{})
    plot_one = reactive({
        ggplot(data[data$symbol %in% input$plot_symbol,], 
                          aes(x = date, y = adjusted, color = symbol)) + 
            geom_line()+
            coord_x_date(xlim = c(format(input$daterange[1]), format(input$daterange[2]))) + 
            xlab('Time') + ylab('Adjusted Price ($)') +
            labs(title = "Historical Stock Prices Visualization") +
            #theme(plot.title = element_text(hjust = 0.5, size = 40)) +
            theme_tq() + scale_color_tq() 
        #ggpubr::ggarrange(p1_call,p2_call,p3_call,ncol = 1)
    })
    output$plot1 = renderPlot(plot_one())
    
    plot_two = reactive({
        ggplot(data[data$symbol %in% input$plot_symbol,],
               aes(x = date, y = close)) +
            geom_candlestick(aes(open = open, high = high,low = low, close = close),
                             colour_up = "darkblue",colour_down = "red",
                             fill_up = "darkblue",fill_down = "red") +
            coord_x_date(xlim = c(format(input$daterange[1]), format(input$daterange[2]))) + 
            xlab('Time') + ylab('Prices / ($)') +
            labs(title = "Candlestick Plot of Historical Stock Prices") +
            #theme(plot.title = element_text(hjust = 0.5, size = 60)) +
            theme_tq() + scale_color_tq() 
    })
    output$plot2 = renderPlot(plot_two())
    
    
    plot_three = reactive({
        ggplot(data[data$symbol %in% input$plot3_symbol,],
               aes(x = date, y = volume/1000000, fill = volume/1000000)) +
            geom_bar(stat = "identity") + 
            #geom_smooth(method = "loess", se = FALSE) +
            scale_fill_gradient(low = "red", high = "darkblue") +
            #theme(legend.position = "none") +
            coord_x_date(xlim = c(format(input$daterange3[1]), format(input$daterange3[2]))) + 
            xlab('Time') + ylab('Volume / (million)') +
            labs(title = "Historical Stock Prices Visualization") +
            labs(fill = 'Volume / (million)') +
            #theme(plot.title = element_text(hjust = 0.5, size = 40)) +
            theme_tq() + scale_color_tq() 
    })
    output$plot3 = renderPlot(plot_three())
    
    output$table <- DT::renderDataTable({
        table_data <- data %>% filter(symbol %in% input$table_symbol)
        DT::datatable(table_data, options = list(scrollX =TRUE, pageLength = 20))
    })
}


# Run the application 
shinyApp(ui, server)

