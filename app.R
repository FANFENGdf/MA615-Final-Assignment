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
Baseline_SP500 <- read.csv('IVE.csv')
Baseline_SP500$date <- as.Date(Baseline_SP500$date)
# UI 
header <- dashboardHeader(title = "Investment Helper")
sider <- dashboardSidebar(
    sidebarMenu(
        #tabName should not include space
        menuItem("Visualization",icon = icon("bar-chart-o"),startExpanded = TRUE,
                 menuSubItem("Stock Price", tabName = "HistoricalPrice"),
                 menuSubItem("Trading Volume", tabName = "Volumn")
                 ),
        #menuItem("Price Forcast", tabName = "PriceForcast", icon = icon("calendar")),
        menuItem("Investment Portfolio", icon = icon("cog"), startExpanded = TRUE,
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
                    )
                ),
                
                
                fluidRow(
                    box(width = 3, status = "warning",title = 'Zoom',
                        numericInput("qr1", NULL, 0.1, min = 0, max = 1,step = 0.05)
                    ),
                    
                    box(width = 3, status = "warning",title = 'BioNTech',
                        numericInput("qr2", NULL, 0.2, min = 0, max = 1,step = 0.05)
                    ),
                    
                    box(width = 3, status = "warning",title = 'Modrna',
                        numericInput("qr3", NULL, 0.6, min = 0, max = 1,step = 0.05)
                    ),
                    
                    box(width = 3, status = "warning",title = 'NVDA',
                        numericInput("qr4", NULL, 0.1, min = 0, max = 1,step = 0.05)
                    )
                ),
                
                fluidRow(
                    column(width = 6,
                        plotOutput('qplot1')
                    ),
                    
                    column(width = 6,
                        plotOutput('qplot2')
                    )
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
                        title = "Manual", width = 4, background = "light-blue",
                        "How to choose the stocks?"
                    ),
                    
                    box(
                        title = "Manual", width = 4, background = "light-blue",
                        "How to decide portfolio weights?"
                    )
                ),
                
                fluidRow(
                    box(width = 3, status = "warning",
                        selectInput('manual_one','Stock 1:',choices = data$symbol,
                                    selected = 'AMZN',multiple = TRUE),
                        numericInput("mr1", NULL, 0.2, min = 0, max = 1,step = 0.05)
                    ),
                    
                    box(width = 3, status = "warning",
                        selectizeInput('manual_two','Stock 2:',choices = data$symbol,
                                       selected = 'ZM',multiple = TRUE),
                        numericInput("mr2", NULL, 0.2, min = 0, max = 1,step = 0.05)
                    ),
                    
                    box(width = 3, status = "warning",
                        selectInput('manual_three','Stock 3:',choices = data$symbol,
                                    selected = 'PFE',multiple = TRUE),
                        numericInput("mr3", NULL, 0.3, min = 0, max = 1,step = 0.05)
                    ),
                    
                    box(width = 3, status = "warning",
                        selectInput('manual_four','Stock 4:',choices = data$symbol,
                                    selected = 'BNTX',multiple = TRUE),
                        numericInput("mr4", NULL, 0.3, min = 0, max = 1,step = 0.05)
                    )
                ),
                
                fluidRow(
                    column(width = 6,
                           plotOutput('mplot1')
                    ),
                    column(width = 6,
                           plotOutput('mplot2')
                    )
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
    
    initial_position <- 250000 
    invest_choice_1 <- c('ZM','BNTX','MRNA','NVDA')
    returns_monthly_1 <- after %>%
        filter(symbol %in% invest_choice_1) %>%
        group_by(symbol) %>%
        tq_transmute(select     = adjusted, 
                     mutate_fun = periodReturn, 
                     period     = "monthly", 
                     col_rename = "monthly.return")
    
    qplot_one = reactive({
        p_quant_each <- returns_monthly_1 %>% 
            ggplot(aes(x = date, y = monthly.return, color = symbol)) + 
            geom_line() +
            theme_tq() + scale_color_tq() +
            labs(title = "Performance of Each Stock Invested",x = "Date",y="Monthly Return Rate")
        ggpubr::ggarrange(p_quant_each,ncol = 1)
    })
    output$qplot1 = renderPlot(qplot_one())
    
    qplot_two = reactive({
        weight_1 <- c(input$qr1,input$qr4,input$qr2,input$qr3)
        growth_monthly_1 <- returns_monthly_1 %>%
            tq_portfolio(assets_col   = symbol, 
                         returns_col  = monthly.return, 
                         weights      = weight_1, 
                         col_rename   = "investment.growth",
                         wealth.index = TRUE) %>%
            mutate(investment.growth = investment.growth * initial_position)
        p_quant_total <- growth_monthly_1 %>%
            ggplot(aes(x = date, y = investment.growth/1000)) +
            geom_line(size = 1, color = 'steelblue') +
            labs(title = "Performance of Portfolio",
                 subtitle = "10% ZOOM, 10% NVDA, 20% BNTX, 60% Moderna",
                 x = "Time", y = "Portfolio Value / (thousand $)") +
            ylim(250, 600) +
            theme_tq() + scale_color_tq()# +
        ggpubr::ggarrange(p_quant_total,ncol = 1)
    })
    output$qplot2 = renderPlot(qplot_two())
    
    mplot_one = reactive({
        invest_choice_2 <- c(input$manual_one,input$manual_two,
                             input$manual_three,input$manual_four)
        returns_monthly_2 <- after %>%
            filter(symbol %in% invest_choice_2) %>%
            group_by(symbol) %>%
            tq_transmute(select     = adjusted, 
                         mutate_fun = periodReturn, 
                         period     = "monthly", 
                         col_rename = "monthly.return")
        
        p_manual_each <- returns_monthly_2 %>% 
            ggplot(aes(x = date, y = monthly.return, color = symbol)) + 
            geom_line() +
            theme_tq() + scale_color_tq() +
            labs(title = "Performance of each stock invested",
                 x = "Date",y="Monthly Return Rate")
        ggpubr::ggarrange(p_manual_each,ncol = 1)
    })
    output$mplot1 = renderPlot(mplot_one())
    
    mplot_two = reactive({
        invest_choice_22 <- c(input$manual_one,input$manual_two,
                             input$manual_three,input$manual_four)
        returns_monthly_22 <- after %>%
            filter(symbol %in% invest_choice_22) %>%
            group_by(symbol) %>%
            tq_transmute(select     = adjusted, 
                         mutate_fun = periodReturn, 
                         period     = "monthly", 
                         col_rename = "monthly.return")
        
        weight_2 <- c(input$mr1,input$mr2,input$mr3,input$mr4)
        growth_monthly_2 <- returns_monthly_22 %>%
            tq_portfolio(assets_col   = symbol, 
                         returns_col  = monthly.return, 
                         weights      = weight_2, 
                         col_rename   = "investment.growth",
                         wealth.index = TRUE) %>%
            mutate(investment.growth = investment.growth * initial_position)
        
        p_manual_total <- growth_monthly_2 %>%
            ggplot(aes(x = date, y = investment.growth/1000)) +
            geom_line(size = 1, color = 'steelblue') +
            labs(title = "Performance of Manual Portfolio",
                 x = "Time", y = "Portfolio Value / Thousand $") +
            ylim(250, 600) +
            theme_tq() + scale_color_tq()
        ggpubr::ggarrange(p_manual_total,ncol = 1)
    })
    output$mplot2 = renderPlot(mplot_two())
    
    output$table <- DT::renderDataTable({
        table_data <- data %>% filter(symbol %in% input$table_symbol)
        DT::datatable(table_data, options = list(scrollX =TRUE, pageLength = 20))
    })
}


# Run the application 
shinyApp(ui, server)

