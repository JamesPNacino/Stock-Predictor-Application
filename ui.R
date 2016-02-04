source("Read_data.R", echo=TRUE)
library(googleVis)
library(shiny)


##NOTE!!! The app takes a couple seconds to load
shinyUI(pageWithSidebar(
        
        headerPanel("Regression Stock Price Predictor and Analysis"),
        sidebarPanel(
                textInput("Stock", "Analyze any Stock on the NYSE or NASDAQ stock exchanges:", value="MSFT"),
                selectInput('DayBack', "How many days back would you like your data to go back to help make the prediction?",
                            choices=c("30 Days"="30", "120 Days"="120", "Maximum Number of Days Available"="Max"), selected="Max"),
                sliderInput('DayForward', "How many days in advance would you like for your stock's price prediction?",
                            30, min=1, max=210, step=1, sep=""),
                sliderInput('FinancialPredictorMultiplier', "Set the multiplier on the financial predictors, listed as '(FP)'. NOTE: Leave the slider on the value of '1' to predict based on the value of the current financial predictor:",
                            1, min=.1, max=2, step=.01, sep=""),
                checkboxGroupInput("Predictors", "Choose any of the following predictors to include in your customized stock prediction. It is recommended to leave the variable, 'Date' checked. (There may be an error to predict prices if you do not use all historic data or if the ratios were not found in the dataset):", 
                             choices=c("Date"="7", "Return On Assets (FP)"="12", "Return On Equity (FP)"="13", "Profit Margin (FP)"="14", 
                                       "Current Ratio (FP)"="15", "Quick Ratio (FP)"="16", "Debt to Equity Ratio (FP)"="17", "Interest Coverage Ratio (FP)"="18", 
                                       "Asset Turnover Ratio (FP)"="19", "Inventory Turnover Ratio (FP)"="20"), selected="7"),
                selectInput('FinancialStatement', "Choose which financial statement to view",
                            choices=c("Balance Sheet"="BS", "Income Statement"="IS", "Cash Flow"="CF"), selected="BS"),
                submitButton(text="Analyze")
                ),
        
        mainPanel(
                tabsetPanel(
                        tabPanel("Data Table",
                                 h4("Stock Data"),
                                 dataTableOutput("data_table")),
                        tabPanel("Financial Statements",
                                 plotOutput("regression_plot"),
                                 dataTableOutput("finance_statement")),
                        tabPanel("Regression Analysis",
                                 h5("May take a couple seconds to load."),
                                 plotOutput("regression_plot2"),
                                 h5("Using the following variables that you selected, listed below is the 95% price prediction interval for the stock (fit=Predicted Price; lwr=Lower Price Interval; upr=Upper Price Interval):"),
                                 verbatimTextOutput("prediction"),
                                 h5("Regression Analysis:"),
                                 verbatimTextOutput("summary")),
                        tabPanel("Ratio Analysis",
                                 h4("GoogleVis Motion Chart API to analyze financial ratios and facet plot to compare ratios side by side."),
                                htmlOutput("plot_table"),
                                plotOutput("facet_plot")),
                        tabPanel("Time Series clustering",
                                 h4("Work In Progress. Coming Soon! The goal here is to automatically classify similar stocks to the one you listed.")),
                        tabPanel("Questions",
                                 h4("Vist this site for further details on the stock analysis", a("https://github.com/JamesPNacino/Stock-Predictor-Application", href="https://github.com/JamesPNacino/Stock-Predictor-Application")))
                )
        )))