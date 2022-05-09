
library(shiny)
library(shinydashboard)
library(fpp3)
library(plotly)
library(ggplot2)
library(dplyr)
library(ggeasy)
library(seasonal)

# Data starts in 3rd row, skip first 2 rows
g_trends <- read.csv("pmdata.csv", skip = 2)
# Rename columns
names(g_trends) <- c("Month", "Interest")
# Convert Month to date
g_trends$Month <- yearmonth(g_trends$Month)
# Convert to tsibble
g_trends <- tsibble(g_trends)

# Google labels low numbers as "<1"
# Convert those to 0s and ensure the column is numbers
g_trends$Interest <- as.numeric(
  ifelse(g_trends$Interest == "<1", 0, g_trends$Interest)
)

plotly_acf <- function(acf_output) {
  n_obs <- attributes(acf_output)$num_obs[[1]]
  ci <- qnorm((1 + 0.95) / 2) / sqrt(n_obs)
  
  interval_str <- format(interval(acf_output))
  xlab <- paste0("lag [", interval_str, "]")
  
  p <- acf_output %>%
    mutate(lag = row_number()) %>%
    ggplot(aes(lag, acf)) +
    geom_bar(stat = "identity", width = 0.25) +
    geom_hline(yintercept = ci, color = "blue", linetype = "dashed") +
    geom_hline(yintercept = -ci, color = "blue", linetype = "dashed") +
    geom_hline(yintercept = 0) +
    labs(x = xlab)
  
  ggplotly(p)
}



# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "Peach Milkshake Relevancy",
                  titleWidth = 350),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Intro & Instructions", tabName = "Tab1"),
      menuItem("Time Series Plots", tabName = "Tab2", icon = icon("chart-line"),
               menuSubItem("Full-Time Series", tabName = "SubTab1"),
               menuSubItem("Analyze Patterns", tabName = "SubTab2")),
      menuItem("Forecasting", tabName = "Tab3", icon = icon("chart-line"),
               menuSubItem("Simple Models", tabName = "SubTab3"),
               menuSubItem("Exponential Smoothing", tabName = "SubTab4"),
               menuSubItem("ARIMA Models", tabName = "SubTab5"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Tab1",
              box(h3("Who doesn't love a refreshing peach milkshake? Especially in the summer!",
                     align = "center"),
                  br(),
                  h4("This app analyzes the relevance of peach milkshakes based on Google search history
                     over the years.",
                     align = "center"),
                  br(),
                  h4("First, a plot is displayed to visualize the entirety of the data set containing
                     the frequency in the words 'peach milkshake' together from January 2004 to March 2022.",
                     align = "center"),
                  br(),
                  h4("Then, there are more options to visualize the data and the patterns in the time series
                     under the 'Analyze Patterns' tab. There are options to view a plot of the seasonality,
                     the autocorrelation, and the multiplicative decomposition of the time series data.",
                     align = "center"),
                  br(),
                  h4("Finally, the 'Forecasting' tab contains predictive models that can be used to forecast
                     the frequency of Google searches for peach milkshakes anywhere from 1 to 10 years out using
                     multiple forecasting methods.",
                     align = "center"),
                  background = "yellow",
                  width = "400%",
                  height = "400%"
              )
      ),
      # Second tab content
      tabItem(tabName = "Tab2"),
      tabItem(tabName = "SubTab1",
              strong(h2("View plot of full time series", align = "center")),
              br(),
              h3("The data has been pulled from Google Trends and shows the frequency that
              'peach milkshake' was searched. The time period range is January 2004 
              to March 2022.", align = "center"),
              h4("Hover over the graph to view specific entries for each month.", 
                 align = "center"),
              hr(),
              box(
                plotlyOutput(
                  "full",
                  width = "200%",
                  height = "400%",
                  inline = FALSE
                )
              )
      ),
      tabItem(tabName = "SubTab2",
              strong(h2("View seasonality, autocorrelation, or decomposition of time series."),
                     align = "center"),
              br(),
              fluidPage(
                box(
                  selectInput("select", label = h3("Choose information to view:"),
                              choices = c("Seasonality", "Autocorrelation", "Decomposition")
                  ),
                  plotlyOutput(
                    "plot2"
                  )
                ),
                box(
                  h4("Interpretation of Time Series:", 
                     align = "center"),
                  h5("There is evident seasonality for the interest in peach milkshakes,
                    according to Google searches. From about May to August every year, 
                    the interest is at the peak for the year. Peach is a summery fruit
                    and more often found as an available flavor in the summer, so the seasonal
                    peak during the summer months makes sense."),
                  br(),
                  h5("There also seems to be a long-term upward trend in the interest in peach
                    milkshakes. There is a peak in the summer of 2004, but then the
                    interest stayed relatively steady at a low level until the summer of 2009. 
                    June 2009 is when Chick-fil-a debuted their fan-favorite peach milkshake, 
                    announcing it would be limited edition -- only available for the summer.
                    Since 2009, there has been an increase in searches for 'peach milkshakes',
                    with that summertime peak getting higher and higher each year."),
                  br(),
                  h5("The highly correlated lag1 means that last month is a good predictor of this month.
                    There are several lags in a row that are relatively correlated because there is
                    seasonality in the data. Then, there are a few lags that are negatively correlated because
                    the data for that month and the month before tends to be very different.")
                )
              )
      ),
      # Third tab content
      tabItem(tabName = "Tab3"),
      tabItem(tabName = "SubTab3",
              strong(h2("Forecast future relevancy for peach milkshakes based
                        on current and past interest data using simple models.",
                        align = "center")),
              fluidRow(
                box(
                  sliderInput("select2", label = h4("Select how many years to forecast:"),
                              min = 1,
                              max = 10,
                              value = 1,
                              width = "200%"),
                  br(),
                  plotOutput(
                    "forecast")
                ),
                fluidRow(
                  box(
                    radioButtons("radio1",
                                 h3("Select simple model to view."),
                                 choices = list("Seasonal Naive" = 1,
                                                "Naive" = 2,
                                                "Drift" = 3,
                                                "Mean" = 4),
                                 selected = 1
                    )
                  ),
                  box(
                    h5("A feature like this could be useful for a company, like Chick-fil-A,
                     that offers a peach-flavored milkshake seasonally. Knowing the increasing
                     trend in interest for that peak season of June-August could affect their
                     demand forecasts. Other companies that currently do not offer a peach milkshake
                     could see the potential demand for doing so in the upcoming years.",
                       align = "center")
                  )
                )
              )
      ),
      tabItem(tabName = "SubTab4",
              strong(h2("Forecast future relevancy for peach milkshakes based
                        on current and past interest data using exponential smoothing.",
                        align = "center")),
              fluidRow(
                box(
                  sliderInput("select3", label = h4("Select how many years to forecast:"),
                              min = 1,
                              max = 10,
                              value = 1,
                              width = "200%"),
                  br(),
                  plotOutput(
                    "expSmooth")
                ),
                fluidRow(
                  box(
                    radioButtons("radio2",
                                 h3("Select exponential smoothing method to view."),
                                 choices = list("Holts Linear Trend" = 1,
                                                "Holts/Winters Seasonal" = 2),
                                 selected = 1
                    )
                  ),
                  box(
                    h5("Forecasting using exponential smoothing methods produces weighted
                       averages of past observations. The weights decrease exponentially as the
                       observations get older so that the most recent observations have a greater
                       impact on the future forecast.",
                       align = "center")
                  )
                )
              )     
      ),
      tabItem(tabName = "SubTab5",
              strong(h2("Forecast future relevancy for peach milkshakes based
                        on current and past interest data using ARIMA models.",
                        align = "center")),
              fluidRow(
                box(
                  sliderInput("select4", label = h4("Select how many years to forecast:"),
                              min = 1,
                              max = 10,
                              value = 1,
                              width = "200%"),
                  br(),
                  plotOutput(
                    "ARIMA")
                ),
                fluidRow(
                  box(
                    radioButtons("radio3",
                                 h3("Select ARIMA model to view."),
                                 choices = list("W/Manually selected parameters" = 1,
                                                "W/Auto-selected parameters" = 2),
                                 selected = 1
                    )
                  ),
                  box(
                    h5("The exponential smoothing methods and ARIMA methods provide complementary
                       approaches to the time series forecasting problem. ARIMA models specifically 
                       aim to describe autocorrelations in the data.",
                       align = "center")
                  )
                )
              )
      )
    )
  )
)


server <- function(input, output) {
  
  # Full plot
  output$full <- renderPlotly({
    g_trends %>%
      ggplot(aes(x = Month, y = Interest)) +
      geom_line(colour = "Black",
                lwd = 0.7) +
      labs(title = "Google Searches for 'Peach Milkshake'", y = "Interest") + 
      easy_center_title() -> PLOT
    ggplotly(PLOT)
  })
  
  # Analysis plot
  output$plot2 <- renderPlotly({
    p1 <- gg_season(g_trends, labels = "both")
    p2 <- g_trends %>%
      ACF(Interest) %>%
      plotly_acf()
    p3 <- g_trends %>%
      model(
        classical_decomposition(Interest, type = "multiplicative")) %>%
      components() %>%
      autoplot() +
      labs(title = "Classical multiplicative decomposition of Interest")
    
    if(input$select == "Seasonality") {p1}
    else if(input$select == "Autocorrelation") {p2}
    else {p3}
  })
  
  # Simple forecast plots
  output$forecast <- renderPlot({
    fit1 <- g_trends %>%
      model(SNAIVE(Interest))
    fit2 <- g_trends %>%
      model(NAIVE(Interest))
    fit3 <- g_trends %>%
      model(RW(Interest ~ drift()))
    fit4 <- g_trends %>%
      model(MEAN(Interest))
    
    forecast1 <- fit1 %>%
      forecast(h = 
                 if(input$select2 == "1") {"1 year"}
               else if(input$select2 == "2") {"2 years"}
               else if(input$select2 == "3") {"3 years"}
               else if(input$select2 == "4") {"4 years"}
               else if(input$select2 == "5") {"5 years"}
               else if(input$select2 == "6") {"6 years"}
               else if(input$select2 == "7") {"7 years"}
               else if(input$select2 == "8") {"8 years"}
               else if(input$select2 == "9") {"9 years"}
               else {"10 years"}
      )
    
    forecast2 <- fit2 %>%
      forecast(h = 
                 if(input$select2 == "1") {"1 year"}
               else if(input$select2 == "2") {"2 years"}
               else if(input$select2 == "3") {"3 years"}
               else if(input$select2 == "4") {"4 years"}
               else if(input$select2 == "5") {"5 years"}
               else if(input$select2 == "6") {"6 years"}
               else if(input$select2 == "7") {"7 years"}
               else if(input$select2 == "8") {"8 years"}
               else if(input$select2 == "9") {"9 years"}
               else {"10 years"}
      )
    
    forecast3 <- fit3 %>%
      forecast(h = 
                 if(input$select2 == "1") {"1 year"}
               else if(input$select2 == "2") {"2 years"}
               else if(input$select2 == "3") {"3 years"}
               else if(input$select2 == "4") {"4 years"}
               else if(input$select2 == "5") {"5 years"}
               else if(input$select2 == "6") {"6 years"}
               else if(input$select2 == "7") {"7 years"}
               else if(input$select2 == "8") {"8 years"}
               else if(input$select2 == "9") {"9 years"}
               else {"10 years"}
      )
    
    forecast4 <- fit4 %>%
      forecast(h = 
                 if(input$select2 == "1") {"1 year"}
               else if(input$select2 == "2") {"2 years"}
               else if(input$select2 == "3") {"3 years"}
               else if(input$select2 == "4") {"4 years"}
               else if(input$select2 == "5") {"5 years"}
               else if(input$select2 == "6") {"6 years"}
               else if(input$select2 == "7") {"7 years"}
               else if(input$select2 == "8") {"8 years"}
               else if(input$select2 == "9") {"9 years"}
               else {"10 years"}
      )
    
    if(input$radio1 == "1") {
      forecast1 %>%
        autoplot(g_trends, level = NULL) +
        labs(title = "Forecasted interest for 'Peach Milkshake'",
             y = "Google searches")
    }
    else if(input$radio1 == "2") {
      forecast2 %>%
        autoplot(g_trends, level = NULL) +
        labs(title = "Forecasted interest for 'Peach Milkshake'",
             y = "Google searches")
    }
    else if(input$radio1 == "3") {
      forecast3 %>%
        autoplot(g_trends, level = NULL) +
        labs(title = "Forecasted interest for 'Peach Milkshake'",
             y = "Google searches")
    }
    else {
      forecast4 %>%
        autoplot(g_trends, level = NULL) +
        labs(title = "Forecasted interest for 'Peach Milkshake'",
             y = "Google searches")
    }
  })
  
  # Exp Smoothing plots
  output$expSmooth <- renderPlot({
    fc1 <- g_trends %>%
      model(ETS(Interest ~ error("A") + trend("N") + season("N")))
    fc2 <- g_trends %>%
      model(
        Additive = ETS(Interest ~ error("A") + trend("A") + season("A")),
        Multiplicative = ETS(Interest ~ error("M") + trend("A") + season("M"))
      )
    
    forecast5 <- fc1 %>%
      forecast(h = 
                 if(input$select3 == "1") {"1 year"}
               else if(input$select3 == "2") {"2 years"}
               else if(input$select3 == "3") {"3 years"}
               else if(input$select3 == "4") {"4 years"}
               else if(input$select3 == "5") {"5 years"}
               else if(input$select3 == "6") {"6 years"}
               else if(input$select3 == "7") {"7 years"}
               else if(input$select3 == "8") {"8 years"}
               else if(input$select3 == "9") {"9 years"}
               else {"10 years"}
      )
    
    forecast6 <- fc2 %>%
      forecast(h = 
                 if(input$select3 == "1") {"1 year"}
               else if(input$select3 == "2") {"2 years"}
               else if(input$select3 == "3") {"3 years"}
               else if(input$select3 == "4") {"4 years"}
               else if(input$select3 == "5") {"5 years"}
               else if(input$select3 == "6") {"6 years"}
               else if(input$select3 == "7") {"7 years"}
               else if(input$select3 == "8") {"8 years"}
               else if(input$select3 == "9") {"9 years"}
               else {"10 years"}
      )
    
    if(input$radio2 == "1") {
      forecast5 %>%
        autoplot(g_trends, level = NULL) +
        labs(title = "Forecasted interest for 'Peach Milkshake'",
             y = "Google searches")
    }
    else {
      forecast6 %>%
        autoplot(g_trends, level = NULL) +
        labs(title = "Forecasted interest for 'Peach Milkshake'",
             y = "Google searches") +
        guides(colour = guide_legend(title = "Forecast"))
    }
  })
  
  # ARIMA Model plots
  output$ARIMA <- renderPlot({
    fc3 <- g_trends %>%
      model(
        arima012011 = ARIMA(Interest ~ pdq(0,1,2) + PDQ(0,1,1)),
        arima210011 = ARIMA(Interest ~ pdq(2,1,0) + PDQ(0,1,1))
      )
    fc4 <- g_trends %>%
      model(ARIMA(Interest))
    
    forecast7 <- fc3 %>%
      forecast(h = 
                 if(input$select4 == "1") {"1 year"}
               else if(input$select4 == "2") {"2 years"}
               else if(input$select4 == "3") {"3 years"}
               else if(input$select4 == "4") {"4 years"}
               else if(input$select4 == "5") {"5 years"}
               else if(input$select4 == "6") {"6 years"}
               else if(input$select4 == "7") {"7 years"}
               else if(input$select4 == "8") {"8 years"}
               else if(input$select4 == "9") {"9 years"}
               else {"10 years"}
      )
    
    forecast8 <- fc4 %>%
      forecast(h = 
                 if(input$select4 == "1") {"1 year"}
               else if(input$select4 == "2") {"2 years"}
               else if(input$select4 == "3") {"3 years"}
               else if(input$select4 == "4") {"4 years"}
               else if(input$select4 == "5") {"5 years"}
               else if(input$select4 == "6") {"6 years"}
               else if(input$select4 == "7") {"7 years"}
               else if(input$select4 == "8") {"8 years"}
               else if(input$select4 == "9") {"9 years"}
               else {"10 years"}
      )
    
    if(input$radio3 == "1") {
      forecast7 %>%
        autoplot(g_trends, level = NULL) +
        labs(title = "Forecasted interest for 'Peach Milkshake'",
             y = "Google searches") +
        guides(colour = guide_legend(title = "Parameters"))
    }
    else {
      forecast8 %>%
        autoplot(g_trends, level = NULL) +
        labs(title = "Forecasted interest for 'Peach Milkshake'",
             y = "Google searches")
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

