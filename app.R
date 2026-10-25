
# BDA400 Assignment 6
# Technical Analysis Using R - Visualization Phase
# Student: Razak Mohammad
# Data source: Yahoo Finance

library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)
library(dplyr)

# Step 1: Dashboard interface
ui <- fluidPage(
  titlePanel("Stock Portfolio Technical Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("symbol", "Stock Symbol", "AAPL"),
      
      dateRangeInput(
        "dates", "Select Date Range",
        start = "2023-01-01",
        end = Sys.Date()
      ),
      
      selectInput(
        "timeframe", "Time Frame",
        choices = c("Daily", "Weekly", "Monthly")
      ),
      
      selectInput(
        "charttype", "Chart Type",
        choices = c("Line", "Area", "Candlestick")
      ),
      
      checkboxGroupInput(
        "indicators", "Technical Indicators",
        choices = c("Moving Averages", "RSI", "MACD"),
        selected = c("Moving Averages", "RSI", "MACD")
      ),
      
      numericInput("shortMA", "Short Moving Average", 20,
                   min = 2, max = 200),
      
      numericInput("longMA", "Long Moving Average", 50,
                   min = 3, max = 250),
      
      checkboxInput("signals", "Show Trading Signals", TRUE),
      
      actionButton("refresh", "Update Dashboard")
    ),
    
    mainPanel(
      h3("Stock Price and Technical Indicators"),
      plotOutput("stock_chart", height = "450px"),
      plotOutput("rsi_chart", height = "220px"),
      plotOutput("macd_chart", height = "220px"),
      h4("Latest Trading Signal"),
      verbatimTextOutput("latest_signal"),
      h4("Recent Trading Signals"),
      tableOutput("signal_table")
    )
  )
)

# Step 2: Dashboard server
server <- function(input, output, session) {
  
  stock_data <- eventReactive(input$refresh, {
    req(input$symbol, input$dates)
    
    tryCatch({
      symbol <- toupper(trimws(input$symbol))
      
      validate(
        need(input$dates[1] <= input$dates[2],
             "Invalid date range.")
      )
      
      prices <- getSymbols(
        symbol,
        src = "yahoo",
        from = as.Date(input$dates[1]) - 400,
        to = as.Date(input$dates[2]) + 1,
        auto.assign = FALSE
      )
      
      validate(
        need(NROW(prices) > 0,
             "No stock data available.")
      )
      
      prices
    }, error = function(e) {
      validate(need(FALSE,
                    paste("Unable to download stock data:", e$message)))
    })
  }, ignoreNULL = FALSE)
  
  # Step 3: Prepare stock data and indicators
  analysis_data <- reactive({
    prices <- stock_data()
    
    
    if (input$timeframe == "Weekly") {
      prices <- xts::to.period(
        prices,
        period = "weeks",
        indexAt = "endof"
      )
    } else if (input$timeframe == "Monthly") {
      prices <- xts::to.period(
        prices,
        period = "months",
        indexAt = "endof"
      )
    }
    
    # Standardize column names for all time frames
    colnames(prices) <- c(
      "Open", "High", "Low",
      "Close", "Volume", "Adjusted"
    )[seq_len(ncol(prices))]
    
    
    
    validate(
      need(isTRUE(input$shortMA < input$longMA),
           "Short MA must be smaller than Long MA."),
      need(isTRUE(NROW(prices) >= input$longMA + 2),
           "Not enough data for the selected moving averages.")
    )
    
    
    close_price <- Cl(prices)
    
    short_ma <- SMA(close_price, n = input$shortMA)
    long_ma <- SMA(close_price, n = input$longMA)
    rsi_values <- RSI(close_price, n = 14)
    macd_values <- MACD(close_price,
                        nFast = 12,
                        nSlow = 26,
                        nSig = 9,
                        maType = "EMA")
    
    df <- data.frame(
      Date = as.Date(index(prices)),
      Open = as.numeric(Op(prices)),
      High = as.numeric(Hi(prices)),
      Low = as.numeric(Lo(prices)),
      Close = as.numeric(close_price),
      ShortMA = as.numeric(short_ma),
      LongMA = as.numeric(long_ma),
      RSI = as.numeric(rsi_values),
      MACD = as.numeric(macd_values[, 1]),
      MACDSignal = as.numeric(macd_values[, 2])
    )
    
    # Moving average crossover trading strategy
    # Buy when short MA crosses above long MA.
    # Sell when short MA crosses below long MA.
    # Otherwise Hold.
    
    difference <- df$ShortMA - df$LongMA
    previous <- dplyr::lag(difference)
    
    df$Signal <- "Hold"
    
    df$Signal[
      !is.na(previous) &
        previous <= 0 & difference > 0
    ] <- "Buy"
    
    df$Signal[
      !is.na(previous) &
        previous >= 0 & difference < 0
    ] <- "Sell"
    
    df <- df[
      df$Date >= as.Date(input$dates[1]) &
        df$Date <= as.Date(input$dates[2]),
    ]
    
    validate(
      need(nrow(df) > 0,
           "No observations in the selected date range.")
    )
    
    df
  })
  
  # Step 4: Stock chart
  output$stock_chart <- renderPlot({
    df <- analysis_data()
    
    p <- ggplot(df, aes(x = Date))
    
    if (input$charttype == "Line") {
      p <- p + geom_line(aes(y = Close),
                         color = "steelblue", linewidth = 0.8)
    }
    
    if (input$charttype == "Area") {
      p <- p + geom_area(aes(y = Close),
                         fill = "lightblue", alpha = 0.6)
    }
    
    if (input$charttype == "Candlestick") {
      candles <- df
      candles$Direction <- ifelse(
        candles$Close >= candles$Open,
        "Up", "Down"
      )
      
      p <- p +
        geom_linerange(
          data = candles,
          aes(ymin = Low, ymax = High,
              color = Direction)
        ) +
        geom_rect(
          data = candles,
          aes(
            xmin = Date - 0.35,
            xmax = Date + 0.35,
            ymin = pmin(Open, Close),
            ymax = pmax(Open, Close),
            fill = Direction
          )
        ) +
        scale_color_manual(values = c(
          "Up" = "darkgreen", "Down" = "red"
        )) +
        scale_fill_manual(values = c(
          "Up" = "darkgreen", "Down" = "red"
        ))
    }
    
    if ("Moving Averages" %in% input$indicators) {
      p <- p +
        geom_line(aes(y = ShortMA),
                  color = "orange", linewidth = 0.8,
                  na.rm = TRUE) +
        geom_line(aes(y = LongMA),
                  color = "purple", linewidth = 0.8,
                  na.rm = TRUE)
    }
    
    if (input$signals) {
      signal_points <- df[df$Signal != "Hold", ]
      
      if (nrow(signal_points) > 0) {
        p <- p +
          geom_point(
            data = signal_points,
            aes(y = Close, color = Signal),
            size = 3
          ) +
          geom_text(
            data = signal_points,
            aes(y = Close, label = Signal,
                color = Signal),
            vjust = -1, size = 3,
            check_overlap = TRUE
          ) +
          scale_color_manual(values = c(
            "Buy" = "darkgreen",
            "Sell" = "red"
          ))
      }
    }
    
    p +
      labs(
        title = paste(input$symbol, "Stock Price"),
        x = "Date",
        y = "Stock Price (USD)"
      ) +
      theme_minimal()
    
  })
  
  # Step 5: RSI chart
  output$rsi_chart <- renderPlot({
    if (!"RSI" %in% input$indicators) return(NULL)
    
    df <- analysis_data()
    
    ggplot(df, aes(x = Date, y = RSI)) +
      geom_line(color = "darkorange") +
      geom_hline(yintercept = 70,
                 linetype = "dashed", color = "red") +
      geom_hline(yintercept = 30,
                 linetype = "dashed", color = "darkgreen") +
      labs(title = "Relative Strength Index (RSI)",
           x = "Date", y = "RSI") +
      theme_minimal()
  })
  
  # Step 6: MACD chart
  output$macd_chart <- renderPlot({
    if (!"MACD" %in% input$indicators) return(NULL)
    
    df <- analysis_data()
    
    ggplot(df, aes(x = Date)) +
      geom_line(aes(y = MACD),
                color = "blue", na.rm = TRUE) +
      geom_line(aes(y = MACDSignal),
                color = "red", na.rm = TRUE) +
      labs(title = "MACD and Signal Line",
           x = "Date", y = "MACD") +
      theme_minimal()
  })
  
  # Step 7: Latest trading signal
  output$latest_signal <- renderText({
    df <- analysis_data()
    last_row <- tail(df, 1)
    
    paste(
      "Stock:", toupper(input$symbol),
      "\nDate:", as.character(last_row$Date),
      "\nClosing Price: $", round(last_row$Close, 2),
      "\nSignal:", last_row$Signal,
      "\nStrategy: Moving Average Crossover"
    )
  })
  
  # Step 8: Recent signals table
  
  output$signal_table <- renderTable({
    df <- analysis_data()
    
    signals <- df[df$Signal != "Hold",
                  c("Date", "Close", "Signal")]
    
    signals$Date <- format(
      as.Date(signals$Date, origin = "1970-01-01"),
      "%Y-%m-%d"
    )
    
    signals$Close <- round(signals$Close, 2)
    
    tail(signals, 10)
  })
  
}

# Step 9: Launch dashboard
shinyApp(ui = ui, server = server)

