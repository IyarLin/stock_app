library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyquant)
library(scales)

choices <- c("S&P 500 (USA)", 
             "STOXX 600 (Europe)", 
             "NIKKEI 225 (Japan)", 
             "MOEX (Russia)", 
             "SSE Composite Index (China)", 
             "iShares Latin America 40 ETF", 
             "iShares MSCI World ETF")

cols <- hcl(h = seq(15, 375, length = length(choices) + 1), l = 65, c = 100)[1:length(choices)]

# Define UI ----
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      h1("Control panel"),
      checkboxGroupInput(inputId = "index_selector",
                         label = h2("Index"),
                         choices = choices,
                         selected = "S&P 500 (USA)"),
      h3('To set period start from a specific date choose 
         "from date" below and set the "from" widget below'),
      radioButtons(inputId = "period_selector", label = h2("Period"),
                   choices = c("day", "week", "month", "3 months",
                               "YTD", "1 year", "5 years", "10 years", 
                               "from date"), selected = "week"),
      dateInput(inputId = "from_date_selector", label = h2("From"), 
                value = as.Date("2017-03-03"))),
    mainPanel = mainPanel(h1("Please wait a few seconds for the data to load"),
                          plotOutput(outputId = "stock_price_plot"))
  )
)

# Define server logic ----
server <- function(input, output) {
  
  prices <- tq_get(c("^GSPC", "^STOXX", "^N225", "000001.SS", "IMOEX.ME", 
                     "ILF", "URTH"), from = "1900-01-01") %>%
    mutate(index = case_when(
      symbol == "^GSPC" ~ choices[1],
      symbol == "^STOXX" ~ choices[2],
      symbol == "^N225" ~ choices[3], 
      symbol == "000001.SS" ~ choices[4], 
      symbol == "IMOEX.ME" ~ choices[5], 
      symbol == "ILF" ~ choices[6], 
      symbol == "URTH" ~ choices[7]
    )) %>%
    filter(!is.na(close))
  
  last_date <- max(prices$date)
  
  output$stock_price_plot <- renderPlot({
    period <- input$period_selector
    if(period == "day"){
      stock_prices <- prices %>%
        filter(index %in% input$index_selector) %>%
        group_by(index) %>%
        filter(row_number() >= n() - 1) %>%
        ungroup()
    } else {
      from <- case_when(
        period == "week" ~ last_date - days(7),
        period == "month" ~ last_date - months(1),
        period == "3 months" ~ last_date - months(3),
        period == "YTD" ~ as.Date(paste0(year(Sys.Date()), "-01-", "01")),
        period == "1 year" ~ last_date - years(1),
        period == "5 years" ~ last_date - years(5),
        period == "10 years" ~ last_date - years(10),
        period == "from date" ~ input$from_date_selector
      )
      
      stock_prices <- prices %>%
        filter(date >= from & index %in% input$index_selector)
    }
    
    change <- stock_prices %>%
      arrange(index, date) %>%
      group_by(index) %>%
      summarise(relative = close[n()]/close[1], date = date[n()]) %>%
      mutate(relative_text = paste0(round((relative - 1) * 100, 2), "%"))
    
    stock_prices %>%
      mutate(index = factor(index, 
                            levels = choices)) %>%
      group_by(index) %>%
      mutate(relative = close / close[1]) %>%
      ungroup() %>%
      ggplot(aes(date, relative, color = index)) +
      geom_line() +
      scale_color_manual(values = setNames(cols, choices)) +
      scale_y_continuous(labels = function(label) scales::percent(label - 1)) +
      theme(axis.title = element_blank(), legend.position = "bottom", 
            text = element_text(size = 20)) +
      scale_x_date(expand = expand_scale(mult = c(0, 0.05))) +
      geom_hline(yintercept = 1, lty = 2) +
      geom_text(data = change, aes(label = relative_text), 
                show.legend = FALSE, size = 7, nudge_x = -0.4)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
