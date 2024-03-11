library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)

# Read the file -> select the col we need
MM <- read.csv("stockM/MM.csv") %>%
  select(Date, Symbol, High, Low)
SBIN <- read.csv("stockM/SBIN.csv") %>%
  select(Date, Symbol, High, Low)
POWERGRID <- read.csv("stockM/POWERGRID.csv") %>%
  select(Date, Symbol, High, Low)
ONGC <- read.csv("stockM/ONGC.csv") %>%
  select(Date, Symbol, High, Low)
BPCL <- read.csv("stockM/BPCL.csv") %>%
  select(Date, Symbol, High, Low)
ASIANPAINT <- read.csv("stockM/ASIANPAINT.csv") %>%
  select(Date, Symbol, High, Low)
ADANIPORTS <- read.csv("stockM/ADANIPORTS.csv") %>%
  select(Date, Symbol, High, Low)
CIPLA <- read.csv("stockM/CIPLA.csv") %>%
  select(Date, Symbol, High, Low)
ITC <- read.csv("stockM/ITC.csv") %>%
  select(Date, Symbol, High, Low)
WIPRO <- read.csv("stockM/WIPRO.csv") %>%
  select(Date, Symbol, High, Low)

# Shiny UI 정의
ui <- fluidPage(
  titlePanel(title = "Volatility of stock prices from 2000 to 2021"),
  selectInput("stock", "Select Stock", choices = c("M&M", "SBIN", "POWERGRID", "ONGC", "BPCL", "ASIANPAINT", "ADANIPORTS", "CIPLA", "ITC", "WIPRO")),
  sliderInput("year", "Year", min = 2000, max = 2021, c(2000, 2021), sep = ""),
  plotlyOutput("stock_plot", width = "800px", height = "500px")
)

# Shiny 서버 정의
server <- function(input, output) {
  output$stock_plot <- renderPlotly({
    data <- switch(input$stock,
                   "M&M" = MM,
                   "SBIN" = SBIN,
                   "POWERGRID" = POWERGRID,
                   "ONGC" = ONGC,
                   "BPCL" = BPCL,
                   "ASIANPAINT" = ASIANPAINT,
                   "ADANIPORTS" = ADANIPORTS,
                   "CIPLA" = CIPLA,
                   "ITC" = ITC,
                   "WIPRO" = WIPRO)%>%
      filter(Date >= input$year[1] & Date <= input$year[2])
    
    data$Date <- as.Date(data$Date)
    
    # p <- ggplot(data, aes(x = Date)) +
    #   geom_line(aes(y = High, color = "High", group = 1)) +
    #   geom_line(aes(y = Low, color = "Low", group = 1)) +
    #   labs(title = paste(input$stock, "Stock Prices"), x = "Date", y = "Price") +
    #   scale_color_manual(values = c("High" = "pink", "Low" = "skyblue")) +
    #   theme_minimal() +
    #   scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m")
    
    plot_ly(data, x = ~Date, y = ~High, type = 'scatter', mode = 'lines', name = "High", line = list(color = 'pink')) %>%
      add_trace(y = ~Low, name = "Low", line = list(color = 'skyblue')) %>%
      layout(title = paste(input$stock, "Stock Prices"), xaxis = list(title = "Date"), yaxis = list(title = "Price")) %>%
      layout(dragmode = "zoom")
  })
  
}

# Shiny 애플리케이션 실행
shinyApp(ui = ui, server = server)