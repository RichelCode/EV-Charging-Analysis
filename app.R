# app.R

library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(plotly)
library(viridis)

# Load Data (update path as needed)
df_clean <- read_csv("www/cleaned_traffic_data.csv", show_col_types = FALSE) %>%
  mutate(
    Timestamp = mdy_hms(Timestamp),
    Date = as.Date(Timestamp),
    Hour = hour(Timestamp),
    Weekday = wday(Timestamp, label = TRUE),
    Month = month(Timestamp, label = TRUE)
  )

# UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Traffic Flow Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome", tabName = "welcome", icon = icon("road")),
      menuItem("Summary", tabName = "summary", icon = icon("chart-bar")),
      menuItem("Time Trends", tabName = "trends", icon = icon("line-chart")),
      menuItem("Heatmap", tabName = "heatmap", icon = icon("th"))
    ),
    selectInput("station", "Select Station:", 
                choices = unique(df_clean$Station), selected = unique(df_clean$Station)[1]),
    sliderInput("date", "Select Date Range:", 
                min = min(df_clean$Date), max = max(df_clean$Date),
                value = c(min(df_clean$Date), max(df_clean$Date)))
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "welcome",
              fluidPage(
                tags$img(src = "traffic_banner.jpg", style = "width:100%; border-radius: 10px; margin-bottom: 20px;"),
                h2("Welcome to California District 3 Traffic Dashboard"),
                p("This app explores congestion and traffic trends using 3 months of PeMS data."),
                h4("Use the menu on the left to explore time trends, congestion patterns, and peak flow hours across selected stations.")
              )
      ),
      
      tabItem(tabName = "summary",
              fluidRow(
                valueBoxOutput("totalVehicles"),
                valueBoxOutput("peakHour"),
                valueBoxOutput("busiestStation")
              )
      ),
      
      tabItem(tabName = "trends",
              fluidPage(
                plotlyOutput("lineTrend")
              )
      ),
      
      tabItem(tabName = "heatmap",
              fluidPage(
                plotOutput("heatmap")
              )
      )
    )
  )
)

# SERVER
server <- function(input, output) {
  filtered_data <- reactive({
    df_clean %>%
      filter(Station == input$station,
             Date >= input$date[1],
             Date <= input$date[2])
  })
  
  output$totalVehicles <- renderValueBox({
    total <- filtered_data() %>%
      summarise(total = sum(`Total Flow`, na.rm = TRUE)) %>%
      pull(total)
    
    valueBox(formatC(total, format = "d", big.mark = ","), "Total Vehicles", icon = icon("car"), color = "green")
  })
  
  output$peakHour <- renderValueBox({
    peak <- filtered_data() %>%
      group_by(Hour) %>%
      summarise(avg = mean(`Total Flow`, na.rm = TRUE)) %>%
      arrange(desc(avg)) %>%
      slice(1) %>%
      pull(Hour)
    
    valueBox(paste0(peak, ":00"), "Peak Hour", icon = icon("clock"), color = "yellow")
  })
  
  output$busiestStation <- renderValueBox({
    top_station <- df_clean %>%
      group_by(Station) %>%
      summarise(total = sum(`Total Flow`, na.rm = TRUE)) %>%
      arrange(desc(total)) %>%
      slice(1) %>%
      pull(Station)
    
    valueBox(top_station, "Busiest Station Overall", icon = icon("map-marker-alt"), color = "red")
  })
  
  output$lineTrend <- renderPlotly({
    data <- filtered_data() %>%
      group_by(Date) %>%
      summarise(AvgFlow = mean(`Total Flow`, na.rm = TRUE))
    
    plot_ly(data, x = ~Date, y = ~AvgFlow, type = 'scatter', mode = 'lines', line = list(color = 'darkblue')) %>%
      layout(title = "Daily Average Flow", yaxis = list(title = "Vehicles/hour"))
  })
  
  output$heatmap <- renderPlot({
    data <- filtered_data() %>%
      group_by(Weekday, Hour) %>%
      summarise(AvgFlow = mean(`Total Flow`, na.rm = TRUE), .groups = "drop") %>%
      mutate(Weekday = factor(Weekday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))
    
    ggplot(data, aes(x = Hour, y = Weekday, fill = AvgFlow)) +
      geom_tile(color = NA) +
      scale_fill_viridis_c(option = "magma", name = "Flow") +
      theme_minimal() +
      labs(title = "Hourly Congestion Pattern", x = "Hour of Day", y = NULL)
  })
}

shinyApp(ui, server)
