# app.R

library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(plotly)
library(viridis)

# Load Data
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
      menuItem("Heatmap", tabName = "heatmap", icon = icon("th")),
      menuItem("Compare Stations", tabName = "compare", icon = icon("exchange-alt")),
      menuItem("Lane & Direction", tabName = "lanedir", icon = icon("random")),
      menuItem("Insights", tabName = "insights", icon = icon("lightbulb")),
      menuItem("Ask Chatbot", tabName = "chat", icon = icon("comments"))
    ),
    selectizeInput("station", "Select Station:", choices = NULL, selected = NULL),
    sliderInput("date", "Select Date Range:", 
                min = min(df_clean$Date), max = max(df_clean$Date),
                value = c(min(df_clean$Date), max(df_clean$Date)))
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "welcome",
              tags$img(
                src = "traffic_banner.jpg",
                style = "width:100%; border-radius: 10px; margin-bottom: 20px;",
                alt = "Traffic Dashboard Banner"
              ),
              h2("Welcome to California District 3 Traffic Dashboard"),
              p("This app explores congestion and traffic trends using 3 months of PeMS data."),
              h4("Use the menu on the left to explore time trends, congestion patterns, and peak flow hours across selected stations.")
      ),
      
      tabItem(tabName = "summary",
              fluidRow(
                valueBoxOutput("totalVehicles"),
                valueBoxOutput("peakHour"),
                valueBoxOutput("busiestStation")
              )
      ),
      
      tabItem(tabName = "trends",
              plotlyOutput("lineTrend")
      ),
      
      tabItem(tabName = "heatmap",
              plotOutput("heatmap")
      ),
      
      tabItem(tabName = "compare",
              selectizeInput("compare1", "Select First Station:", choices = NULL, selected = NULL),
              selectizeInput("compare2", "Select Second Station:", choices = NULL, selected = NULL),
              plotlyOutput("comparePlot")
      ),
      
      tabItem(tabName = "lanedir",
              fluidRow(
                box(title = "Traffic Flow by Lane Type", width = 6, plotlyOutput("lanePlot")),
                box(title = "Traffic Flow by Direction", width = 6, plotlyOutput("dirPlot"))
              )
      ),
      
      tabItem(tabName = "insights",
              h3("Key Insights"),
              tags$ul(
                tags$li("Morning and evening peak hours consistently show highest traffic flow."),
                tags$li("Main Lanes (ML) carry significantly more traffic compared to On-Ramps and Off-Ramps."),
                tags$li("Station 312564 is the most congested station in this dataset."),
                tags$li("Weekend traffic shows significantly lower flow compared to weekdays."),
                tags$li("Northbound and Southbound routes experience uneven distribution in traffic load.")
              )
      ),
      
      tabItem(tabName = "chat",
              h3("Ask the Chatbot"),
              p("Need help with the dashboard or traffic insights? Ask away!"),
              textInput("user_question", "Your Question:", placeholder = "e.g., What’s the busiest station?"),
              actionButton("ask", "Ask"),
              br(), br(),
              verbatimTextOutput("chat_response")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  observe({
    updateSelectizeInput(session, "station", choices = unique(df_clean$Station), server = TRUE)
    updateSelectizeInput(session, "compare1", choices = unique(df_clean$Station), server = TRUE)
    updateSelectizeInput(session, "compare2", choices = unique(df_clean$Station), server = TRUE)
  })
  
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
  
  output$comparePlot <- renderPlotly({
    data1 <- df_clean %>% filter(Station == input$compare1)
    data2 <- df_clean %>% filter(Station == input$compare2)
    
    p1 <- ggplot(data1, aes(x = Date, y = `Total Flow`)) +
      geom_line(color = "steelblue") +
      labs(title = paste("Station", input$compare1)) +
      theme_minimal()
    
    p2 <- ggplot(data2, aes(x = Date, y = `Total Flow`)) +
      geom_line(color = "darkorange") +
      labs(title = paste("Station", input$compare2)) +
      theme_minimal()
    
    subplot(ggplotly(p1), ggplotly(p2), nrows = 2, shareX = TRUE)
  })
  
  output$lanePlot <- renderPlotly({
    lane_data <- df_clean %>%
      group_by(`Lane Type`) %>%
      summarise(AvgFlow = mean(`Total Flow`, na.rm = TRUE))
    
    ggplot(lane_data, aes(x = `Lane Type`, y = AvgFlow, fill = `Lane Type`)) +
      geom_bar(stat = "identity") +
      labs(title = "Average Flow by Lane Type", y = "Vehicles/hour") +
      theme_minimal()
  })
  
  output$dirPlot <- renderPlotly({
    dir_data <- df_clean %>%
      group_by(`Direction of Travel`) %>%
      summarise(AvgFlow = mean(`Total Flow`, na.rm = TRUE))
    
    ggplot(dir_data, aes(x = `Direction of Travel`, y = AvgFlow, fill = `Direction of Travel`)) +
      geom_bar(stat = "identity") +
      labs(title = "Average Flow by Direction", y = "Vehicles/hour") +
      theme_minimal()
  })
  
  # Chatbot Logic
  observeEvent(input$ask, {
    user_input <- tolower(input$user_question)
    
    response <- case_when(
      str_detect(user_input, "peak hour") ~ "Peak hour is usually between 7–9 AM and 4–6 PM.",
      str_detect(user_input, "busiest station") ~ "Station 312564 is the most congested station in the dataset.",
      str_detect(user_input, "weekend") ~ "Traffic on weekends is significantly lower compared to weekdays.",
      str_detect(user_input, "direction") ~ "Northbound and Southbound routes have uneven distribution in traffic flow.",
      str_detect(user_input, "how to use") ~ "Use the sidebar to explore different visualizations like trends, heatmaps, and comparisons.",
      TRUE ~ "Sorry, I’m not sure how to answer that yet. Try asking about peak hours, station trends, or lane types."
    )
    
    output$chat_response <- renderText({ response })
  })
}

shinyApp(ui, server)
