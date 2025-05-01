library(shiny)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(leaflet)
library(viridis)
library(RColorBrewer)
library(colourpicker)

# === Load Data ===
df_cleaned <- read_csv("www/sampled_traffic_data.csv", show_col_types = FALSE) %>%
  select(Timestamp, Date, Hour, Weekday, Month,
         Station, District, Route,
         `Direction of Travel`, `Lane Type`,
         `Total Flow`, `Avg Speed`, `Delay (V_t=45)`) %>%
  filter(
    !is.na(`Total Flow`),
    !is.na(`Avg Speed`),
    !is.na(`Delay (V_t=45)`)
  )

station_coords <- read_csv("C:\\Users\\attafuro\\Desktop\\EV Charging Analysis\\Traffic_Dashboard\\Coordinates_for_District_3_Stations.csv")

df_map <- df_cleaned %>%
  inner_join(station_coords, by = "Station")

bubble_data <- df_map %>%
  group_by(Station, Latitude, Longitude) %>%
  summarise(
    avg_flow = mean(`Total Flow`, na.rm = TRUE),
    avg_speed = mean(`Avg Speed`, na.rm = TRUE),
    .groups = "drop"
  )

# === UI ===
ui <- fluidPage(
  titlePanel("Traffic Dashboard for District 3"),
  tabsetPanel(
    
    tabPanel("Welcome",
             tags$img(
               src = "traffic_banner.jpg",
               style = "width:100%; margin-top:20px; border-radius: 10px;"
             ),
             h2("Welcome to the District 3 Traffic Dashboard"),
             p("Explore congestion patterns, traffic speed, and more. Use the tabs above to navigate.")
    ),
    
    tabPanel("Flow & Delay Trends",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("flow_direction", "Select Direction(s):",
                                choices = unique(df_cleaned$`Direction of Travel`),
                                selected = unique(df_cleaned$`Direction of Travel`),
                                multiple = TRUE),
                 sliderInput("hour_slider", "Select Hour Range:", min = 0, max = 23, value = c(0, 23)),
                 checkboxInput("show_delay_line", "Show Average Delay Line", value = TRUE),
                 colourInput("delay_color", "Delay Line Color:", value = "#FF5733"),
                 selectInput("facet_choice", "Facet By:", choices = c("None", "Weekday", "Month"), selected = "None")
               ),
               mainPanel(
                 plotOutput("flow_delay_plot")
               )
             )
    ),
    
    tabPanel("Heatmap",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("hour_range", "Select Hour Range:", min = 0, max = 23, value = c(6, 18)),
                 checkboxGroupInput("selected_days", "Select Days:",
                                    choices = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
                                    selected = c("Mon", "Tue", "Wed", "Thu", "Fri"))
               ),
               mainPanel(
                 plotOutput("heatmapPlot")
               )
             )
    ),
    
    tabPanel("Speed by Lane",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("lane_choice", "Select Lane Type(s):",
                                choices = unique(df_cleaned$`Lane Type`),
                                selected = unique(df_cleaned$`Lane Type`),
                                multiple = TRUE,
                                options = list(placeholder = 'Choose lane(s)'))
               ),
               mainPanel(
                 plotOutput("boxplot")
               )
             )
    ),
    
    tabPanel("Traffic Map",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("map_stations", "Select Station(s):",
                                choices = unique(bubble_data$Station),
                                selected = unique(bubble_data$Station)[1:10],
                                multiple = TRUE),
                 sliderInput("flow_range", "Filter by Avg Flow:",
                             min = min(bubble_data$avg_flow),
                             max = max(bubble_data$avg_flow),
                             value = c(min(bubble_data$avg_flow), max(bubble_data$avg_flow)))
               ),
               mainPanel(
                 leafletOutput("trafficMap", height = 600)
               )
             )
    ),
    
    tabPanel("Delay by Direction",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("direction_choice", "Select Direction(s):",
                                choices = unique(df_cleaned$`Direction of Travel`),
                                selected = unique(df_cleaned$`Direction of Travel`),
                                multiple = TRUE),
                 sliderInput("delay_range", "Delay (V_t=45) Range:",
                             min = min(df_cleaned$`Delay (V_t=45)`),
                             max = max(df_cleaned$`Delay (V_t=45)`),
                             value = c(min(df_cleaned$`Delay (V_t=45)`), max(df_cleaned$`Delay (V_t=45)`)))
               ),
               mainPanel(
                 plotOutput("delayPlot")
               )
             )
    ),
    
    tabPanel("Chatbot",
             tags$div(style = "background:#f8f9fa; padding:20px; border-radius:10px; max-width:600px;",
                      h3("Ask the Traffic Bot"),
                      p("Try asking: 'What is the busiest hour?', 'Which station has the highest flow?', 'Which lane type is fastest?'"),
                      textInput("question", "Your Question:", placeholder = "Type here..."),
                      actionButton("submit", "Ask"),
                      br(), br(),
                      verbatimTextOutput("chat_response")
             )
    )
  )
)

# === Server ===
server <- function(input, output, session) {
  
  output$flow_delay_plot <- renderPlot({
    df_filtered <- df_cleaned %>%
      filter(`Direction of Travel` %in% input$flow_direction,
             Hour >= input$hour_slider[1], Hour <= input$hour_slider[2])
    
    df_flow <- df_filtered %>%
      group_by(Hour, `Direction of Travel`) %>%
      summarise(avg_flow = mean(`Total Flow`, na.rm = TRUE), .groups = "drop")
    
    df_delay <- df_filtered %>%
      group_by(Hour) %>%
      summarise(avg_delay = mean(`Delay (V_t=45)`, na.rm = TRUE), .groups = "drop") %>%
      mutate(scaled_delay = avg_delay * max(df_flow$avg_flow) / max(avg_delay))
    
    p <- ggplot() +
      geom_line(data = df_flow, aes(x = Hour, y = avg_flow, group = `Direction of Travel`),
                color = "#2c3e50", linewidth = 1.5)
    
    if (input$show_delay_line) {
      p <- p + geom_line(data = df_delay, aes(x = Hour, y = scaled_delay),
                         color = input$delay_color, linewidth = 1.2, alpha = 0.8) +
        annotate("text", x = 23, y = max(df_flow$avg_flow) * 0.95,
                 label = "Avg Delay (scaled)", color = input$delay_color, hjust = 1, fontface = "italic")
    }
    
    if (input$facet_choice == "Weekday") {
      df_flow <- df_filtered %>%
        group_by(Hour, Weekday) %>%
        summarise(avg_flow = mean(`Total Flow`, na.rm = TRUE), .groups = "drop")
      p <- ggplot(df_flow, aes(x = Hour, y = avg_flow)) +
        geom_line(color = "#2c3e50", linewidth = 1.5) +
        facet_wrap(~Weekday)
    } else if (input$facet_choice == "Month") {
      df_flow <- df_filtered %>%
        group_by(Hour, Month) %>%
        summarise(avg_flow = mean(`Total Flow`, na.rm = TRUE), .groups = "drop")
      p <- ggplot(df_flow, aes(x = Hour, y = avg_flow)) +
        geom_line(color = "#2c3e50", linewidth = 1.5) +
        facet_wrap(~Month)
    }
    
    p + labs(title = "Hourly Traffic Flow with Optional Delay Overlay",
             x = "Hour of Day", y = "Average Total Flow") +
      theme_minimal()
  })
  
  output$heatmapPlot <- renderPlot({
    req(input$hour_range, input$selected_days)
    
    heatmap_filtered <- df_cleaned %>%
      filter(Hour >= input$hour_range[1], Hour <= input$hour_range[2],
             Weekday %in% input$selected_days)
    
    heatmap_grid <- expand.grid(
      Hour = 0:23,
      Weekday = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    )
    
    heatmap_summary <- heatmap_filtered %>%
      group_by(Weekday, Hour) %>%
      summarise(avg_flow = mean(`Total Flow`, na.rm = TRUE), .groups = "drop")
    
    heatmap_data <- heatmap_grid %>%
      left_join(heatmap_summary, by = c("Weekday", "Hour")) %>%
      mutate(Weekday = factor(Weekday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))
    
    ggplot(heatmap_data, aes(x = Hour, y = Weekday, fill = avg_flow)) +
      geom_tile(color = "white", linewidth = 0.3) +
      scale_fill_gradient(low = "lightyellow", high = "darkred", na.value = "gray90", name = "Avg Flow") +
      labs(title = "Traffic Flow Heatmap", x = "Hour of Day", y = "Day of Week") +
      theme_minimal(base_size = 14)
  })
  
  output$boxplot <- renderPlot({
    filtered_box <- df_cleaned %>%
      filter(`Lane Type` %in% input$lane_choice)
    
    ggplot(filtered_box, aes(x = `Lane Type`, y = `Avg Speed`, fill = `Lane Type`)) +
      geom_boxplot(outlier.shape = 21, outlier.fill = "white", outlier.color = "black") +
      scale_fill_brewer(palette = "Set2") +
      labs(title = "Avg Speed by Lane Type", x = "Lane Type", y = "Speed (mph)") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
  })
  
  output$trafficMap <- renderLeaflet({
    filtered_bubbles <- bubble_data %>%
      filter(Station %in% input$map_stations,
             avg_flow >= input$flow_range[1],
             avg_flow <= input$flow_range[2])
    
    pal <- colorNumeric("YlOrRd", domain = filtered_bubbles$avg_speed)
    
    leaflet(filtered_bubbles) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude,
        radius = ~sqrt(avg_flow)/2.5,
        color = ~pal(avg_speed),
        fillOpacity = 0.85,
        label = ~paste0("Station: ", Station, "<br>",
                        "Avg Flow: ", round(avg_flow), "<br>",
                        "Avg Speed: ", round(avg_speed, 1), " mph"),
        labelOptions = labelOptions(direction = "auto")
      ) %>%
      addLegend("bottomright", pal = pal, values = ~avg_speed, title = "Avg Speed (mph)")
  })
  
  output$delayPlot <- renderPlot({
    filtered_delay <- df_cleaned %>%
      filter(`Direction of Travel` %in% input$direction_choice,
             `Delay (V_t=45)` >= input$delay_range[1],
             `Delay (V_t=45)` <= input$delay_range[2])
    
    ggplot(filtered_delay, aes(x = `Direction of Travel`, y = `Delay (V_t=45)`, fill = `Direction of Travel`)) +
      geom_boxplot(outlier.shape = 21, outlier.fill = "white", outlier.color = "black") +
      scale_fill_brewer(palette = "Dark2") +
      labs(title = "Delay Distribution by Direction", x = "Direction", y = "Avg Delay (V_t=45)") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
  })
  
  observeEvent(input$submit, {
    q <- tolower(input$question)
    
    if (grepl("busiest hour", q)) {
      busiest <- df_cleaned %>%
        filter(!is.na(Hour)) %>%
        group_by(Hour) %>%
        summarise(flow = mean(`Total Flow`, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(flow)) %>%
        slice(1)
      
      response <- if (nrow(busiest) > 0) {
        paste("The busiest hour is", busiest$Hour, "with average flow of", round(busiest$flow))
      } else {
        "No valid data found for busiest hour."
      }
      
    } else if (grepl("fastest lane", q)) {
      fastest <- df_cleaned %>%
        group_by(`Lane Type`) %>%
        summarise(speed = mean(`Avg Speed`, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(speed)) %>%
        slice(1)
      
      response <- paste("The fastest lane type is", fastest$`Lane Type`, "with average speed of", round(fastest$speed, 1), "mph.")
      
    } else if (grepl("highest flow", q)) {
      top_station <- bubble_data %>%
        arrange(desc(avg_flow)) %>%
        slice(1)
      
      response <- paste("Station", top_station$Station, "has the highest average flow of", round(top_station$avg_flow))
      
    } else {
      response <- "Sorry, I couldn't understand that. Try asking about 'busiest hour', 'fastest lane', or 'highest flow station'."
    }
    
    output$chat_response <- renderText({ response })
  })
}

# === Run the App ===
shinyApp(ui, server)
