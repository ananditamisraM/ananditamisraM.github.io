library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

# Assuming 'new1_data' is already loaded
new1_data <- read.csv("sample1_updated.csv")

# Define a function to categorize countries into groups
categorize_country <- function(country) {
  europe_gb <- c(
    "United Kingdom of Great Britain and Northern Ireland",
    "France", "Italy", "Spain", "Switzerland", "Belgium", "Netherlands",
    "Germany", "Romania", "Denmark", "Sweden", "Norway", "Czechia",
    "Austria", "Greece", "Poland", "Ireland", "Portugal", "Luxembourg",
    "Hungary"
  )
  us_canada <- c("United States of America", "Canada")
  aus_nz <- c("Australia", "New Zealand")
  if (country %in% europe_gb) {
    return("Europe+Great Britain")
  } else if (country %in% us_canada) {
    return("United States+Canada")
  } else if (country %in% aus_nz) {
    return("Australia and New Zealand")
  } else {
    return("Rest of the World")
  }
}

# Add a new column for country groups
new1_data <- new1_data %>% mutate(country_group = sapply(country, categorize_country))

# Filter data for "Entry Level" and "Senior Level" and calculate total counts
filtered_data <- new1_data %>%
  filter(level %in% c("Entry Level", "Senior Level")) %>%
  group_by(country_group, level) %>%
  summarise(total_female = sum(female_ratio), .groups = 'drop')  # Add .groups argument to override grouping

# Reorder the levels of country_group
filtered_data$country_group <- factor(filtered_data$country_group, levels = c(
  "Europe+Great Britain", "United States+Canada", "Australia and New Zealand", "Rest of the World"
))

# Create UI ----
ui <- fluidPage(
  titlePanel("Total Female Count by Region"),
  sidebarLayout(
    sidebarPanel(
      selectInput("level", "Select Level:",
                  choices = c("Entry Level", "Senior Level", "Both Lines"),
                  selected = "Both Lines")
    ),
    mainPanel(
      plotlyOutput("line_chart")
    )
  )
)

# Create Server ----
server <- function(input, output) {
  
  output$line_chart <- renderPlotly({
    # Filter data based on selected level
    if (input$level == "Both Lines") {
      selected_data <- filtered_data
    } else {
      selected_data <- filtered_data %>% filter(level == input$level)
    }
    
    # Create the line chart with rotated x-axis labels
    gg <- ggplot(selected_data, aes(x = country_group, y = total_female, color = level, group = level)) +
      geom_line() +
      labs(
        title = paste("Total Female Count at", input$level, "Level by Region"),
        x = "Region",
        y = "Total Female Count",
        color = "Level"
      ) +
      scale_color_manual(values = c("Entry Level" = "blue", "Senior Level" = "red")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(gg, tooltip = "all")
  })
}

# Run Shiny app ----
shinyApp(ui, server)

