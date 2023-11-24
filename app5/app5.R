library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

new1_data <- read.csv("sample1_updated.csv")

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

new1_data <- new1_data %>% mutate(country_group = sapply(country, categorize_country))


filtered_data <- new1_data %>%
  filter(level %in% c("Entry Level", "Senior Level")) %>%
  group_by(country_group, level) %>%
  summarise(total_female = sum(female_ratio), .groups = 'drop')  


filtered_data$country_group <- factor(filtered_data$country_group, levels = c(
  "Europe+Great Britain", "United States+Canada", "Australia and New Zealand", "Rest of the World"
))

ui <- fluidPage(
  titlePanel("Total Female Count by Region- Entry Level vs Senior Level"),
  sidebarLayout(
    sidebarPanel(
      selectInput("level", "Select Level:",
                  choices = c("Entry Level", "Senior Level", "Both Levels"),
                  selected = "Both Levels")
    ),
    mainPanel(
      plotlyOutput("line_chart")
    )
  )
)


server <- function(input, output) {
  
  output$line_chart <- renderPlotly({
    if (input$level == "Both Levels") {
      selected_data <- filtered_data
    } else {
      selected_data <- filtered_data %>% filter(level == input$level)
    }
    
    gg <- ggplot(selected_data, aes(x = country_group, y = total_female, fill = level)) +
      geom_bar(stat = "identity", position = "identity", alpha = 0.7) + 
      labs(
        title = paste("Total Female Count at", input$level, "by Region"),
        x = "Region",
        y = "Total Female Count",
        fill = "Level"
      ) +
      scale_fill_manual(values = c("Entry Level" = "blue", "Senior Level" = "red")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(gg, tooltip = "all")
  })
}


shinyApp(ui, server)

