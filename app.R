library(shiny)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(readr)


ui <- fluidPage(
  titlePanel("Kernel Density Plots for Percentage of Women in Different Country Groups"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country_group", "Select Country Group:",
                  choices = c("Europe+Great Britain", "United States+Canada"),
                  selected = "Europe+Great Britain")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("All Positions", plotOutput("all_positions")),
        tabPanel("Senior Level", plotOutput("senior_level")),
        tabPanel("Entry Level", plotOutput("entry_level"))
      )
    )
  )
)


server <- function(input, output) {
  
  create_density_plot <- function(data, title) {
    ggplot(data, aes(x = female_ratio, color = rank)) +
      geom_density(fill = "blue", alpha = 0.5) +
      labs(
        title = title,
        x = "Percentage of Women",
        y = "Density"
      ) +
      scale_color_manual(values = c("Top 1-119" = "red", "Top 120-238" = "blue")) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      guides(color = guide_legend(title = "Rank Group")) +
      expand_limits(y = 0)
  }
  
  new1_data <- read_csv("sample1_updated.csv")
  
  observe({
    if (input$country_group == "Europe+Great Britain") {
      country_data <- new1_data %>% filter(country %in% c(
        "United Kingdom of Great Britain and Northern Ireland", "France", "Italy", "Spain",
        "Switzerland", "Belgium", "Netherlands", "Germany", "Romania", "Denmark",
        "Sweden", "Norway", "Czechia", "Austria", "Greece", "Poland", "Ireland",
        "Portugal", "Luxembourg", "Hungary"
      ))
    } else {
      country_data <- new1_data %>% filter(country %in% c("United States of America", "Canada"))
    }
    
    all_positions_data <- country_data %>%
      filter(level == "All Positions") %>%
      mutate(rank = ifelse(rank <= 119, "Top 1-119", "Top 120-238"))
    
    output$all_positions <- renderPlot({
      create_density_plot(all_positions_data, paste(input$country_group, "- All Positions"))
    })
    
    senior_level_data <- country_data %>%
      filter(level == "Senior Level") %>%
      mutate(rank = ifelse(rank <= 119, "Top 1-119", "Top 120-238"))
    
    output$senior_level <- renderPlot({
      create_density_plot(senior_level_data, paste(input$country_group, "- Senior Level"))
    })
    
    entry_level_data <- country_data %>%
      filter(level == "Entry Level") %>%
      mutate(rank = ifelse(rank <= 119, "Top 1-119", "Top 120-238"))
    
    output$entry_level <- renderPlot({
      create_density_plot(entry_level_data, paste(input$country_group, "- Entry Level"))
    })
  })
}


shinyApp(ui, server)

