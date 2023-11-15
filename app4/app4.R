library(shiny)
library(ggplot2)
library(dplyr)

# Assuming 'new1_data' is already loaded
new1_data <- read.csv("sample1_updated.csv")
# Filter the data for the top 119 and top 120-238 institutions
top_119_data <- new1_data %>% filter(rank %in% 1:119)
top_120_238_data <- new1_data %>% filter(rank %in% 120:238)

# Create UI ----
ui <- fluidPage(
  titlePanel("Kernel Density Plots"),
  sidebarLayout(
    sidebarPanel(
      selectInput("level", "Select Level:",
                  choices = c("All Positions", "Entry Level", "Senior Level"),
                  selected = "All Positions")
    ),
    mainPanel(
      plotOutput("density_plot")
    )
  )
)

# Create Server ----
server <- function(input, output) {
  
  # ...
  
  # ...
  
  output$density_plot <- renderPlot({
    # Filter data based on selected level
    selected_data <- switch(input$level,
                            "All Positions" = new1_data,
                            "Entry Level" = new1_data %>% filter(level == "Entry Level"),
                            "Senior Level" = new1_data %>% filter(level == "Senior Level"))
    
    # Create kernel density plots for each category
    ggplot() +
      geom_density(data = selected_data %>% filter(rank %in% 1:119),
                   aes(x = female_ratio, fill = "Top 119"), alpha = 0.5) +
      geom_density(data = selected_data %>% filter(rank %in% 120:238),
                   aes(x = female_ratio, fill = "Top 120-238"), alpha = 0.5) +
      labs(
        title = paste("Kernel Density Plot of Female Ratio by Institution Rank -", input$level),
        x = "Percentage of Women",
        y = "Density"
      ) +
      scale_fill_manual(values = c("Top 119" = "blue", "Top 120-238" = "red")) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      guides(fill = guide_legend(title = "Rank Group")) +
      coord_cartesian(xlim = c(0, 100), ylim = c(0, 0.06))  # Adjust xlim and ylim as needed
  })
  
  
}

# Run Shiny app ----
shinyApp(ui, server)

