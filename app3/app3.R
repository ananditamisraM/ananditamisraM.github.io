library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

# Assuming your datasets are named 'dataset1' and 'dataset2'

# Define UI ----
ui <- fluidPage(
  titlePanel("Scatter Plot and Regression Line"),
  sidebarLayout(
    sidebarPanel(
      selectInput("position", "Choose Position", choices = c("All Positions", "Senior Positions"))
    ),
    mainPanel(
      plotlyOutput("scatter_plot")
    )
  )
)

new3_data <- read.csv("sample3_updated.csv")

# Define Server ----
server <- function(input, output) {
  
  # Create a scatter plot function
  create_scatter_plot <- function(data, x_var, y_var, color) {
    gg <- ggplot(data, aes_string(x = x_var, y = y_var)) +
      geom_point() +
      geom_smooth(method = "loess", se = FALSE, color = color) +
      labs(
        title = paste("Scatter Plot -", x_var, "vs", y_var),
        x = x_var,
        y = y_var
      ) +
      theme_minimal()
    
    ggplotly(gg, tooltip = "all")
  }
  
  # Render the scatter plot
  output$scatter_plot <- renderPlotly({
    position <- input$position
    if (position == "All Positions") {
      scatter_plot <- create_scatter_plot(new3_data, "rank_women_all_positions", "rank_gender_gap_index", "blue")
    } else {
      scatter_plot <- create_scatter_plot(new3_data, "rank_women_senior_positions", "rank_gender_gap_index", "red")
    }
    
    scatter_plot
  })
}

# Run Shiny app ----
shinyApp(ui, server)
