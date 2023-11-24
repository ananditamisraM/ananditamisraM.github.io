library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)



ui <- fluidPage(
  titlePanel("Scatter Plot of Gender Gap Index Rank vs. Rank by Share of Women"),
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


server <- function(input, output) {

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


shinyApp(ui, server)
