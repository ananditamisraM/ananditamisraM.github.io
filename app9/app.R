library(shiny)
library(ggplot2)
library(plotly)


ui <- fluidPage(
  titlePanel("Comparison of share of women in different fields (1995 vs. 2014)"),
    mainPanel(
      plotlyOutput("barPlot")
    )
  )



server <- function(input, output) {
  df <- data.frame(
    Serial = 1:10,
    Field = c(
      "Economics", "Social science excluding econ", "Business and management",
      "Humanities", "STEM", "Economics", "Social science excluding econ",
      "Business and management", "Humanities", "STEM"
    ),
    Percentage = c(30.5, 46.7, 32.3, 51.1, 40.6, 31.4, 56.4, 42.8, 51.9, 55.7),
    Year = c(1995, 1995, 1995, 1995, 1995, 2014, 2014, 2014, 2014, 2014)
  )
  

  output$barPlot <- renderPlotly({
    gg <- ggplot(data = df, aes(x = Field, y = Percentage, fill = factor(Year), 
                                text = paste("Field: ", Field, "<br>Percentage: ", Percentage, "%<br>Year: ", Year))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        y = "Percentage of Women",
        x = "Fields of Work"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      guides(fill = guide_legend(title = "Year"))
    
    ggplotly(gg, tooltip = "text") %>%
      layout(
        showlegend = TRUE
      )
  })
}


shinyApp(ui, server)
