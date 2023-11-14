library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

# Assuming your dataset is called "new2_data"
new2_data <- read.csv("sample2_updated.csv")

summary2_data <- new2_data %>%
  group_by(institution_classification) %>%
  summarize(
    total_identified_positions = sum(identified_positions),
    total_senior_positions = sum(senior_positions)
  )

# Define UI ----
ui <- fluidPage(
  titlePanel("Total Identified and Senior Positions by Institution Classification"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("plot_type", "Select Plot Type:",
                   choices = c("Bars", "Dots", "Both"),
                   selected = "Both")
    ),
    mainPanel(
      plotlyOutput("custom_plot")
    )
  )
)

# Define Server ----
server <- function(input, output) {
  
  output$custom_plot <- renderPlotly({
    gg <- ggplot(data = summary2_data, aes(x = institution_classification)) +
      labs(
        title = "Total Identified and Senior Positions by Institution Classification",
        y = "Count",
        x = "Institution Classification"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    if ("Bars" %in% input$plot_type || "Both" %in% input$plot_type) {
      gg <- gg + geom_bar(aes(y = total_identified_positions), stat = "identity", fill = "blue", width = 0.5)
    }
    
    if ("Dots" %in% input$plot_type || "Both" %in% input$plot_type) {
      gg <- gg + geom_point(aes(y = total_senior_positions), color = "red", size = 3)
    }
    
    ggplotly(gg, tooltip = "all")
  })
}

# Run Shiny app ----
shinyApp(ui, server)

