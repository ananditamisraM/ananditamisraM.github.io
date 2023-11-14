library(shiny)
library(dplyr)
library(maps)
library(ggplot2)

new2_data <- read.csv("sample2_updated.csv")
# Assuming your dataset is called "new2_data"
# Group by "country" and calculate the sums
region_data <- new2_data %>%
  group_by(country) %>%
  summarize(
    total_senior_positions = sum(senior_positions),
    total_identified_positions = sum(identified_positions)
  )

# Get world map data
world_map_simplified <- map_data("world")

# Check for duplicates in "country"
if (any(duplicated(region_data$country))) {
  stop("Duplicates found in 'country'. Please ensure unique values.")
}

# Merge your summarized data with the world map data
merged_data <- merge(world_map_simplified, region_data, by.x = "region", by.y = "country", all.x = TRUE)

# Define UI ----
ui <- fluidPage(
  titlePanel("Distribution of Identified Positions by Country"),
  mainPanel(
    plotOutput("world_map", hover = hoverOpts("plot_hover"))
  )
)

# Define Server ----
server <- function(input, output) {
  
  output$world_map <- renderPlot({
    ggplot(merged_data, aes(x = long, y = lat, group = group)) +
      geom_polygon(fill = "white", color = "white") +
      geom_polygon(aes(fill = total_identified_positions), color = "white", size = 0.001) +
      scale_fill_gradient(low = "lightpink", high = "red") +
      coord_fixed(ratio = 1.3) +
      labs(
        title = "Distribution of Identified Positions by Country",
        x = "Longitude",
        y = "Latitude"
      ) +
      geom_text(aes(x = long, y = lat, label = country), size = 2) +
      theme_minimal()
  })
  
  output$plot_hover <- renderPrint({
    nearPoints(merged_data, input$world_map_hover, threshold = 5, maxpoints = 1, addDist = TRUE)
  })
}

# Run Shiny app ----
shinyApp(ui, server)

