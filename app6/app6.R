library(shiny)
library(dplyr)
library(maps)
library(ggplot2)
library(plotly)

new2_data <- read.csv("sample2_updated.csv")

# Assuming your dataset is called "df"
# Group by "world_region" and calculate the sums
region_data <- new2_data %>%
  group_by(country) %>%
  summarize(
    total_senior_positions = sum(senior_positions),
    total_identified_positions = sum(identified_positions)
  )

# Get world map data
world_map <- map_data("world")

# Check for duplicates in "world_region"
if (any(duplicated(region_data$world_region))) {
  stop("Duplicates found in 'world_region'. Please ensure unique values.")
}

# Merge your summarized data with the world map data
merged_data <- merge(world_map, region_data, by.x = "region", by.y = "country", all.x = TRUE)

# Create a Shiny app
ui <- fluidPage(
  titlePanel("Distribution of Senior Positions by World Region"),
  plotlyOutput("world_map")
)

server <- function(input, output) {
  # Create the world map
  output$world_map <- renderPlotly({
    gg <- ggplot(merged_data, aes(x = long, y = lat, group = group, text = paste(region, "\nTotal Senior Positions: ", total_senior_positions))) +
      geom_polygon(fill = "white", color = "white") +
      geom_polygon(aes(fill = total_senior_positions), color = "white", size = 0.001) +
      scale_fill_gradient(low = "lightblue", high = "blue") +
      coord_fixed(ratio = 1.3) +
      labs(
        title = "Distribution of Senior Positions by World Region",
        x = "Longitude",
        y = "Latitude"
      )
    
    ggplotly(gg, tooltip = "text")
  })
}

shinyApp(ui, server)

