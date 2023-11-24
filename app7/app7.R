library(shiny)
library(dplyr)
library(maps)
library(ggplot2)
library(plotly)

new2_updated <- read.csv("sample2_updated2.csv")


region_data <- new2_updated %>%
  group_by(country) %>%
  summarize(
    total_senior_positions = sum(senior_positions),
    total_identified_positions = sum(identified_positions)
  )


world_map <- map_data("world")

merged_data <- merge(world_map, region_data, by.x = "region", by.y = "country", all.x = TRUE)


ui <- fluidPage(
  titlePanel("Distribution of Total Identified Positions by women by World Region"),
  plotlyOutput("world_map")
)

server <- function(input, output) {
  output$world_map <- renderPlotly({
    gg <- ggplot(merged_data, aes(x = long, y = lat, group = group, text = paste(region, "\nTotal Identified Positions: ", total_identified_positions))) +
      geom_polygon(fill = "white", color = "white") +
      geom_polygon(aes(fill = total_identified_positions), color = "white", size = 0.001) +
      scale_fill_gradient(low = "lightblue", high = "blue") +
      coord_fixed(ratio = 1.3) +
      labs(
        title = "Distribution of Total Identified Positions by World Region",
        x = "Longitude",
        y = "Latitude"
      )
    
    ggplotly(gg, tooltip = "text")
  })
}

shinyApp(ui, server)

