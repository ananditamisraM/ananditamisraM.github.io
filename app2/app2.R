library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(gridExtra)
library(readr)


ui <- fluidPage(
  titlePanel("Institution Frequency by Rank"),
    mainPanel(
      plotlyOutput("interactive_plot")
    )
  )

new1_data <- read_csv("sample1_updated.csv")

europe_great_britain <- c(
  "United Kingdom of Great Britain and Northern Ireland", "France", "Italy", "Spain",
  "Switzerland", "Belgium", "Netherlands", "Germany", "Romania", "Denmark",
  "Sweden", "Norway", "Czechia", "Austria", "Greece", "Poland", "Ireland",
  "Portugal", "Luxembourg", "Hungary"
)
us_canada <- c("United States of America", "Canada")


europe_great_britain_data <- new1_data %>% filter(country %in% europe_great_britain)
us_canada_data <- new1_data %>% filter(country %in% us_canada)


plot_data <- rbind(
  data.frame(rank = "Top 50", country_group = "Europe+Great Britain", freq = length(unique(europe_great_britain_data$rank[europe_great_britain_data$rank <= 50]))),
  data.frame(rank = "Top 50", country_group = "United States+Canada", freq = length(unique(us_canada_data$rank[us_canada_data$rank <= 50]))),
  data.frame(rank = "Top 100", country_group = "Europe+Great Britain", freq = length(unique(europe_great_britain_data$rank[europe_great_britain_data$rank <= 100]))),
  data.frame(rank = "Top 100", country_group = "United States+Canada", freq = length(unique(us_canada_data$rank[us_canada_data$rank <= 100]))),
  data.frame(rank = "Top 150", country_group = "Europe+Great Britain", freq = length(unique(europe_great_britain_data$rank[europe_great_britain_data$rank <= 150]))),
  data.frame(rank = "Top 150", country_group = "United States+Canada", freq = length(unique(us_canada_data$rank[us_canada_data$rank <= 150]))),
  data.frame(rank = "Top 200", country_group = "Europe+Great Britain", freq = length(unique(europe_great_britain_data$rank[europe_great_britain_data$rank <= 200]))),
  data.frame(rank = "Top 200", country_group = "United States+Canada", freq = length(unique(us_canada_data$rank[us_canada_data$rank <= 200]))),
  data.frame(rank = "Top 238", country_group = "Europe+Great Britain", freq = length(unique(europe_great_britain_data$rank))),
  data.frame(rank = "Top 238", country_group = "United States+Canada", freq = length(unique(us_canada_data$rank)))
)


server <- function(input, output) {
  
  plot_data$rank <- factor(plot_data$rank, levels = c("Top 50", "Top 100", "Top 150", "Top 200", "Top 238"))
  
  base_plot <- ggplot(plot_data, aes(x = rank, y = freq, fill = country_group)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    labs(
      title = "Institution Frequency by Rank",
      x = "Rank Category",
      y = "Frequency"
    ) +
    scale_fill_manual(values = c("Europe+Great Britain" = "blue", "United States+Canada" = "red")) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5)
    ) +
    guides(fill = guide_legend(title = "Country Group"))
  
  output$interactive_plot <- renderPlotly({
    ggplotly(base_plot)
  })
}


shinyApp(ui, server)

