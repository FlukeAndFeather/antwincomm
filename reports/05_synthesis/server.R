source(here::here("reports", "05_synthesis", "util.R"))

server <- function(input, output) {

  # Left column
  output$community_map <- renderPlot(make_community_map(highlighted = NULL))
  output$env_hist <- renderPlot(make_env_hist(input$env_var, highlighted = NULL))

  # Right column
  output$cluster_dendro <- renderPlot(make_cluster_dendro(highlighted = NULL))
  output$station_table <- DT::renderDataTable(make_station_table(highlighted = NULL))
  output$sightings_table <- DT::renderDataTable(make_sightings_table(highlighted = NULL))

}
