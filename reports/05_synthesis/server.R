source(here::here("reports", "05_synthesis", "util.R"))

server <- function(input, output) {

  # Left column
  output$community_map <- renderGirafe(make_community_map())
  output$env_hist <- renderPlot(
    make_env_hist(input$env_var, highlighted = input$community_map_selected)
  )

  # Right column
  output$cluster_dendro <- renderPlot(
    make_cluster_dendro(highlighted = input$community_map_selected)
  )
  output$station_table <- renderText(
    make_station_table(highlighted = input$community_map_selected)
  )
  output$sightings_table <- renderTable(
    make_sightings_table(highlighted = input$community_map_selected)
  )

}
