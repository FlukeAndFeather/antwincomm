source(here::here("reports", "05_synthesis", "util.R"))

ui <- fillPage(

  fillRow(
    # Left column: map and env histogram
    fillCol(
      girafeOutput("community_map", height = "100%"),
      div(
        selectInput("env_var",
                    "Environmental variable",
                    choices = list("Salinity" = "avg.salinity",
                                   "Ice coverage" = "ice_coverage",
                                   "Int. chl a" = "Integ.chla.100m"),
                    selected = "Salinity"),
        plotOutput("env_hist", height = "150px")
      ),
      flex = c(1, NA)
    ),

    # Right column: tree, station table, sightings table
    fillCol(
      plotOutput("cluster_dendro", height = "100%"),
      DT::dataTableOutput("station_table"),
      DT::dataTableOutput("sightings_table"),
      flex = c(3, 2, 3)
    ),

    flex = c(2, 1)
  )

)
