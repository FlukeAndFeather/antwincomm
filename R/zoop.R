#' Read station (zooplankton) data
#'
#' @param zoop_path path to StationZooplanktonPhysics.csv
#'
#' @return data frame with station-level data
#' @export
read_zoop <- function(zoop_path) {
  sun_angle <- function(t, x, y) {
    pmap_dbl(list(t, x, y), \(.t, .x, .y) oce::sunAngle(.t, .x, .y)$altitude)
  }

  read_csv(zoop_path, show_col_types = FALSE) %>%
    mutate(sun_angle = sun_angle(start.time.UTC, dec.longitude, dec.latitude),
           time.of.day = cut(sun_angle,
                             c(-Inf, -18, 0, Inf),
                             labels = c("Night", "Twilight", "Day"))) %>%
    filter(leg == "W")
}

tod_pal <- function() {
  c(Day = "gold",
    Twilight = "violet",
    Night = "darkblue")
}
