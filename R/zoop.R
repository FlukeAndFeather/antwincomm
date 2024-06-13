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
           time.of.day = factor(ifelse(sun_angle > -6, "Day", "Night"))) %>%
    filter(leg == "W")
}

tod_pal <- function() {
  c(Day = "gold",
    Night = "darkblue")
}
