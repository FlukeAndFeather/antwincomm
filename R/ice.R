# Most frequent value i.e., mode
# Note: returns a character
mfv <- function(x) {
  x_tbl <- table(x)
  names(x_tbl)[which.max(x_tbl)]
}

# Clean ice categories
# Converts e.g., FY/MY to FY and collapses SLUSH and BRASH into THIN
clean_ice <- function(ice_types) {
  str_extract(ice_types, "[^/]+") %>%
    factor(levels = c("OPEN",
                      "SLUSH",
                      "BRASH",
                      "THIN",
                      "FY",
                      "MY",
                      "NULL")) %>%
    fct_collapse(THIN = "SLUSH",
                 THIN = "BRASH")
}

# Aggregates ice data:
# ice_type [fct] OPEN, THIN, FY, MY
# ice_intervals [int] count of intervals with ice obs associated with station
# ice_type_freq [dbl] fraction of intervals assoc w station with modal ice type
# ice_coverage [dbl] mean of ice coverage code
aggregate_ice <- function(stations, ice_raw) {
  stations2 <- stations %>%
    transmute(amlr.station,
              year_station = Year,
              date_station = as.Date(date.tow))

  # Spatially join stations to raw ice observations
  ice_stations <- ice_raw %>%
    group_by(year_ice) %>%
    group_modify(function(ice_by_year, ice_keys) {
      stations_by_year <- filter(stations2, year_station == ice_keys$year_ice)
      nearest_stations_idx <- st_nearest_feature(ice_by_year, stations_by_year)
      nearest_stations <- stations_by_year[nearest_stations_idx, ]
      station_distance <- st_distance(ice_by_year,
                                      nearest_stations,
                                      by_element = TRUE)
      station_lag <- ice_by_year$date_ice - nearest_stations$date_station
      stations_no_sf <- select(as_tibble(nearest_stations), -geometry)
      ice_by_year %>%
        cbind(stations_no_sf) %>%
        mutate(station_distance = station_distance,
               station_lag = station_lag)
    }) %>%
    ungroup() %>%
    filter(station_distance <= units::as_units(15, "km"),
           abs(station_lag) <= as.difftime(3, units = "days")) %>%
    st_as_sf(sf_column_name = "geometry")

  # Aggregate ice observations by station
  ice_stations %>%
    group_by(amlr.station, year = year_ice) %>%
    summarize(modal_ice_type = mfv(ice_type),
              ice_intervals = n(),
              ice_type_freq = sum(ice_type == modal_ice_type),
              ice_coverage = mean(ice_code, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(ice_type = factor(modal_ice_type, levels = c("OPEN",
                                                        "THIN",
                                                        "FY",
                                                        "MY"))) %>%
    as_tibble() %>%
    select(amlr.station, ice_type, ice_intervals, ice_type_freq, ice_coverage)
}

collect_seaice <- function(seaice_dir) {
  seaice <- seaice_dir %>%
    dir(full.names = TRUE) %>%
    map(rast) %>%
    rast()
  set.names(seaice, as.character(2012:2016))

  proj_ext <- ext(-67, -50, -68, -55) %>%
    project(from = "epsg:4326",
            to = crs(seaice))
  wrap(crop(seaice, proj_ext))
}

seaice2contour <- function(seaice_rast, contour_level) {
  seaice_rast <- unwrap(seaice_rast)
  map(
    seq(nlyr(seaice_rast)),
    ~ as.contour(seaice_rast[[.x]],
                 levels = contour_level * 1000)
  ) %>%
    vect() %>%
    mutate(Year = 2012:2016,
           ice_conc = contour_level) %>%
    wrap()
}

seaice2df <- function(seaice_rast) {
  seaice_rast <- unwrap(seaice_rast)
  proj_ext <- ext(-67, -50, -68, -55) %>%
    project(from = "epsg:4326",
            to = crs(seaice_rast))
  template_ext <- ext(-67, -50, -68, -55) %>%
    project(from = "epsg:4326",
            to = as.character(ant_proj())[[1]])
  rast_template <- rast(crs = as.character(ant_proj())[[1]],
                        extent = template_ext,
                        resolution = 25e3)
  project(seaice_rast, rast_template) %>%
    as.data.frame(xy = TRUE) %>%
    pivot_longer(as.character(2012:2016),
                 names_to = "Year",
                 values_to = "seaice_conc") %>%
    mutate(seaice_conc = seaice_conc / 1000)
}
