# Load this research compendium as a package
# This is functionally equivalent to library(antwincomm)
devtools::load_all()

# Check raw data have been downloaded
if (!all(file.exists(file.path(
  "analysis", "data", "raw_data", "AntPeninsula_winter_marine_data",
  c("Ice.csv", "PredatorSightings.csv", "StationZooplanktonPhysics.csv")
)))) {
  stop("RAW DATA NOT FOUND. See: analysis/data/raw_data/README.md.")
}

# Predator data
predators_file <- "analysis/data/raw_data/AntPeninsula_winter_marine_data/PredatorSightings.csv"
predators <- read_csv(predators_file, show_col_types = FALSE)
predators_agg <- aggregate_predators(predators)
save_obj(predators_agg, "predators_agg")

message("Predator data processed")

# Effort data
effort_sf <- predators %>%
  distinct(cruise, interval, UTC_start, UTC_end, nmi,
           lon_mean, lat_mean) %>%
  filter(nmi < 1000) %>%
  mutate(year = lubridate::year(UTC_start),
         species = "survey",
         km = nmi_to_km(nmi)) %>%
  latlon_to_sf(coords = c("lon_mean", "lat_mean"))
save_obj(effort_sf, "effort_sf")

message("Effort data processed")

# Natural Earth data
ant_sf <- create_ant_sf(ne_dir = "analysis/data/derived_data/ne")
save_obj(ant_sf, "ant_sf")

message("Natural Earth data processed")
