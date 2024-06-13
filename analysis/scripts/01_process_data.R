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

# Predator data -----------------------------------------------------------
predators_file <- "analysis/data/raw_data/AntPeninsula_winter_marine_data/PredatorSightings.csv"
predators <- read_csv(predators_file, show_col_types = FALSE) %>%
  mutate(year = lubridate::year(UTC_start))
predators_sf <- latlon_to_sf(predators, coords = c("lon_mean", "lat_mean"))
predators_agg <- aggregate_predators(predators)
save_obj(predators_sf, "predators_sf")
save_obj(predators_agg, "predators_agg")

message("Predator data processed")


# Effort data -------------------------------------------------------------
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


# Zooplankton data --------------------------------------------------------
zoop_file <- "analysis/data/raw_data/AntPeninsula_winter_marine_data/StationZooplanktonPhysics.csv"
zoop <- read_zoop(zoop_file)
zoop_sf <- latlon_to_sf(zoop, coords = c("dec.longitude", "dec.latitude"))
save_obj(zoop_sf, "zoop_sf")

message("Zooplankton data processed")


# Ice data ----------------------------------------------------------------

ice_file <- "analysis/data/raw_data/AntPeninsula_winter_marine_data/Ice.csv"
ice_raw <- read_csv(ice_file, na = c("", "NULL")) %>%
  transmute(year_ice = Year,
            lat = Latitude,
            lon = Longitude,
            ice_code = as.numeric(IceCode),
            ice_type = clean_ice(IceType),
            date_ice = as.Date(paste(Year, Month, Day, sep = "-"))) %>%
  filter(ice_type != "NULL",
         # 1 ice_code is "85"
         ice_code <= 10) %>%
  latlon_to_sf(coords = c("lon", "lat"))
save_obj(ice_raw, "ice_raw")

seaice_dir <- "analysis/data/raw_data/Aug sea ice concentration"
seaice_rast <- collect_seaice(seaice_dir)
seaice_contours <- seaice2contour(seaice_rast, 0.65)
seaice_conc_df <- seaice2df(seaice_rast)
save_obj(seaice_rast, "seaice_rast", type = "rast")
save_obj(seaice_contours, "seaice_contours", type = "vect")
save_obj(seaice_conc_df, "seaice_conc_df")

message("Ice data processed")


# Natural Earth data ------------------------------------------------------
ant_sf <- create_ant_sf(ne_dir = "analysis/data/derived_data/ne")
save_obj(ant_sf, "ant_sf")

message("Natural Earth data processed")


# Associate effort, ice, and predator data with stations ------------------
station_effort <- assign_sightings(effort_sf,
                                   zoop_sf,
                                   max_dist_km = 15,
                                   max_days = 3) %>%
    group_by(year, amlr.station) %>%
    summarize(survey_nmi = sum(nmi),
              survey_km = nmi_to_km(survey_nmi),
              .groups = "drop")
stations_ice <- aggregate_ice(zoop_sf, ice_raw)
predators_stations <- predators_sf %>%
  assign_sightings(zoop_sf, max_dist_km = 15, max_days = 3) %>%
  normalize_counts(station_effort) %>%
  filter_species(station_thr = 0.00)
save_obj(station_effort, "station_effort")
save_obj(stations_ice, "stations_ice")
save_obj(predators_stations, "predators_stations")

message("Station and track data associated")
