library(here)
library(targets)
library(tarchetypes)
library(tidyverse)

# Set target options:
tar_option_set(
  packages = c("readxl", "sf", "tidyverse"),
  format = "rds"
)

# Source scripts in R/
tar_source()

# Pipeline
list(
  # Download WAMLR data from OSF (if necessary)
  tar_target(
    wamlr_dir,
    check_wamlr(here("data", "WAMLR"))
  ),
  # Spatial data
  tar_target(
    ne_dir,
    here("data", "geospatial", "ne"),
    format = "file"
  ),
  tar_target(
    ne_data,
    download_ne(ne_dir)
  ),
  tar_target(
    ant_sf,
    create_ant_sf(ne_dir)
  ),
  # Predators
  tar_target(
    predators_file,
    here(wamlr_dir, "WAMLR-2012-2016-Underway-Predator-Sightings.csv"),
    format = "file"
  ),
  tar_target(predators, read_csv(predators_file)),
  tar_target(predators_agg, aggregate_predators(predators)),
  tar_target(predators_sf,
             latlon_to_sf(predators_agg, coords = c("lon_mean", "lat_mean"))),
  # Effort
  tar_target(effort,
             predators %>%
               filter(nmi < 1000) %>%
               mutate(year = lubridate::year(lubridate::mdy_hm(UTC_start))) %>%
               group_by(cruise, year, interval, lon_mean, lat_mean) %>%
               summarize(nmi = sum(nmi), .groups = "drop") %>%
               mutate(species = "survey", count_norm = 1)),
  tar_target(effort_sf,
             latlon_to_sf(effort, coords = c("lon_mean", "lat_mean"))),
  # Zooplankton
  tar_target(
    zoop_file,
    here(wamlr_dir, "WAMLR-2012-2016-Zooplnkton-Physics.xlsx"),
    format = "file"
  ),
  tar_target(zoop, read_zoop(zoop_file)),
  tar_target(zoop_sf,
             latlon_to_sf(zoop, coords = c("dec.longitude", "dec.latitude"))),
  tar_target(
    zoop_long,
    pivot_longer(zoop, -(1:53), names_to = "taxa", values_to = "abundance")
  ),
  # Ice observations
  tar_target(
    ice_file,
    here("data", "ice", "ice.csv"),
    format = "file"
  ),
  tar_target(
    ice_raw,
    read_csv(ice_file, na = c("", "NULL")) %>%
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
  ),
  # Aggregate predators and ice observations
  tar_target(
    station_effort,
    assign_sightings(effort_sf, zoop_sf, max_dist_km = 15) %>%
      group_by(year, amlr.station) %>%
      summarize(survey_nmi = sum(nmi), .groups = "drop")
  ),
  tar_target(
    predators_stations,
    assign_sightings(predators_sf, zoop_sf, max_dist_km = 15) %>%
      aggregate_ice(ice_raw) %>%
      left_join(station_effort %>%
                  as_tibble() %>%
                  select(amlr.station, survey_nmi),
                by = "amlr.station")
  ),
  tar_target(
    predators_abundant,
    filter_sightings(predators_stations, zoop_sf, station_thr = 0.05)
  ),
  # Clustering
  tar_target(sightings_mtx, sightings_to_matrix(predators_abundant)),
  tar_target(sightings_dist, vegan::vegdist(sightings_mtx, method = "bray")),
  tar_target(sightings_clust, hclust(sightings_dist, method = "ward.D2")),
  # Reports
  tar_quarto(reports, here("reports"))
)
