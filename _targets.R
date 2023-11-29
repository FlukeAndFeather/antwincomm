library(here)
library(targets)
library(tarchetypes)
library(tidyverse)

# Set target options:
tar_option_set(
  packages = c("readxl", "sf", "terra", "tidyterra", "tidyverse"),
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
  tar_target(
    effort,
    predators %>%
      distinct(cruise, processID, interval, UTC_start, UTC_end, nmi,
               lon_mean, lat_mean) %>%
      filter(nmi < 1000) %>%
      mutate(UTC_start = lubridate::mdy_hm(UTC_start),
             year = lubridate::year(UTC_start),
             species = "survey",
             km = nmi_to_km(nmi))
  ),
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
  # Satellite sea ice data
  tar_target(
    seaice_dir,
    here("data", "ice", "Aug sea ice concentration"),
    format = "file"
  ),
  tar_target(seaice_rast, collect_seaice(seaice_dir)),
  tar_target(seaice_contours, seaice2contour(seaice_rast, 0.65)),
  tar_target(seaice_conc_df, seaice2df(seaice_rast)),
  # Aggregate effort and ice observations
  tar_target(
    station_effort,
    assign_sightings(effort_sf, zoop_sf, max_dist_km = 15, max_days = 3) %>%
      group_by(year, amlr.station) %>%
      summarize(survey_nmi = sum(nmi), .groups = "drop")
  ),
  tar_target(
    stations_ice,
    aggregate_ice(zoop_sf, ice_raw)
  ),
  tar_target(
    predators_stations,
    assign_sightings(predators_sf, zoop_sf, max_dist_km = 15, max_days = 3) %>%
      normalize_counts(station_effort) %>%
      filter_species(station_thr = 0.00)
  ),
  # Clustering
  tar_target(sightings_mtx, sightings_to_matrix(predators_stations)),
  tar_target(sightings_dist, vegan::vegdist(sightings_mtx, method = "bray")),
  tar_target(sightings_clust, hclust(sightings_dist, method = "ward.D2")),
  tar_target(sightings_gap, cluster_gap(sightings_mtx, max_k = 10, seed = 139)),
  tar_target(sightings_cut, cutree(sightings_clust, k = 3)),
  tar_target(sightings_indval, labdsv::indval(sightings_mtx, sightings_cut)),
  tar_target(
    predators_clust,
    mutate(predators_stations,
           pred_clust = station_to_cluster(amlr.station, sightings_cut))
  ),
  tar_target(
    stations_clust,
    zoop_sf %>%
      left_join(stations_ice, by = "amlr.station") %>%
      mutate(pred_clust = station_to_cluster(amlr.station, sightings_cut)) %>%
      semi_join(predators_clust, by = "amlr.station")
  ),
  # NMDS
  tar_target(nmds_stress, stress(sightings_mtx, k = 1:6)),
  tar_target(nmds_sightings,
             vegan::metaMDS(
               sightings_dist,
               k = 3,
               trymax = 100,
               trace = FALSE)),
  tar_target(nmds_shepard, shepard(nmds_sightings)),
  tar_target(nmds_env, env_data(zoop, stations_ice, predators_stations)),
  tar_target(nmds_df, nmds_to_df(nmds_sightings, sightings_cut, nmds_env)),
  tar_target(nmds_envfit, vegan::envfit(nmds_sightings,
                                        select(nmds_env, -amlr.station),
                                        permutations = 999,
                                        choices = 1:3,
                                        na.rm = TRUE)),
  # Reports
  tar_quarto(reports, here("reports"))
)
