# From Dietrich et al. 2021:
# Abundance was log-transformed (log10(x + 1)) to minimize the influence of
# highly abundant taxa in subsequent multivariate analysis. To minimize the
# influence of rare taxa, taxa were excluded if they did not occur in at least
# 5% of the net hauls. [predators sighted near >=5% of stations]
# ...
# A Bray-Curtis dissimilarity matrix based on abundance
# ...
# Separate hierarchical agglomerative cluster analyses of taxa abundance were
# performed with the R package cluster
# ...
# Ward’s minimum variance was selected as the linkage method due to having the
# highest agglomerative coefficient
# ...
# Indicator values were calculated for each taxa cluster combination based on
# formulas in Dufrene and Leg- endre (1997) and implemented in the R package
# labdsv

assign_sightings <- function(sightings, stations, max_dist_km) {
  stopifnot(inherits(sightings, "sf"),
            inherits(stations, "sf"))

  # Assign sightings to stations within year
  assign_group <- function(sightings_group, sightings_key) {
    # Note difference in column title Year vs year
    stations_group <- stations[stations$Year == sightings_key$year, ]
    # Find nearest station within year
    nearest_station <- st_join(sightings_group,
                               select(stations_group, amlr.station, date.tow),
                               join = st_nearest_feature)
    # Get station coordinates
    station_coords <- stations_group %>%
      slice(map_int(nearest_station$amlr.station,
                    ~ which(stations_group$amlr.station == .x)))
    # Calculate distance from predator sighting to nearest station
    nearest_station %>%
      mutate(dist_to_station = st_distance(nearest_station,
                                           station_coords,
                                           by_element = TRUE))
  }

  # Assign sightings across years
  sightings %>%
    group_by(year) %>%
    group_modify(assign_group) %>%
    ungroup() %>%
    filter(dist_to_station <= units::as_units(max_dist_km, "km")) %>%
    st_as_sf()
}

filter_sightings <- function(sightings, stations) {
  unk_sp <- c("UNSE", "UNPR", "UGPT", "UNPN")
  total_stations <- n_distinct(stations$amlr.station)
  retain <- sightings %>%
    group_by(species) %>%
    summarize(stations_present = n()) %>%
    mutate(station_frac = stations_present / total_stations) %>%
    filter(!species %in% unk_sp)
  semi_join(sightings, retain, by = "species")
}

normalize_counts <- function(sightings, effort, stations) {
  sightings %>%
    as_tibble() %>%
    group_by(amlr.station, species) %>%
    summarize(count = sum(count), .groups = "drop") %>%
    left_join(select(as_tibble(effort), amlr.station, survey_nmi),
              by = "amlr.station") %>%
    mutate(count_nmi = count / survey_nmi) %>%
    left_join(select(stations, amlr.station, Year), by = "amlr.station")
}

sightings_to_matrix <- function(sightings) {
  sightings_wide <- sightings %>%
    as_tibble() %>%
    pivot_wider(id_cols = amlr.station,
                names_from = species,
                values_from = count_nmi,
                values_fill = 0) %>%
    mutate(across(-amlr.station, ~ log(.x + 1)))
  sightings_mtx <- as.matrix(select(sightings_wide, -amlr.station))
  row.names(sightings_mtx) <- sightings_wide$amlr.station
  sightings_mtx
}
