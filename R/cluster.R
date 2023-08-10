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

# Normalize species counts by survey effort
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

# Convert sightings data frame to matrix
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

# Generate gap statistic curve for choosing k
cluster_gap <- function(mtx, max_k, seed = 139) {
  cluster_fn <- function(x, k) {
    list(cluster = stats::cutree(hclust(vegan::vegdist(x, method = "bray"),
                                        method = "ward.D2"),
                                 k = k))
  }
  set.seed(seed)
  gap <- suppressWarnings(
    cluster::clusGap(mtx,
                     FUN = cluster_fn,
                     K.max = 10,
                     B = 500,
                     verbose = FALSE)
  )
  as_tibble(gap$Tab) %>%
    mutate(
      k = seq_along(logW),
      is_optimum = k == cluster::maxSE(gap, SE.sim, method = "Tibs2001SEmax")
    )
}

# Create a predator cluster factor from a vector of stations and the cut tree
station_to_cluster <- function(station, clust) {
  clust_int <- clust[station]
  factor(clust_int, levels = 1:3, labels = c("Open", "Marginal", "Pagophilic"))
}

# Determine if a species is rare (<5% of stations)
is_rare <- function(species, station) {
  n_station <- n_distinct(station)
  species_rarity <- tapply(station, species, length) / n_station < 0.05
  species_rarity[species]
}
