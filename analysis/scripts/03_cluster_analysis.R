# Load this research compendium as a package
# This is functionally equivalent to library(antwincomm)
devtools::load_all()

# Load necessary derived data
predators_stations <- read_obj("predators_stations")
effort_sf <- read_obj("effort_sf")
zoop_sf <- read_obj("zoop_sf")
stations_ice <- read_obj("stations_ice")

# Cluster sites by predator sightings
sightings_mtx <- sightings_to_matrix(predators_stations)
sightings_dist <- vegan::vegdist(sightings_mtx, method = "bray")
sightings_clust <- hclust(sightings_dist, method = "ward.D2")
sightings_gap <- cluster_gap(sightings_mtx, max_k = 10, seed = 139)
sightings_cut <- cutree(sightings_clust, k = 3)

# Indicator values
sightings_indval <- labdsv::indval(sightings_mtx, sightings_cut)

# Associate predators and sites with predator clusters
predators_clust <- mutate(
  predators_stations,
  pred_clust = station_to_cluster(amlr.station, sightings_cut)
)

stations_clust <- zoop_sf %>%
  left_join(stations_ice, by = "amlr.station") %>%
  mutate(pred_clust = station_to_cluster(amlr.station, sightings_cut)) %>%
  semi_join(predators_clust, by = "amlr.station")

# Save derived objects
save_obj(sightings_mtx, "sightings_mtx")
save_obj(sightings_dist, "sightings_dist")
save_obj(stations_clust, "stations_clust")
save_obj(sightings_cut, "sightings_cut")
