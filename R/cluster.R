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
# formulas in Dufrene and Legendre (1997) and implemented in the R package
# labdsv

# Convert sightings data frame to matrix
sightings_to_matrix <- function(sightings) {
  sightings_wide <- sightings %>%
    as_tibble() %>%
    mutate(count_nmi_log = log1p(count_nmi)) %>%
    pivot_wider(id_cols = amlr.station,
                names_from = species,
                values_from = count_nmi_log,
                values_fill = 0)
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
  factor(clust_int, levels = 1:3, labels = c("Pack ice", "Marginal ice", "Open water"))
}

# Determine if a species is rare (<5% of stations)
is_rare <- function(species, station) {
  n_station <- n_distinct(station)
  species_rarity <- tapply(station, species, length) / n_station < 0.05
  species_rarity[species]
}
