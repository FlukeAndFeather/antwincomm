# Calculate NMDS stress across numbers of axes
stress <- function(dist, k) {
  tibble(
    k = k,
    stress = map_dbl(k, \(k) {
      vegan::metaMDS(
        dist,
        k = k,
        trymax = 100,
        trace = FALSE
      )$stress
    })
  )
}

# Create shepard diagram data
shepard <- function(nmds) {
  nmds_isoreg <- isoreg(nmds$diss, nmds$dist)
  tibble(
    diss = nmds$diss,
    dist = nmds$dist,
    mono = nmds_isoreg$yf
  )
}

# Convert NMDS object to a data frame, including cluster and environmental data
nmds_to_df <- function(nmds, clust, env) {
  nmds$points %>%
    as_tibble(rownames = "amlr.station") %>%
    rename_with(\(axis) paste0("N", axis), starts_with("MDS")) %>%
    mutate(pred_clust = station_to_cluster(amlr.station, clust)) %>%
    left_join(env, by = "amlr.station")
}

# Combine ice and station environmental data, filtering down to stations used
# in this analysis
env_data <- function(stations, ice, predators) {
  left_join(
    select(stations, amlr.station, zuml_m, avg.temp, avg.salinity,
           Integ.chla.100m, Integ.phae.100m, TOD_2levels_civil, Year,
           zoop_clust = `Winter Cluster factor`),
    select(ice, amlr.station, ice_type, ice_coverage),
    by = "amlr.station"
  ) %>%
    semi_join(predators, by = "amlr.station") %>%
    mutate(Year = factor(Year))
}
