#' Calculate NMDS stress across numbers of axes
#'
#' @param dist distance matrix
#' @param k number of axes (vectorized, e.g., 1:6 is ok)
#'
#' @return tibble with k and stress
#' @export
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

#' Create shepard diagram data
#'
#' @param nmds nmds object (see vegan::metaMDS)
#'
#' @return tibble with diss, dist, and monotonically increasing nonparametric
#'   fit
#' @export
shepard <- function(nmds) {
  nmds_isoreg <- isoreg(nmds$diss, nmds$dist)
  tibble(
    diss = nmds$diss,
    dist = nmds$dist,
    mono = nmds_isoreg$yf
  )
}

#' Convert NMDS object to a data frame, including cluster and environmental data
#'
#' @param nmds NMDS object
#' @param clust Cluster data
#' @param env Environmental data
#'
#' @return NMDS object as data frame
#' @export
nmds_to_df <- function(nmds, clust, env) {
  nmds$points %>%
    as_tibble(rownames = "amlr.station") %>%
    rename_with(\(axis) paste0("N", axis), starts_with("MDS")) %>%
    mutate(pred_clust = station_to_cluster(amlr.station, clust)) %>%
    left_join(env, by = "amlr.station")
}

#' Combine ice and station environmental data
#'
#' Filters to stations used in this analysis
#'
#' @param stations Zooplankton station data
#' @param ice Ice data
#' @param predators Predator data
#'
#' @return Environmental data for NMDS loadings
#' @export
env_data <- function(stations, ice, predators) {
  stations <- as_tibble(stations)
  left_join(
    select(stations, amlr.station, zuml_m, avg.temp, avg.salinity,
           Integ.chla.100m, Integ.phae.100m, time.of.day, Year,
           zoop_clust = `Winter Cluster factor`),
    select(ice, amlr.station, ice_type, ice_coverage),
    by = "amlr.station"
  ) %>%
    semi_join(predators, by = "amlr.station") %>%
    mutate(Year = factor(Year))
}
