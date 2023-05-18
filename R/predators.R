aggregate_predators <- function(predators) {
  predators %>%
    filter(species != "NULL") %>%
    mutate(year = lubridate::year(lubridate::mdy_hm(UTC_start))) %>%
    group_by(year, interval, species, lon_mean, lat_mean, nmi) %>%
    summarize(count = sum(count_species), .groups = "drop") %>%
    mutate(count_norm = count / nmi)
}
