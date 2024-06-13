#' Make a table summarizing survey effort and ice conditions
#'
#' @param stations_ice Station-level ice data
#' @param station_effort Station-level effort data
#' @param predators_stations Station-level predator data
#'
#' @return knitr::knitr_kable
#' @export
make_tbl_effortice <- function(stations_ice, station_effort, predators_stations) {
  # Effort
  effort_by_year <- station_effort %>%
    as_tibble() %>%
    semi_join(predators_stations, by = "amlr.station") %>%
    group_by(Year = year) %>%
    summarize(
      `Stations sampled` = n(),
      `Effort per station (km)` = sprintf("%0.1f \U00B1 %0.1f",
                                          mean(survey_km),
                                          sd(survey_km))
    )
  effort_total <- station_effort %>%
    as_tibble() %>%
    semi_join(predators_stations, by = "amlr.station") %>%
    summarize(
      `Stations sampled` = n(),
      `Effort per station (km)` = sprintf("%0.1f \U00B1 %0.1f",
                                          mean(survey_km),
                                          sd(survey_km))
    ) %>%
    mutate(Year = "Total")
  effort <- rbind(effort_by_year, effort_total)

  # Ice
  se <- function(p, n) sqrt(p * (1 - p) / n)
  ice_lbl <- c(OPEN = "Open water",
               THIN = "Thin ice",
               FY = "First-year ice",
               MY = "Multi-year ice")
  ice_by_year <- stations_ice %>%
    semi_join(predators_stations, by = "amlr.station") %>%
    mutate(ice_coverage = ice_coverage / 10,
           Year = as.numeric(substr(amlr.station, 5, 8)),
           ice_type = ice_lbl[ice_type]) %>%
    group_by(Year, ice_type) %>%
    summarize(ice = sprintf("%d (%0.1f%% \U00B1 %0.1f%%)",
                            n(),
                            mean(ice_coverage) * 100,
                            se(mean(ice_coverage), n())),
              .groups = "drop") %>%
    pivot_wider(names_from = ice_type, values_from = ice, values_fill = "0")
  ice_total <- stations_ice %>%
    semi_join(predators_stations, by = "amlr.station") %>%
    mutate(ice_coverage = ice_coverage / 10,
           ice_type = ice_lbl[ice_type]) %>%
    group_by(ice_type) %>%
    summarize(ice = sprintf("%d (%0.1f%% \U00B1 %0.1f%%)",
                            n(),
                            mean(ice_coverage) * 100,
                            se(mean(ice_coverage), n()))) %>%
    pivot_wider(names_from = ice_type, values_from = ice, values_fill = "0") %>%
    mutate(Year = "Total")
  ice <- rbind(ice_by_year, ice_total)

  # Together
  cbind(effort, ice) %>%
    knitr::kable()
}

make_tbl_densfreq <- function(predators_clust, stations_clust, station_effort) {
  # Density/frequency by cluster
  # Effort
  effort_by_clust <- as_tibble(stations_clust) %>%
    left_join(station_effort, by = "amlr.station") %>%
    select(amlr.station, pred_clust, survey_km) %>%
    group_by(pred_clust) %>%
    summarize(survey_km = sum(survey_km),
              n_station = n())
  # Species
  species_by_clust <- predators_clust %>%
    group_by(Species = species, pred_clust) %>%
    summarize(count = sum(count),
              n_present = n(),
              .groups = "drop")
  # Combined
  fmt_densfreq <- function(d, f) {
    case_when(
      f == 0 ~ "0.000 (0.0%)",
      f > 0 & d < 0.001 ~ sprintf("<0.001 (%0.1f%%)", f * 100),
      TRUE ~ sprintf("%0.3f (%0.1f%%)", d, f * 100)
    )
  }
  densfreq_by_clust <- species_by_clust %>%
    complete(Species, pred_clust, fill = list(count = 0,
                                              survey_km = 1,
                                              n_present = 0)) %>%
    left_join(effort_by_clust, by = "pred_clust") %>%
    mutate(density = count / survey_km,
           freq = n_present / n_station,
           pred = fmt_densfreq(density, freq)) %>%
    select(Species, pred_clust, pred) %>%
    pivot_wider(names_from = pred_clust,
                values_from = pred)

  # Density/frequency overall
  # Effort
  effort_total <- summarize(effort_by_clust,
                            survey_km = sum(survey_km),
                            n_station = sum(n_station))
  # Species
  species_total <- species_by_clust %>%
    group_by(Species) %>%
    summarize(count = sum(count),
              n_present = sum(n_present))
  # Combined
  densfreq_total <- species_total %>%
    cbind(effort_total) %>%
    mutate(density = count / survey_km,
           freq = n_present / n_station,
           `All clusters` = fmt_densfreq(density, freq)) %>%
    arrange(desc(density)) %>%
    select(Species, `All clusters`)

  # By cluster and overall together
  densfreq_total %>%
    left_join(densfreq_by_clust, by = "Species") %>%
    relocate(`All clusters`, .after = last_col()) %>%
    mutate(Species = code_to_common(Species)) %>%
    knitr::kable()
}
