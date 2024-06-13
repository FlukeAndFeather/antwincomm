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
