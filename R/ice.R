aggregate_ice <- function(predators, ice_cat, stations) {
  ice_mode <- function(ice_cats) {
    names(which.max(table(foo$ice_cat)))
  }
  predators %>%
    mutate(year = lubridate::year(lubridate::mdy_hm(UTC_start))) %>%
    group_by(amlr_station, ice_type_desc)
  ice %>%
    filter(ice_type_desc != "NULL") %>%
    latlon_to_sf(coords = c("lon_mean", "lat_mean")) %>%
    left_join(ice_cat, by = "ice_type_desc") %>%
    mutate(ice_cat = factor(ice_cat, levels = c("OPEN",
                                                "THIN",
                                                "FY",
                                                "MY"))) %>%
    assign_sightings(stations, max_dist_km = 15) %>%
    group_by(amlr.station) %>%
    summarize(ice_cat = ice_mode(ice_cat))
}
