# Most frequent value i.e., mode
# Note: returns a character
mfv <- function(x) {
  x_tbl <- table(x)
  names(x_tbl)[which.max(x_tbl)]
}

# Adds three columns to stations data frame:
# ice_cat [fct] OPEN, THIN, FY, MY
# ice_intervals [int] count of intervals with ice obs associated with station
# ice_cat_freq [dbl] fraction of intervals assoc w station with modal ice cat
aggregate_ice <- function(stations, underway, ice_cat) {
  # Aggregates underway ice observations by nearest AMLR station
  # Uses mode as aggregation function i.e. mfv()
  underway_ice <- as_tibble(stations) %>%
    left_join(transmute(underway,
                        year = as.numeric(substr(cruise, 5, 8)),
                        interval,
                        ice_type_desc),
              by = c("year", "interval"),
              multiple = "all") %>%
    filter(ice_type_desc != "NULL") %>%
    left_join(ice_cat, by = "ice_type_desc") %>%
    # Ice observations were unique to intervals, but intervals are
    # repeated if there were multiple predator sightings
    group_by(amlr.station,
             interval) %>%
    summarize(interval_ice_cat = first(ice_cat),
              .groups = "drop_last") %>%
    summarize(ice_cat = mfv(interval_ice_cat),
              ice_intervals = n(),
              ice_cat_freq = sum(ice_cat == interval_ice_cat) / n()) %>%
    mutate(ice_cat = factor(ice_cat,
                            levels = c("OPEN", "THIN", "FY", "MY")))

  stations %>%
    left_join(underway_ice, by = "amlr.station")
}
