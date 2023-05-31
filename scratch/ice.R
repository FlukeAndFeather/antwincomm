library(tidyverse)
library(readxl)
library(lubridate)
library(sf)
source("R/sp.R")
source("R/ice.R")

clean_ice <- function(ice_types) {
  str_extract(ice_types, "[^/]+") %>%
    factor(levels = c("OPEN",
                      "SLUSH",
                      "BRASH",
                      "THIN",
                      "FY",
                      "MY",
                      "NULL")) %>%
    fct_collapse(THIN = "SLUSH",
                 THIN = "BRASH")
}
ice_raw <- read_excel("data/ALl_raw_bird_pred_obs_12_16_winter.xlsx") %>%
  transmute(year_ice = Year,
            lat = Latitude,
            lon = Longitude,
            ice_code = as.numeric(IceCode),
            ice_type = clean_ice(IceType),
            date_ice = as.Date(paste(Year, Month, Day, sep = "-"))) %>%
  filter(ice_type != "NULL") %>%
  latlon_to_sf(coords = c("lon", "lat"))

tar_load("zoop")
stations <- zoop %>%
  transmute(amlr_station = amlr.station,
            year_station = Year,
            lat = dec.latitude,
            lon = dec.longitude,
            date_station = as.Date(date.tow)) %>%
  latlon_to_sf(coords = c("lon", "lat"))

ice_stations <- ice_raw %>%
  group_by(year_ice) %>%
  group_modify(function(ice_by_year, ice_keys) {
    stations_by_year <- filter(stations, year_station == ice_keys$year_ice)
    nearest_stations_idx <- st_nearest_feature(ice_by_year, stations_by_year)
    nearest_stations <- stations_by_year[nearest_stations_idx, ]
    station_distance <- st_distance(ice_by_year,
                                    nearest_stations,
                                    by_element = TRUE)
    station_lag <- ice_by_year$date_ice - nearest_stations$date_station
    stations_no_sf <- select(as_tibble(nearest_stations), -geometry)
    ice_by_year %>%
      cbind(stations_no_sf) %>%
      mutate(station_distance = station_distance,
             station_lag = station_lag)
  }) %>%
  ungroup() %>%
  filter(station_distance <= units::as_units(15, "km"),
         abs(station_lag) <= as.difftime(1, units = "days")) %>%
  st_as_sf(sf_column_name = "geometry")

ice_stations %>%
  mutate(amlr_station = fct_infreq(amlr_station)) %>%
  ggplot(aes(amlr_station, fill = ice_type)) +
  geom_bar()

ice_aggregate <- ice_stations %>%
  group_by(amlr_station, year = year_ice) %>%
  summarize(modal_ice_type = mfv(ice_type),
            n_obs = n(),
            n_modal = sum(ice_type == modal_ice_type),
            frac_modal = n_modal / n_obs,
            mean_coverage = mean(ice_code, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(modal_ice_type = factor(modal_ice_type, levels = c("OPEN",
                                                            "THIN",
                                                            "FY",
                                                            "MY")))

ice_aggregate %>%
  count(n_obs, n_modal) %>%
  mutate(n = factor(n)) %>%
  ggplot(aes(n_obs, n_modal, color = n)) +
  geom_abline(slope = c(1, 0.5), intercept = 0,
              color = "purple", linetype = "dotted") +
  geom_point(alpha = 0.75) +
  annotate("text",
           x = 750,
           y = 750,
           label = "100%",
           color = "purple4",
           hjust = 1.5) +
  annotate("text",
           x = 750,
           y = 750 / 2,
           label = "50%",
           color = "purple4",
           hjust = -1) +
  labs(x = "Total observations",
       y = "Modal observations",
       color = "Stations") +
  scale_color_manual(values = c("grey20", "#fc8d59", "#b30000")) +
  theme_classic() +
  theme(legend.justification = c(0, 1),
        legend.position = c(0.05, 0.95))

ant_basemap() +
  geom_sf(aes(color = modal_ice_type), ice_aggregate) +
  scale_color_brewer("Ice category", palette = "YlGnBu") +
  facet_wrap(~year) +
  theme(legend.position = "left")
