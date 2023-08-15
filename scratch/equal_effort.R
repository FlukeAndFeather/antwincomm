library(lubridate)
library(sf)
library(tidyverse)
source("R/sp.R")
tar_load_everything()

# Distance between stations
inter_station_dist <- zoop_sf %>%
  group_by(Year) %>%
  group_modify(~ mutate(.,
                        dist_near_km = apply(st_distance(.x),
                                             1,
                                             \(x) min(x[x > 0])) / 1000)) %>%
  ungroup()

station_radius_km <- 15
dist_quant <- ecdf(inter_station_dist$dist_near_km)(station_radius_km * 1.5)
ggplot(inter_station_dist, aes(dist_near_km)) +
  stat_ecdf() +
  geom_segment(x = station_radius_km * 1.5, xend = station_radius_km * 1.5, y = 0, yend = dist_quant,
               color = "firebrick") +
  geom_segment(x = 0, xend = station_radius_km * 1.5, y = dist_quant, yend = dist_quant,
               color = "firebrick") +
  annotate("text",
           x = 0,
           y = 0.2,
           label = str_glue("{round(dist_quant * 100, 1)}% of stations within\n{station_radius_km * 1.5} km of nearest neighbor"),
           hjust = 0) +
  theme_classic()

# Exclude stations <30 km from nearest neighbor
# TODO: If 2 stations <30km apart, keep 1
keep <- inter_station_dist$amlr.station[inter_station_dist$dist_near_km > station_radius_km * 1.5]
stations <- filter(zoop_sf, amlr.station %in% keep)

# 15km buffers around station
station_buffers <- stations %>%
  mutate(tow.utc = as.POSIXct(date.tow) +
           (start.time.UTC - as.POSIXct("1899-12-31", tz = "UTC"))) %>%
  latlon_to_sf(coords = c("dec.longitude", "dec.latitude")) %>%
  st_transform(ant_proj()) %>%
  st_buffer(15e3)

# Track w/in 24 hrs
track_time_buffer <- map(station_buffers$tow.utc, \(utc) {
  time_h <- 24
  predators %>%
    mutate(UTC_start = mdy_hm(UTC_start, tz = "UTC")) %>%
    filter(between(UTC_start,
                   utc - time_h * 3600,
                   utc + time_h * 3600)) %>%
    distinct(lon_mean, lat_mean) %>%
    as.matrix() %>%
    st_linestring()
}) %>%
  { tibble(geometry = .) } %>%
  st_as_sf(crs = "EPSG:4326") %>%
  st_transform(ant_proj())

# Pairwise intersection between station 15 km buffer and track 24 hour buffer
local_track <- map(seq(nrow(station_buffers)), \(i) {
  st_intersection(slice(station_buffers, i),
                  slice(track_time_buffer, i))
}) %>%
  list_rbind() %>%
  st_as_sf(crs = ant_proj()) %>%
  mutate(effort_km = as.numeric(st_length(.)) / 1000)

# Distribution of survey effort
effort_lim <- c(29, 34)
effort_quant <- ecdf(local_track$effort_km)(effort_lim)
coverage <- diff(effort_quant)
ecdf_text <- str_glue("{round(coverage * 100, 1)}% of stations had between {effort_lim[1]} and\n{effort_lim[2]} km of survey effort within 15 km and\n24 hours")
ggplot(local_track, aes(effort_km)) +
  stat_ecdf() +
  geom_vline(xintercept = effort_lim, color = "firebrick") +
  geom_hline(yintercept = ecdf(local_track$effort_km)(effort_lim),
             color = "cornflowerblue") +
  annotate("text", x = 40, y = 0.5, label = ecdf_text, hjust = 0) +
  theme_classic()

# Retain only stations with 29-34 km of effort
stations <- stations %>%
  left_join(select(as_tibble(local_track), amlr.station, effort_km),
            by = "amlr.station") %>%
  filter(between(effort_km, 29, 34))

for (yr in unique(stations$Year)) {
  filename <- file.path("figs",
                        "effort_maps",
                        paste0(yr, "_effort.pdf"))
  st <- filter(stations, Year == yr)
  tr <- semi_join(local_track, as_tibble(st), by = "amlr.station")
  bf <- semi_join(station_buffers, as_tibble(st), by = "amlr.station")

  p <- ant_basemap() +
    geom_sf(data = tr) +
    geom_sf(data = st, color = "firebrick") +
    geom_sf(data = bf, color = "cornflowerblue", fill = NA) +
    labs(title = st$amlr.station) +
    coord_ant(map_lim = expand_bbox(st_bbox(bf), 1.2))

  ggsave(filename, p)
}
