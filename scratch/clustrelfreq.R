library(sf)
library(tidyverse)

tar_load(c("predators_stations", "sightings_clust", "zoop"))
clust <- cutree(sightings_clust, 3)
station_ice <- predators_stations %>%
  distinct(amlr.station, ice_type, ice_coverage) %>%
  as_tibble()
pred_prey <- tibble(
  amlr.station = names(clust),
  pred_clust = factor(clust,
                      labels = c("Open", "Marginal", "Pagophilic"))
) %>%
  left_join(select(zoop,
                   amlr.station,
                   Year,
                   zoop_clust = `Winter Cluster factor`),
            by = "amlr.station") %>%
  left_join(station_ice, by = "amlr.station")

pred_prey %>%
  mutate(zoop_clust2 = substr(zoop_clust, 1, 1)) %>%
  crosstable(c(zoop_clust2, ice_coverage),
             by = pred_clust,
             total = "both") %>%
  as_flextable()

