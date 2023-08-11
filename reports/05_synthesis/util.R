library(ggiraph)
library(ggtree)
library(shiny)
library(sf)
library(targets)
library(tidyterra)
library(tidyverse)

tar_load_everything(store = here::here("_targets"))
source(here::here("R", "sp.R"))
seaice_contours <- terra::unwrap(seaice_contours)

# Create the community map
make_community_map <- function() {
  map_lim <- st_bbox(stations_clust) %>%
    project_bbox() %>%
    expand_bbox(factor = 1.2)

  p <- ant_basemap(map_lim) +
    geom_sf_interactive(aes(color = pred_clust, data_id = amlr.station),
            stations_clust,
            size = 2,
            shape = 15) +
    geom_sf(data = seaice_contours, color = "skyblue") +
    facet_wrap(~ Year) +
    scale_x_continuous(breaks = c(-60, -55)) +
    scale_y_continuous(breaks = c(-63, -62, -61, -60)) +
    scale_color_brewer(palette = "Dark2") +
    scale_fill_gradient(lim = c(0, 1), na.value = "transparent") +
    coord_ant(map_lim) +
    theme(legend.position = "bottom",
          legend.title = element_blank())

  girafe(ggobj = p) %>%
    girafe_options(opts_selection(type = "single"))
}

# Create the environment histograms
make_env_hist <- function(var, highlighted = NULL) {
  dat <- nmds_env
  dat[["var"]] <- dat[[var]]
  dat <- dat[!is.na(dat[["var"]]), ]

  highlighted_val <- if(!is.null(highlighted)) {
    val <- dat[["var"]][dat$amlr.station == highlighted]
    geom_vline(xintercept = val, color = "red")
  }

  xlbl <- c(
    avg.salinity = "Salinity (PSU)",
    ice_coverage = "Ice coverage (%)",
    Integ.chla.100m = "Chl a (mg m^-2)"
  )[var]
  ggplot(dat, aes(var)) +
    geom_histogram(bins = 30) +
    highlighted_val +
    labs(x = xlbl) +
    theme_classic()
}

# Create cluster dendrogram
make_cluster_dendro <- function(highlighted = NULL) {
  g <- split(names(sightings_cut), sightings_cut)
  p <- ggtree(sightings_clust, hang = -1)
  pred_clust_mrca <- sapply(g, function(n) MRCA(p, n))

  station_highlight <- if (!is.null(highlighted)) {
    station_y <- p$data$y[p$data$isTip & p$data$label == highlighted]
    geom_point(x = 0, y = station_y, color = "red", shape = 17)
  }

  p %>%
    groupClade(pred_clust_mrca, group_name = "Predator cluster") +
    aes(color = `Predator cluster`) +
    station_highlight +
    layout_dendrogram() +
    theme_dendrogram()
}

# Create station table
make_station_table <- function(highlighted = NULL) {
  if (is.null(highlighted)) {
    return(NULL)
  }
  nmds_env %>%
    filter(amlr.station == highlighted) %>%
    left_join(select(zoop, amlr.station, date.tow),
              by = "amlr.station") %>%
    left_join(select(as_tibble(station_effort), amlr.station, survey_nmi),
              by = "amlr.station") %>%
    select(amlr.station, Year, survey_nmi, avg.salinity,
           Integ.chla.100m, ice_coverage, zoop_clust, avg.temp, Integ.phae.100m,
           TOD_2levels_civil, ice_type)
}

# Create sightings table
make_sightings_table <- function(highlighted = NULL) {
  if (is.null(highlighted)) {
    return(NULL)
  }
  predators_clust %>%
    filter(amlr.station == highlighted) %>%
    select(amlr.station,  species, count, count_nmi)
}
