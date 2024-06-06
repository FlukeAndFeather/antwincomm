#' Create a figure with climatological distribution of six most abundant
#' predators
#'
#' @return ggplot object
#' @export
make_fig_top6map <- function(topsix_clim_class) {
  ant_sf <- read_obj("ant_sf")

  # Map bounding box, projected
  map_bbox <- terra::ext(-63, -54, -64, -59) %>%
    terra::project(from = "+proj=longlat +datum=WGS84",
                   to = as.character(ant_proj())[[1]])

  # Map sublabels
  sublabels <- tibble(lyr = names(topsix_clim_class),
                      x = -62.25, y = -59.25) %>%
    arrange(lyr) %>%
    mutate(labels = LETTERS[1:6]) %>%
    sf:: st_as_sf(crs = "EPSG:4326", coords = c("x", "y"))

  ant_basemap() +
    tidyterra::geom_spatraster(data = topsix_clim_class) +
    geom_sf(data = ant_sf) +
    geom_sf_text(aes(label = labels), sublabels, fontface = "bold") +
    scale_fill_brewer(palette = "RdPu",
                      na.value = NA,
                      direction = 1,
                      na.translate = FALSE) +
    scale_x_continuous(breaks = seq(-64, -54, by = 4)) +
    scale_y_continuous(breaks = seq(-64, -59, by = 2)) +
    labs(fill = expression("Ind km"^-1)) +
    facet_wrap(~lyr) +
    coord_sf(xlim = map_bbox[1:2],
             ylim = map_bbox[3:4],
             crs = ant_proj()) +
    theme(legend.position = "top",
          panel.background = element_rect(color = "grey20"),
          strip.text = element_blank())
}

#' Create a figure with distribution of sea ice by predator cluster
#'
#' @return ggplot object
#' @export
make_fig_seaiceclust <- function(stations_clust) {
  median_coverge <- stations_clust %>%
    drop_na(ice_coverage) %>%
    group_by(pred_clust) %>%
    summarize(median_ice = median(ice_coverage / 10, na.rm = TRUE))

  sublabels <- tibble(
    pred_clust = factor(levels(stations_clust$pred_clust)),
    labels = LETTERS[1:3],
    x = 1, y = 15
  )

  ggplot(stations_clust,
              aes(x = ice_coverage / 10, fill = after_stat(x))) +
    geom_histogram(bins = 20, color = "grey30") +
    geom_vline(aes(xintercept = median_ice),
               median_coverge,
               color = "red", linetype = "dashed") +
    geom_text(aes(x = x, y = y, label = labels), sublabels,
              hjust = 1, vjust = 1,
              fontface = "bold") +
    scale_x_continuous(labels = scales::percent) +
    scale_fill_distiller(palette = "Blues", guide = NULL) +
    facet_grid(rows = vars(pred_clust)) +
    labs(x = "Ice coverage",
         y = "# sites") +
    theme_classic() +
    theme(strip.text = element_blank())
}
