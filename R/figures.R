#' Create a figure with climatological distribution of six most abundant
#' predators
#'
#' As a side effect, saves figure to EPS in figures/
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
