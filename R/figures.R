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

#' Create a figure with geographic distribution of predator clusters by year
#'
#' @return ggplot object
#' @export
make_fig_predclustannual <- function(stations_clust, seaice_conc_df) {
  map_lim <- sf::st_bbox(stations_clust) %>%
    project_bbox() %>%
    expand_bbox(factor = 1.2)

  sublabels <- tibble(Year = 2012:2016,
                      x = -63, y = -60,
                      labels = LETTERS[1:5]) %>%
    sf::st_as_sf(crs = "EPSG:4326", coords = c("x", "y"))

  ant_basemap() +
    # Sea ice
    geom_tile(aes(x, y, fill = seaice_conc),
              filter(seaice_conc_df, between(seaice_conc, 0.65, 1)),
              alpha = 0.8) +
    scale_fill_distiller("Ice concentration",
                         lim = c(0.65, 1),
                         labels = scales::percent,
                         palette = "Blues",
                         na.value = "transparent",
                         guide = guide_colorbar(title.position = "top")) +
    # Predator clusters
    ggnewscale::new_scale_fill() +
    geom_sf(aes(fill = pred_clust),
            stations_clust,
            size = 2,
            shape = 22) +
    facet_wrap(~ Year) +
    # Labels
    geom_sf_text(aes(label = labels), sublabels, fontface = "bold", vjust = 0) +
    scale_x_continuous(breaks = c(-60, -55)) +
    scale_y_continuous(breaks = c(-63, -62, -61, -60)) +
    scale_fill_brewer("Predator cluster", palette = "Dark2",
                      guide = guide_legend(override.aes = list(size = 3),
                                           title.position = "top")) +
    coord_ant(map_lim) +
    theme(legend.position = "bottom",
          strip.text = element_blank(),
          panel.background = element_rect())
}

#' Create a figure with geographic distribution of predator clusters across
#' years
#'
#' @return ggplot object
#' @export
make_fig_predclustkde <- function(stations_clust) {
  map_lim <- sf::st_bbox(stations_clust) %>%
    project_bbox() %>%
    expand_bbox(factor = 1.2)

  cluster_kde <- stations_clust %>%
    group_by(pred_clust) %>%
    group_map(\(rows, keys) eks::st_kde(rows)$sf %>%
                filter(contlabel %in% c(50, 95)) %>%
                mutate(pred_clust = keys$pred_clust)) %>%
    reduce(sf:::rbind.sf)

  sublabels <- tibble(pred_clust = factor(levels(cluster_kde$pred_clust)),
                      x = -63, y = -60,
                      labels = LETTERS[1:3]) %>%
    sf::st_as_sf(crs = "EPSG:4326", coords = c("x", "y"))

  ant_basemap() +
    # Predator clusters
    ggnewscale::new_scale_fill() +
    geom_sf(aes(color = pred_clust,
                linetype = contlabel),
            cluster_kde,
            fill = NA,
            linewidth = 1) +
    geom_sf_text(aes(label = labels), sublabels, fontface = "bold", vjust = 0) +
    facet_wrap(~ pred_clust) +
    scale_x_continuous(breaks = c(-60, -55)) +
    scale_y_continuous(breaks = c(-63, -62, -61, -60)) +
    scale_color_brewer("Predator cluster", palette = "Dark2") +
    scale_linetype_manual(values = c(2, 1), guide = "none") +
    coord_ant(map_lim) +
    theme(legend.position = "bottom",
          strip.text = element_blank(),
          panel.background = element_rect())
}

#' Create a figure with NMDS loadings
#' years
#'
#' @return ggplot object
#' @export
make_fig_nmds <- function(nmds_envfit, nmds_df, nmds_sightings, nmds_env) {
  year_centroids <- vegan::scores(nmds_envfit,
                                  display = "factors",
                                  choices = 1:3) %>%
    as_tibble(rownames = "envvar") %>%
    filter(str_starts(envvar, "Year")) %>%
    mutate(across(starts_with("NMDS"),
                  ~ .x * vegan::ordiArrowMul(nmds_envfit)),
           Year = substr(envvar, 5, 8))

  nmds_plot <- function(axis1, axis2) {
    ice_ordisurf <- vegan::ordisurf(
      nmds_sightings ~ nmds_env$ice_coverage,
      choices = c(axis1, axis2),
      plot = FALSE
    )

    x <- paste0("NMDS", axis1)
    y <- paste0("NMDS", axis2)

    ice_df <- expand_grid(
      axis1 = ice_ordisurf$grid$x,
      axis2 = ice_ordisurf$grid$y,
    ) %>%
      cbind(as.numeric(ice_ordisurf$grid$z)) %>%
      set_names(c(x, y, "ice_coverage")) %>%
      mutate(ice_coverage = ice_coverage / 10) %>%
      drop_na(ice_coverage)

    ggplot(nmds_df, aes(.data[[x]], .data[[y]])) +
      stat_contour(aes(z = .data$ice_coverage,
                       color = after_stat(level)),
                   ice_df) +
      scale_color_distiller(
        "Ice coverage",
        palette = "Blues",
        labels = scales::percent,
        limits = c(0.25, 0.75),
        breaks = seq(0.25, 0.75, by = 0.1),
        guide = guide_colorbar(barwidth = unit(2, "in"),
                               direction = "horizontal")
      ) +
      ggnewscale::new_scale_color() +
      geom_point(aes(color = pred_clust), size = 1, alpha = 0.8) +
      geom_point(data = year_centroids,
                 shape = 18,
                 size = 2.5,
                 color = "black",
                 alpha = 0.85) +
      ggrepel::geom_text_repel(aes(label = Year),
                               year_centroids) +
      scale_color_brewer(
        palette = "Dark2",
        guide = guide_legend(override.aes = list(size = 2),
                             direction = "horizontal",
                             order = 1)
      ) +
      labs(color = "Predator cluster") +
      theme_classic(base_size = 10)
  }

  map2(c(1, 1, 2), c(2, 3, 3), nmds_plot) %>%
    reduce(patchwork:::`|.ggplot`) +
    patchwork::plot_annotation(tag_levels = 'A') +
    patchwork::plot_layout(guides = "collect") &
    theme(legend.position = "bottom",
          legend.box = "vertical",
          legend.box.margin = margin(t = -3),
          legend.spacing.y = unit(-5, "pt"),
          plot.tag = element_text(face = "bold"))
}
