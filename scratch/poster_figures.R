library(eks)
library(grid)
library(sf)
library(tidyverse)
tar_load_everything()
source("R/sp.R")


# Community distribution --------------------------------------------------

map_lim <- st_bbox(stations_clust) %>%
  project_bbox() %>%
  expand_bbox(factor = 1.2)
cluster_kde <- stations_clust %>%
  group_by(pred_clust) %>%
  group_map(\(rows, keys) st_kde(rows)$sf %>%
              filter(contlabel %in% c(50, 95)) %>%
              mutate(pred_clust = keys$pred_clust)) %>%
  reduce(sf:::rbind.sf)

p <- ant_basemap(map_lim) +
  # Predator clusters
  ggnewscale::new_scale_fill() +
  geom_sf(aes(color = pred_clust,
              linetype = contlabel),
          cluster_kde,
          fill = NA,
          linewidth = 1) +
  facet_grid(cols = vars(pred_clust)) +
  scale_x_continuous(breaks = c(-60, -55)) +
  scale_y_continuous(breaks = c(-63, -62, -61, -60)) +
  scale_color_brewer(palette = "Dark2") +
  scale_linetype_manual(values = c(2, 1), guide = "none") +
  coord_ant(map_lim) +
  theme(legend.position = "none",
        strip.text = element_text(size = 14, face = "bold"))

# Generate the ggplot2 plot grob
g <- grid.force(ggplotGrob(p))
# Get the names and paths of grobs
grob_ls <- grid.ls(g, print = FALSE)
grob_names <- grob_ls$name
grob_paths <- grob_ls$gPath

strip_names <- guide_parent_names <- str_subset(
  grob_names[str_detect(grob_paths, "strip.text.*titleGrob")],
  "GRID.text"
)
guide_names <- str_subset(
  grob_names[str_detect(grob_paths, "guides.*label.*GRID")],
  "GRID.text"
)

txt_colors <- RColorBrewer::brewer.pal(3, "Dark2")
for (i in 1:3) {
  g <- editGrob(grob = g, gPath = strip_names[i], gp = gpar(col = txt_colors[i]))
}
# Draw the edited plot
grid.newpage()
grid.draw(g)

# 14.31x4.5
scale_factor <- 0.60
ggsave("figs/poster/community_distributions.jpg",
       g,
       width = 14.31 * scale_factor,
       height = 4.5 * scale_factor,
       units = "in",
       dpi = 600)


# ice coverage by cluster -------------------------------------------------

median_coverge <- stations_clust %>%
  drop_na(ice_coverage) %>%
  group_by(pred_clust) %>%
  summarize(median_ice = median(ice_coverage / 10, na.rm = TRUE))

p <- ggplot(stations_clust,
       aes(x = ice_coverage / 10, fill = after_stat(x))) +
  geom_histogram(bins = 20, color = "grey30") +
  geom_vline(aes(xintercept = median_ice),
             median_coverge,
             color = "red", linetype = "dashed") +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_distiller(palette = "Blues", guide = NULL) +
  facet_grid(rows = vars(pred_clust)) +
  labs(x = "Ice coverage",
       y = "# sites") +
  theme_minimal(base_size = 14) +
  theme(strip.text = element_text(face = "bold"))

# Generate the ggplot2 plot grob
g <- grid.force(ggplotGrob(p))
# Get the names and paths of grobs
grob_ls <- grid.ls(g, print = FALSE)
grob_names <- grob_ls$name
grob_paths <- grob_ls$gPath

strip_names <- guide_parent_names <- str_subset(
  grob_names[str_detect(grob_paths, "strip.text.*titleGrob")],
  "GRID.text"
)

txt_colors <- RColorBrewer::brewer.pal(3, "Dark2")
for (i in 1:3) {
  g <- editGrob(grob = g, gPath = strip_names[i], gp = gpar(col = txt_colors[i]))
}

# Draw the edited plot
grid.newpage()
grid.draw(g)

# 14.31x8.8
scale_factor <- 0.40
ggsave("figs/poster/ice_coverage.jpg",
       g,
       width = 14.31 * scale_factor,
       height = 8.8 * scale_factor,
       units = "in",
       dpi = 600)

