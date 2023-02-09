# Use WGS 84 / Antarctic Polar Stereographic for mapping, recentered on ant_lims()
ant_proj <- function() {
  lat_ts <- (ant_lims()["ymin"] + ant_lims()["ymax"]) / 2
  lon_0 <- (ant_lims()["xmin"] + ant_lims()["xmax"]) / 2
  glue::glue(
    "+proj=stere +lat_0=-90 +lat_ts={lat_ts} +lon_0={lon_0} +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs"
  ) %>%
  sf::st_crs()
}

ant_lims <- function() {
  c(xmin = -67, xmax = -50, ymin = -68, ymax = -55)
}

download_ne <- function(ne_dir) {
  rnaturalearth::ne_download(scale = "large",
                             type = "land",
                             category = "physical",
                             destdir = ne_dir,
                             returnclass = "sf")
}

create_ant_sf <- function(ne_dir, limits = ant_lims()) {
  land <- rnaturalearth::ne_load(scale = "large",
                                 type = "land",
                                 category = "physical",
                                 destdir = ne_dir,
                                 returnclass = "sf")
  sf::st_crop(land, sf::st_bbox(limits))
}

ant_raster_template <- function(limits = ant_lims(), res = 5e4) {
  lims_prj <- sf::st_bbox(limits, crs = "EPSG:4326") %>%
    project_bbox()
  raster::raster(xmn = lims_prj["xmin"],
                 xmx = lims_prj["xmax"],
                 ymn = lims_prj["ymin"],
                 ymx = lims_prj["ymax"],
                 crs = ant_proj()[1]$input,
                 resolution = res)
}

rasterize_counts <- function(counts_sf,
                             limits = ant_lims(),
                             res = 5e4,
                             layer_name = "counts") {
  raster::rasterize(
    sf::as_Spatial(sf::st_transform(counts_sf, ant_proj())),
    ant_raster_template(limits, res),
    field = "count",
    fun = "sum",
    na.rm = TRUE
  ) %>%
    stars::st_as_stars() %>%
    setNames(layer_name)
}

latlon_to_sf <- function(df, coords = c("x", "y")) {
  sf::st_as_sf(df,
               coords = coords,
               crs = sp::CRS("+proj=longlat +datum=WGS84"))
}

ant_basemap <- function(map_lim = NULL) {
  if (is.null(map_lim)) {
    map_lim <- sf::st_bbox(ant_lims(), crs = "EPSG:4326") %>%
      project_bbox()
  }

  ggplot() +
    geom_sf(data = tar_read("ant_sf", store = here::here("_targets"))) +
    coord_sf(xlim = map_lim[c("xmin", "xmax")],
             ylim = map_lim[c("ymin", "ymax")],
             expand = FALSE,
             crs = ant_proj()) +
    theme_minimal() +
    theme(axis.title = element_blank())
}

project_bbox <- function(x) {
  x %>%
    sf::st_as_sfc(crs = "EPSG:4326") %>%
    sf::st_transform(ant_proj()) %>%
    sf::st_bbox()
}

expand_bbox <- function(x, factor) {
  result <- x
  xrng <- x["xmax"] - x["xmin"]
  x0 <- x["xmin"] + xrng / 2
  yrng <- x["ymax"] - x["ymin"]
  y0 <- x["ymin"] + yrng / 2
  result["xmin"] <- x0 - xrng * factor / 2
  result["xmax"] <- x0 + xrng * factor / 2
  result["ymin"] <- y0 - yrng * factor / 2
  result["ymax"] <- y0 + yrng * factor / 2
  result
}
