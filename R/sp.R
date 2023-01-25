# Use WGS 84 / South Georgia Lambert for mapping
sg_proj <- function() {
  sf::st_crs("EPSG:3762")
}

sg_lims <- function() {
  c(xmin = -67, xmax = -50, ymin = -68, ymax = -55)
}

download_ne <- function(ne_dir) {
  rnaturalearth::ne_download(scale = "large",
                             type = "land",
                             category = "physical",
                             destdir = ne_dir,
                             returnclass = "sf")
}

create_sg_sf <- function(ne_dir, limits = sg_lims()) {
  land <- rnaturalearth::ne_load(scale = "large",
                                 type = "land",
                                 category = "physical",
                                 destdir = ne_dir,
                                 returnclass = "sf")
  sg_sf <- sf::st_crop(land, sf::st_bbox(limits))
}

sg_raster_template <- function(limits = sg_lims(), res = 5e4) {
  lims_prj <- sf::st_bbox(limits, crs = "EPSG:4326") %>%
    sf::st_as_sfc() %>%
    sf::st_transform(sg_proj()) %>%
    sf::st_bbox()
  raster::raster(xmn = lims_prj["xmin"],
                 xmx = lims_prj["xmax"],
                 ymn = lims_prj["ymin"],
                 ymx = lims_prj["ymax"],
                 crs = sg_proj()[1]$input,
                 resolution = res)
}

rasterize_counts <- function(counts_sf,
                             limits = sg_lims(),
                             res = 5e4,
                             layer_name = "counts") {
  raster::rasterize(
    sf::as_Spatial(sf::st_transform(counts_sf, sg_proj())),
    sg_raster_template(limits, res),
    field = "count",
    fun = "sum",
    na.rm = TRUE
  ) %>%
    stars::st_as_stars() %>%
    setNames(layer_name)
}

