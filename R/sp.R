# Use WGS 84 / South Georgia Lambert for mapping
sg_proj <- function(){
  sf::st_crs("EPSG:3762")
}

download_ne <- function(ne_dir) {
  rnaturalearth::ne_download(scale = "large",
                             type = "land",
                             category = "physical",
                             destdir = ne_dir,
                             returnclass = "sf")
}

create_sg_sf <- function(
    ne_dir,
    limits = c(xmin = -67, xmax = -50, ymin = -68, ymax = -55)
) {
  land <- rnaturalearth::ne_load(scale = "large",
                                 type = "land",
                                 category = "physical",
                                 destdir = ne_dir,
                                 returnclass = "sf")
  sg_sf <- sf::st_crop(land, sf::st_bbox(limits))
}
