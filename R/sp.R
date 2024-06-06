#' Use WGS 84 / Antarctic Polar Stereographic for mapping, recentered on ant_lims()
#'
#' @return CRS object
#' @export
ant_proj <- function() {
  lat_ts <- (ant_lims()["ymin"] + ant_lims()["ymax"]) / 2
  lon_0 <- (ant_lims()["xmin"] + ant_lims()["xmax"]) / 2
  sf::st_crs(str_glue("+proj=stere +lat_0=-90 +lat_ts={lat_ts} +lon_0={lon_0} +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs"))
}

#' Spatial limits of study region
#'
#' @return A named vector with xmin, xmax, ymin, and ymax values
#' @export
ant_lims <- function() {
  c(xmin = -67, xmax = -50, ymin = -68, ymax = -55)
}

#' Create Antarctic Peninsula sf object using Natural Earth data
#'
#' @param ne_dir directory to store Natural Earth data
#' @param limits spatial limits
#'
#' @return Antarctic Peninsula sf object
#' @export
create_ant_sf <- function(ne_dir, limits = ant_lims()) {
  # Download data from Natural Earth, if necessary
  if (!dir.exists(ne_dir)) dir.create(ne_dir, recursive = TRUE)
  if (!file.exists(file.path(ne_dir, "ne_10m_land.shp")))
    rnaturalearth::ne_download(scale = "large",
                               type = "land",
                               category = "physical",
                               destdir = ne_dir,
                               returnclass = "sf")

  # Load and crop data
  land <- rnaturalearth::ne_load(scale = "large",
                                 type = "land",
                                 category = "physical",
                                 destdir = ne_dir,
                                 returnclass = "sf")
  suppressWarnings(sf::st_crop(land, sf::st_bbox(limits)))
}

ant_raster_template <- function(limits = ant_lims(), res = 5e4) {
  lims_prj <- st_bbox(limits, crs = "EPSG:4326") %>%
    project_bbox()
  raster::raster(xmn = lims_prj["xmin"],
                 xmx = lims_prj["xmax"],
                 ymn = lims_prj["ymin"],
                 ymx = lims_prj["ymax"],
                 crs = ant_proj()[1]$input,
                 resolution = res)
}

# By species and year
rasterize_counts <- function(counts_sf,
                             count_col = "count",
                             species_col = "species",
                             year_col = "year",
                             layer_name = count_col,
                             limits = ant_lims(),
                             res = 5e4) {
  stopifnot(inherits(counts_sf, "sf"),
            all(c(count_col, species_col, year_col) %in% colnames(counts_sf)))
  # Group counts, call rasterize on each group, convert back to df
  rasterized_df <- counts_sf %>%
    group_by(.data[[species_col]], .data[[year_col]]) %>%
    group_modify(
      function(rows, key) {
        result <- raster::rasterize(
          as_Spatial(st_transform(rows, ant_proj())),
          ant_raster_template(limits, res),
          field = count_col,
          fun = "sum",
          na.rm = TRUE
        ) %>%
          raster::as.data.frame(xy = TRUE) %>%
          rename(!!layer_name := layer)
      }
    )

  # Convert df to stars and set projection
  stars::st_as_stars(
    rasterized_df,
    dims = 1:4,
    xy = 3:4
  ) %>%
    st_set_crs(ant_proj())
}

#' Convert latitude/longitude data to sf object
#'
#' @param df Data frame with spatial data
#' @param coords Names of columns containing longitude and latitude
#'
#' @return df converted to sf object
#' @export
latlon_to_sf <- function(df, coords = c("x", "y")) {
  sf::st_as_sf(df,
               coords = coords,
               crs = sf::st_crs("+proj=longlat +datum=WGS84"))
}

#' Create basemap of study region
#'
#' @return ggplot object
#' @export
ant_basemap <- function() {
  ant_sf <- read_obj("ant_sf")
  ggplot() +
    geom_sf(data = ant_sf) +
    theme_minimal() +
    theme(axis.title = element_blank())
}

coord_ant <- function(map_lim = NULL) {
  if (is.null(map_lim)) {
    map_lim <- st_bbox(ant_lims(), crs = "EPSG:4326") %>%
      project_bbox()
  }
  coord_sf(xlim = map_lim[c("xmin", "xmax")],
           ylim = map_lim[c("ymin", "ymax")],
           expand = FALSE,
           crs = ant_proj())
}

#' Project a bounding box
#'
#' @param x sf::bbox
#'
#' @return sf::bbox x projected from lat/lon into Antarctic projection (see
#'   ant_proj())
#' @export
project_bbox <- function(x) {
  x %>%
    sf::st_as_sfc(crs = "EPSG:4326") %>%
    sf::st_transform(ant_proj()) %>%
    sf::st_bbox()
}

#' Expand a bounding box
#'
#' @param x sf::bbox
#'
#' @return sf::bbox x expanded by factor
#' @export
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

nmi_to_km <- function(nmi) nmi * 1.852

#' Rasterize data (predator abundance or effort) by year or other category
#'
#' @param x_sf point data to rasterize
#' @param field field to rasterize
#' @param by grouping variable
#' @param fun rasterizing function
#' @param template raster template
#'
#' @return terra raster
#' @export
rasterize_by <- function(x_sf, field, by, fun, template) {
  result <- x_sf %>%
    terra::vect() %>%
    terra::rasterize(template,
                     field = field,
                     by = by,
                     fun = fun)
  result_names <- names(result) %>%
    str_extract("c[(]([^,]+)", 1) %>%
    str_replace_all("\\\"", "")
  names(result) <- ifelse(
    !is.na(result_names),
    result_names,
    names(result)
  )
  result
}

make_overview_map <- function() {
  map_bbox <- st_bbox(c(xmin = -64, xmax = -53, ymin = -64.5, ymax = -58.5),
                      crs = "EPSG:4326") %>%
    project_bbox()
  ant_basemap() +
    coord_ant(map_bbox)
}
