# Use WGS 84 / Antarctic Polar Stereographic for mapping, recentered on ant_lims()
ant_proj <- function() {
  lat_ts <- (ant_lims()["ymin"] + ant_lims()["ymax"]) / 2
  lon_0 <- (ant_lims()["xmin"] + ant_lims()["xmax"]) / 2
  glue::glue(
    "+proj=stere +lat_0=-90 +lat_ts={lat_ts} +lon_0={lon_0} +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs"
  ) %>%
  st_crs()
}

ant_lims <- function() {
  c(xmin = -67, xmax = -50, ymin = -68, ymax = -55)
}

download_ne <- function(ne_dir) {
  if (!dir.exists(ne_dir)) dir.create(ne_dir, recursive = TRUE)
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
  st_crop(land, st_bbox(limits))
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

latlon_to_sf <- function(df, coords = c("x", "y")) {
  st_as_sf(df,
               coords = coords,
               crs = sp::CRS("+proj=longlat +datum=WGS84"))
}

ant_basemap <- function() {
  ggplot() +
    geom_sf(data = tar_read("ant_sf", store = here::here("_targets"))) +
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

project_bbox <- function(x) {
  x %>%
    st_as_sfc(crs = "EPSG:4326") %>%
    st_transform(ant_proj()) %>%
    st_bbox()
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

nmi_to_km <- function(nmi) nmi * 1.852

rasterize_by <- function(x_sf, field, by, fun, template) {
  result <- x_sf %>%
    vect() %>%
    rasterize(template,
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
