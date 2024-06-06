#' Save an object to derived_data/
#'
#' @param obj object to be saved
#' @param obj_name name of object
#' @param type whether object is a basic R object, raster, or vector. Raster and
#'   vector data require additional processing.
#'
#' @return nothing, but saves object in analysis/data/derived_data/
#' @export
save_obj <- function(obj, obj_name, type = c("r", "rast", "vect")) {
  type = match.arg(type)
  switch (type,
    r = {
      path <- here::here(str_glue("analysis/data/derived_data/{obj_name}.rds"))
      saveRDS(obj, path)
    },
    rast = {
      path <- here::here(str_glue("analysis/data/derived_data/{obj_name}.tif"))
      terra::writeRaster(obj, path, overwrite = TRUE)
    },
    vect = {
      path <- here::here(str_glue("analysis/data/derived_data/{obj_name}.shp"))
      terra::writeVector(obj, path, overwrite = TRUE)
    }
  )
}

#' Read an object from derived_data/
#'
#' @param obj_name name of object
#' @param type whether object is a basic R object, raster, or vector. Raster and
#'   vector data require additional processing.
#'
#' @return object read from file in analysis/data/derived_data/
#' @export
read_obj <- function(obj_name, type = c("r", "rast", "vect")) {
  type = match.arg(type)
  switch (
    type,
    r = readRDS(here::here(str_glue("analysis/data/derived_data/{obj_name}.rds"))),
    rast = {
      path <- here::here(str_glue("analysis/data/derived_data/{obj_name}.tif"))
      terra::rast(path)
    },
    vect = {
      path <- here::here(str_glue("analysis/data/derived_data/{obj_name}.shp"))
      terra::vect(path)
    }
  )
}

# The following allows internal use of some tidyverse packages without using ::
#' @import dplyr
#' @import forcats
#' @import ggplot2
#' @import purrr
#' @import readr
#' @import stringr
#' @import tidyr
NULL
