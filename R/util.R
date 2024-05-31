#' Save an object to derived_data/
#'
#' @param obj object to be saved
#' @param obj_name name of object
#' @param rast whether object is a terra raster. false by default. requires
#'   extra steps.
#'
#' @return nothing, but saves object in analysis/data/derived_data/
#' @export
save_obj <- function(obj, obj_name, rast = FALSE) {
  if (rast) {
    path <- here::here(str_glue("analysis/data/derived_data/{obj_name}.tif"))
    terra::writeRaster(obj, path, overwrite = TRUE)
  } else {
    path <- here::here(str_glue("analysis/data/derived_data/{obj_name}.rds"))
    saveRDS(obj, path)
  }
}

#' Read an object from derived_data/
#'
#' @param obj_name name of object
#' @param rast whether object is a terra raster. false by default. requires
#'   extra steps.
#'
#' @return object read from file in analysis/data/derived_data/
#' @export
read_obj <- function(obj_name, rast = FALSE) {
  if (rast) {
    path <- here::here(str_glue("analysis/data/derived_data/{obj_name}.tif"))
    terra::rast(path)
  } else {
    readRDS(here::here(str_glue("analysis/data/derived_data/{obj_name}.rds")))
  }
}

# The following allows internal use of some tidyverse packages without using ::
#' @import dplyr
#' @import ggplot2
#' @import purrr
#' @import readr
#' @import stringr
NULL
