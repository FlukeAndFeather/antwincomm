library(ncdf4)
library(terra)
library(tidyterra)
library(tidyverse)

ext(-67, -50, -68, -55) %>%
  project(from = "epsg:4326",
          to = "epsg:3412")

ice_nc <- nc_open("~/Downloads/nsidcG02202v4shmday_df31_2bad_59c8_U1688308936040.nc")
ice_conc_arr <- ncvar_get(ice_nc, "cdr_seaice_conc_monthly") %>%
  # NETcdf stores values reversed along x-axis
  apply(3, pracma::fliplr, simplify = FALSE) %>%
  simplify2array()
ice_conc_ext <- ext(
  min(ncvar_get(ice_nc, "xgrid")),
  max(ncvar_get(ice_nc, "xgrid")),
  min(ncvar_get(ice_nc, "ygrid")),
  max(ncvar_get(ice_nc, "ygrid"))
)
ice_conc <- rast(
  ice_conc_arr,
  crs = "epsg:3412",
  ice_conc_ext
)

plot(ice_conc)

ggplot() +
  geom_spatraster(data = ice_conc) +
  geom_spatraster_contour(data = ice_conc, breaks = 0.65, color = "white") +
  facet_wrap(~lyr)


