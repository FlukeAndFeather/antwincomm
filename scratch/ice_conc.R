library(ncdf4)
library(terra)
library(tidyterra)
library(tidyverse)

proj_ext <- ext(-67, -50, -68, -55) %>%
  project(from = "epsg:4326",
          to = "epsg:3412")
url <- str_glue("
https://polarwatch.noaa.gov/erddap/griddap/nsidcG02202v4nhmday.csv?cdr_seaice_conc_monthly%5B(2012-08-01T00:00:00Z):12:(2016-08-01T00:00:00Z)%5D%5B({round(proj_ext[3])}):1:({round(proj_ext[4])})%5D%5B({round(proj_ext[1])}):1:({round(proj_ext[2])})%5D
")

seaice_df <- read_csv("~/Downloads/nsidcG02202v4nhmday_16d4_f5d0_af8c.csv",
                      col_names = c("time",
                                    "y",
                                    "x",
                                    "seaice_conc"),
                      col_types = "Tddd",
                      skip = 2) %>%
  mutate(year = lubridate::year(time))
seaice_rast <- seaice_df %>%
  split()
rast(seaice_df,
                    type = "xy",
                    crs = "epsg:3412",
                    extent = proj_ext)

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

ice_contour <- map(
  seq(nlyr(ice_conc)),
  ~ as.contour(ice_conc[[.x]], levels = 0.65)
) %>%
  vect() %>%
  mutate(Year = 2012:2016)

ice_conc_df <- as.data.frame(ice_conc)

plot(ice_conc)

ggplot() +
  geom_spatraster(data = ice_conc) +
  geom_sf(data = ice_contour, color = "white") +
  coord_ant() +
  facet_wrap(~lyr)


