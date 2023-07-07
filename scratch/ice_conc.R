library(terra)
library(tidyterra)
library(tidyverse)

seaice <- "~/Downloads/Aug sea ice concentration/" %>%
  dir(full.names = TRUE) %>%
  map(rast) %>%
  rast()
set.names(seaice, as.character(2012:2016))

proj_ext <- ext(-67, -50, -68, -55) %>%
  project(from = "epsg:4326",
          to = crs(seaice))
npen_seaice <- crop(seaice, proj_ext)
ice_contour <- map(
  seq(nlyr(npen_seaice)),
  ~ as.contour(npen_seaice[[.x]], levels = 700)
) %>%
  vect() %>%
  mutate(Year = 2012:2016,
         ice_conc = level / 1000)
plot(ice_contour)

ggplot() +
  geom_spatraster(data = ice_conc) +
  geom_sf(data = ice_contour, color = "white") +
  coord_ant() +
  facet_wrap(~lyr)


