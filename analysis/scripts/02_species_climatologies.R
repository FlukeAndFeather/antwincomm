# Load this research compendium as a package
# This is functionally equivalent to library(antwincomm)
devtools::load_all()

# Load necessary derived data
predators_agg <- read_obj("predators_agg")
effort_sf <- read_obj("effort_sf")


# Top six -----------------------------------------------------------------

# Find six most abundant species
top_six <- predators_agg %>%
  group_by(species) %>%
  summarize(total_count = sum(count)) %>%
  arrange(desc(total_count)) %>%
  slice(1:6) %>%
  mutate(common_name = code_to_common(species))

# Convert abundant species' data to sf
topsix_sf <- predators_agg %>%
  semi_join(top_six, by = "species") %>%
  left_join(select(top_six, species, common_name), by = "species") %>%
  latlon_to_sf(coords = c("lon_mean", "lat_mean"))

# Create a template raster of the study area
effort_ext <- effort_sf %>%
  sf::st_transform(ant_proj()) %>%
  terra::ext()
rast_template <- terra::rast(crs = as.character(ant_proj())[[1]],
                             extent = effort_ext,
                             resolution = 30e3)

# Rasterize effort by year
effort_rast <- effort_sf %>%
  sf::st_transform(ant_proj()) %>%
  rasterize_by("km", "year", sum, rast_template)

# Rasterize the predator sightings by year and species
topsix_rast <- topsix_sf %>%
  sf::st_transform(ant_proj()) %>%
  mutate(year_commonname = as.character(interaction(year, common_name))) %>%
  rasterize_by("count", "year_commonname", sum, rast_template)

# Normalize counts by effort
topsix_norm <- topsix_rast
for (i in seq(terra::nlyr(topsix_rast))) {
  lyr_name <- names(topsix_rast)[i]
  lyr_year <- substr(lyr_name, 1, 4)
  effort_year <- effort_rast[lyr_year] %>%
    terra::subst(0, NA)
  topsix_norm[[i]] <- topsix_norm[[i]] / effort_year
}

# Average across years
topsix_climatology <- unique(top_six$common_name) %>%
  map(function(sp) {
    sp_rast <- terra::subset(topsix_norm,
                             which(str_detect(names(topsix_norm), sp)))
    terra::mean(sp_rast, na.rm = TRUE)
  }) %>%
  set_names(unique(top_six$common_name)) %>%
  terra::rast()

# Classify raster (otherwise CRSE throw it all off)
topsix_clim_class <- topsix_climatology %>%
  terra::classify(rcl = c(0, 1, 5, 10, 50))


# All species -------------------------------------------------------------

pred_sf <- predators_agg %>%
  filter(!str_starts(species, "U")) %>%
  mutate(common_name = code_to_common(species)) %>%
  latlon_to_sf(coords = c("lon_mean", "lat_mean"))

pred_rast <- pred_sf %>%
  sf::st_transform(ant_proj()) %>%
  mutate(year_commonname = as.character(interaction(year, common_name))) %>%
  rasterize_by("count", "year_commonname", sum, rast_template)

pred_norm <- pred_rast
for (i in seq(terra::nlyr(pred_rast))) {
  lyr_name <- names(pred_rast)[i]
  lyr_year <- substr(lyr_name, 1, 4)
  effort_year <- effort_rast[lyr_year] %>%
    terra::subst(0, NA)
  pred_norm[[i]] <- pred_norm[[i]] / effort_year
}

pred_climatology <- unique(pred_sf$common_name) %>%
  map(function(sp) {
    sp_rast <- terra::subset(pred_norm,
                             which(str_detect(names(pred_norm), sp)))
    terra::mean(sp_rast, na.rm = TRUE)
  }) %>%
  set_names(unique(pred_sf$common_name)) %>%
  terra::rast()

map_bbox <- terra::ext(-63, -54, -64, -59) %>%
  terra::project(from = "+proj=longlat +datum=WGS84",
                 to = as.character(ant_proj())[[1]])

pred_class <- pred_climatology %>%
  terra::classify(rcl = c(0, 1, 5, 10, 50, 100, 1000))

pred_class_df <- as.data.frame(pred_class, xy = TRUE) %>%
  pivot_longer(-c("x", "y"),
               names_to = "name",
               values_to = "count") %>%
  drop_na(count) %>%
  mutate(name = map_chr(name, \(n) paste(strwrap(n, 20), collapse = "\n"))) %>%
  # one sighting each, maps not useful
  filter(!name %in% c("Antarctic prion", "Humpback whale"))

# Save to derived data
save_obj(pred_class_df, "pred_class_df")
save_obj(topsix_clim_class, "topsix_clim_class", type = "rast")

message("Species climatologies generated")
