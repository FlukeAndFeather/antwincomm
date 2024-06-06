# Load this research compendium as a package
# This is functionally equivalent to library(antwincomm)
devtools::load_all()

# Load necessary derived data
sightings_mtx <- read_obj("sightings_mtx")
sightings_cut <- read_obj("sightings_cut")
sightings_dist <- read_obj("sightings_dist")
zoop_sf <- read_obj("zoop_sf")
stations_ice <- read_obj("stations_ice")
predators_stations <- read_obj("predators_stations")

# NMDS
nmds_stress <- stress(sightings_mtx, k = 1:6)
nmds_sightings <- vegan::metaMDS(sightings_dist,
                                 k = 3,
                                 trymax = 100,
                                 trace = FALSE)
nmds_shepard <- shepard(nmds_sightings)
nmds_env <- env_data(zoop_sf, stations_ice, predators_stations)
nmds_df <- nmds_to_df(nmds_sightings, sightings_cut, nmds_env)
nmds_envfit <- vegan::envfit(nmds_sightings,
                             select(nmds_env, -amlr.station),
                             permutations = 999,
                             choices = 1:3,
                             na.rm = TRUE)

# Save derived data
save_obj(nmds_sightings, "nmds_sightings")
save_obj(nmds_env, "nmds_env")
save_obj(nmds_df, "nmds_df")
save_obj(nmds_envfit, "nmds_envfit")
