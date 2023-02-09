library(here)
library(targets)
library(tarchetypes)
library(tidyverse)

# Set target options:
tar_option_set(
  packages = "tidyverse",
  format = "rds"
)

# Source scripts in R/
tar_source()

# Pipeline
list(
  # Spatial data
  tar_target(
    ne_dir,
    here("data", "geospatial", "ne"),
    format = "file"
  ),
  tar_target(
    ne_data,
    download_ne(ne_dir)
  ),
  tar_target(
    ant_sf,
    create_ant_sf(ne_dir)
  ),
  # Predators
  tar_target(
    predators_file,
    here("data", "WAMLR", "WAMLR-2012-2016-Underway-Predator-Sightings.csv"),
    format = "file"
  ),
  tar_target(predators, readr::read_csv(predators_file)),
  tar_target(predators_agg, aggregate_predators(predators)),
  # Zooplankton
  tar_target(
    zoop_file,
    here("data", "WAMLR", "WAMLR-2012-2016-Zooplnkton-Physics.xlsx"),
    format = "file"
  ),
  tar_target(zoop, read_zoop(zoop_file)),
  tar_target(
    zoop_long,
    pivot_longer(zoop, -(1:53), names_to = "taxa", values_to = "abundance")
  ),
  # Reports
  tar_quarto(reports, here("reports"))
)
