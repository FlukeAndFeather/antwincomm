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
predators <- read_csv("data/WAMLR/WAMLR-2012-2016-Underway-Predator-Sightings.csv")
list(
  tar_target(
    predators_file,
    here("data", "WAMLR", "WAMLR-2012-2016-Underway-Predator-Sightings.csv"),
    format = "file"
  ),
  tar_target(predators, readr::read_csv(predators_file)),
  tar_target(predators_agg, aggregate_predators(predators)),
  tar_quarto(predator_summary, here("reports", "01_predators.qmd"))
)
