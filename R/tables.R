#' Make a table with predator occurrence and additional details
#'
#' @param predators predator data aggregated to stations
#' @param stations station-level data
#'
#' @return knitr::knitr_kable
#' @export
make_predatortbl <- function(predators, stations) {
  format_percent <- scales::label_percent(0.1)

  predator_context <- tribble(
    ~species, ~`Typical prey`, ~`Foraging behavior`,     ~Reference,
    "SNPT",   "Myctophids",    "Surface feeding (<5 m)", "@spearMorphologicalDifferencesRelative1998; @delordSpeciesSpecificForaging2016",
    "ANPT",   "Myctophids, krill, squid",    "Surface feeding (<5 m)", "@spearMorphologicalDifferencesRelative1998; @delordAntarcticPetrelsIce2020",
    "SGPT",   "Juvenile pinnipeds and seabirds, carrion",    "Scavenging, surface feeding (<5 m)", "@hunterFoodFeedingEcology1983",
    "FUSE",   "Krill, myctophids",    "Diving (~15-50 m)", "@stanilandForagingBehaviourTwo2011",
    "KEGU",   "Limpets, krill",    "Surface feeding (<5 m)", "@silvaDoesAccessHighquality2001",
    "ANFU",   "Myctophids, krill, squid",    "Surface feeding (<5 m)", "@ridoux1989diets; @spearMorphologicalDifferencesRelative1998",
    "CAPT",   "Krill, myctophids",    "Surface feeding (<5 m)", "@ridoux1989diets; @spearMorphologicalDifferencesRelative1998",
    "ADPN",   "Krill",    "Diving (~15-50 m)", "@ciminoClimatedrivenSympatryMay2016; @juaresDietAdeliePenguins2018",
    "CRSE",   "Krill",    "Diving (~50-100 m)", "@burnsWinterHabitatUse2004; @huckstadtDietSpecialistChanging2012",
    "BLPT",   "Krill, myctophids, amphipods",    "Surface feeding (<5 m)", "@princeFoodFeedingEcology1980",
    "LESE",   "Seals, penguins, fish, rkill",    "Diving (~50-100 m)", "@walkerSeasonalOccurrenceDiet1998; @casauxDietLeopardSeal2009; @krauseNovelForagingStrategies2015",
    "ANTE",   "Krill, myctophids",    "Surface feeding (<5 m)", "@croxallFoodFeedingEcology1980; @ainley1992does",
    "PFSB",   "Carrion",    "Scavenging", "@faveroForagingEcologyPaleFaced1996",
    "MIWH",   "Krill",    "Diving (~5-100 m)", "@friedlaenderFeedingRatesUnderice2014",
    "WESE",   "Fish, squid",    "Diving (~100-350 m)", "@testaOverwinterMovementsDiving1994; @lakeRegionalTemporalFinescale2003",
    "GEPN",   "Krill, fish",    "Diving (~25-100 m)", "@croxallFoodFeedingEcology1980; @ciminoClimatedrivenSympatryMay2016",
    "ELSE",   "Myctophids, squid",    "Diving (~200-800 m)", "@guinetSouthernElephantSeal2014; @daneriFeedingHabitsSouthern2015",
    "KIWH",   "Whales, seals, toothfish, penguins",    "Complex and diverse", "@pitmanThreeFormsKiller2003",
    "EMPN",   "Krill, silverfish",    "Diving (~50-300 m)", "@kirkwoodForagingEcologyFemale1997; @rodaryBenthicDivingMale2000",
    "SBWH",   "Squid",    "Diving (>500 m)", "@macleodReviewDataDiets2003",
    "ANSH",   "Demersal fish",    "Diving (<50 m)", "@casauxShagsAntarcticaTheir2006",
    "ROSE",   "Myctophids, squid",    "Diving (~100-300 m)", "@blixRossSealOmmatophoca2007; @southwellReviewDataAbundance2012"
  )

  n_sites <- nrow(stations)

  predators %>%
    mutate(common = code_to_common(species),
           scientific = code_to_scientific(species),
           Species = str_glue("{common} _{scientific}_")) %>%
    group_by(Species, species) %>%
    summarize(Ind = sum(count),
              Sites = n(),
              `Freq (%)` = format_percent(Sites / n_sites[1]),
              .groups = "drop") %>%
    left_join(predator_context, by = "species") %>%
    select(-species) %>%
    arrange(desc(Sites)) %>%
    knitr::kable()
}

#' Create indicator species table
#'
#' @param indval labdsv::indval Indicator value object
#'
#' @return knitr::kable table
#' @export
make_indicatortbl <- function(indval) {
  indval_mtx <- indval$indval
  cluster_names <- c("Pack ice", "Open water", "Marginal ice")

  # Filter to species >-0.25, format nicely
  apply(indval_mtx, 2, \(col) {
    tibble(
      `Indicator species` = code_to_common(names(col)[col >= 0.25]),
      `Indicator value` = round(col[col >= 0.25], 2)
    )
  }) %>%
    set_names(cluster_names) %>%
    bind_rows(.id = "Predator cluster") %>%
    arrange(`Predator cluster`, desc(`Indicator value`)) %>%
  # Create table
    knitr::kable()
}

#' Create predator community frequency table
#'
#' Note: originally I implemented this using flextable output, but there's a bug
#' involving flextable, docx output, and table captions. See
#' https://github.com/quarto-dev/quarto-cli/issues/9922.
#'
#' @param stations_clust Station-level cluster data
#'
#' @return knitr::kable table
#' @export
make_commfreqtbl <- function(stations_clust) {
  # Add a "total" row to the table
  add_total_row <- function(df) {
    total_row <- tibble(Year = "Total") %>%
      cbind(summarize(df, across(-Year, sum)))
    rbind(df, total_row)
  }

  # Format number of clusters as number and frequency
  format_cluster <- function(c, t) {
    str_glue("{c}\t{round(c / t * 100, 2)}")
  }

  stations_clust %>%
    as_tibble() %>%
    count(Year, pred_clust) %>%
    pivot_wider(names_from = "pred_clust", values_from = "n") %>%
    add_total_row() %>%
    mutate(total = `Open water` + `Marginal ice` + `Pack ice`,
           across(c("Open water", "Marginal ice", "Pack ice"),
                  \(c) format_cluster(c, total))) %>%
    select(-total) %>%
    knitr::kable()
}
