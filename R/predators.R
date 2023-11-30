aggregate_predators <- function(predators) {
  predators %>%
    filter(species != "NULL") %>%
    mutate(UTC_start = lubridate::mdy_hm(UTC_start),
           year = lubridate::year(UTC_start),
           km = nmi_to_km(nmi)) %>%
    group_by(year, interval, species, UTC_start, lon_mean, lat_mean, nmi, km) %>%
    summarize(count = sum(count_species), .groups = "drop")
}

filter_species <- function(sightings, station_thr) {
  total_stations <- n_distinct(sightings$amlr.station)
  sightings %>%
    group_by(species) %>%
    summarize(n_stations = n_distinct(amlr.station)) %>%
    filter(n_stations >= station_thr * total_stations,
           !str_starts(species, "UN")) %>%
    semi_join(x = sightings, y = ., by = "species")
}

assign_sightings <- function(sightings, stations, max_dist_km, max_days) {
  stopifnot(inherits(sightings, "sf"),
            inherits(stations, "sf"))

  # Assign sightings to stations within year
  assign_group <- function(sightings_group, sightings_key) {
    station_buffers <- stations %>%
      filter(Year == sightings_key$year) %>%
      st_transform(ant_proj()) %>%
      st_buffer(max_dist_km * 1000) %>%
      transmute(
        amlr.station,
        tow.UTC = date.tow + (start.time.UTC - as.POSIXct("1899-12-31", tz = "UTC"))
      )
    sightings_group %>%
      st_transform(ant_proj()) %>%
      st_join(station_buffers, left = FALSE) %>%
      mutate(lag_days = abs(as.numeric(UTC_start - tow.UTC, units = "secs")) / 3600 / 24) %>%
      filter(lag_days <= max_days)
  }

  # Assign sightings across years
  sightings %>%
    group_by(year) %>%
    group_modify(assign_group) %>%
    ungroup() %>%
    st_as_sf()
}

# Normalize species counts by survey effort
normalize_counts <- function(sightings, effort) {
  sightings %>%
    as_tibble() %>%
    group_by(amlr.station, species) %>%
    summarize(count = sum(count), .groups = "drop") %>%
    right_join(select(as_tibble(effort), amlr.station, survey_nmi, survey_km),
               by = "amlr.station") %>%
    mutate(count_nmi = count / survey_nmi,
           count_km = nmi_to_km(count_nmi))
}

code_to_common <- function(species_code) {
  c(ADPN = "Adélie penguin",
    ANFU = "Southern fulmar",
    ANPT = "Antarctic petrel",
    ANPR = "Antarctic prion",
    ANSH = "Antarctic shag",
    ANTE = "Antarctic tern",
    BBAL = "Black-browed albatross",
    BLPT = "Blue petrel",
    CAPT = "Cape petrel",
    CHPN = "Chinstrap penguin",
    CODP = "Common diving petrel",
    CRSE = "Crabeater seal",
    ELSE = "Elephant seal",
    EMPN = "Emperor penguin",
    FUSE = "Antarctic fur seal",
    GEPN = "Gentoo penguin",
    HUWH = "Humpback whale",
    KEGU = "Kelp gull",
    KEPT = "Kerguelen petrel",
    KIWH = "Killer whale",
    LESE = "Leopard seal",
    MIWH = "Minke whale",
    NGPT = "Northern giant petrel",
    PFSB = "Pale-faced sheathbill",
    ROSE = "Ross seal",
    SBWH = "Bottlenose whale",
    SGPT = "Southern giant petrel",
    SNPT = "Snow petrel",
    SPSK = "South polar skua",
    WESE = "Weddell seal")[species_code]
}
