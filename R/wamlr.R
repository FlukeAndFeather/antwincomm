download_wamlr <- function(wamlr_dir) {
  # Don't proceed if the WAMLR data directory has files in it
  if (length(dir(wamlr_dir)) > 0)
    return()

  # We need the OSF_PAT to proceed
  if (Sys.getenv("OSF_PAT") == "")
    stop("No OSF_PAT environment variable defined. Create an OSF personal access token and define OSF_PAT in .Renviron (see https://docs.ropensci.org/osfr/articles/auth.html)")

  # Retreive the WAMLR node
  antwincomm_prj <- osfr::osf_retrieve_node("https://osf.io/hwnvy/")
  antwincomm_data <- filter(osfr::osf_ls_files(antwincomm_prj),
                            name == "data")
  antwincomm_wamlr <- filter(osfr::osf_ls_files(antwincomm_data),
                             name == "WAMLR")

  # Download data
  osfr::osf_download(antwincomm_wamlr,
                     path = dirname(wamlr_dir), # So we don't get WAMLR/WAMLR/
                     recurse = TRUE)
}
