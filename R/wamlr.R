check_wamlr <- function(wamlr_path) {
  if (!dir.exists(wamlr_path))
    stop(str_glue("WAMLR directory not found at {wamlr_path}. Follow the ",
                  "instructions in the README for downloading these data."))
  wamlr_path
}
