for (script in dir("analysis/scripts", "^[0-9]", full.names = TRUE)) {
  source(script)
}
