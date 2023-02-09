read_zoop <- function(zoop_path) {
  readxl::read_excel(zoop_path, sheet = 1) %>%
    mutate(time.of.day = factor(time.of.day,
                                levels = c("D", "T", "N"),
                                labels = c("Day", "Twilight", "Night")))
}
