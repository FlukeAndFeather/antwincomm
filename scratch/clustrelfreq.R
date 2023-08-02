library(sf)
library(tidyverse)

tar_load(c("predators_stations", "sightings_clust", "zoop"))
clust <- cutree(sightings_clust, 3)
station_ice <- predators_stations %>%
  distinct(amlr.station, ice_type, ice_coverage) %>%
  as_tibble()
pred_prey <- tibble(
  amlr.station = names(clust),
  pred_clust = factor(clust,
                      labels = c("Open", "Marginal", "Pagophilic"))
) %>%
  left_join(select(zoop,
                   amlr.station,
                   Year,
                   zoop_clust = `Winter Cluster factor`),
            by = "amlr.station") %>%
  left_join(station_ice, by = "amlr.station")

pred_prey %>%
  mutate(zoop_clust2 = substr(zoop_clust, 1, 1)) %>%
  crosstable(c(zoop_clust2, ice_coverage),
             by = pred_clust,
             total = "both") %>%
  as_flextable()


# Multinomial regression --------------------------------------------------

library(nnet)
pred_prey2 <- pred_prey %>%
  mutate(zoop_clust2 = substr(zoop_clust, 1, 1),
         pred_clust = relevel(pred_clust, ref = "Open"))
test <- multinom(pred_clust ~ 0 + zoop_clust + ice_coverage,
                 data = pred_prey2)
summary(test)
z <- summary(test)$coefficients/summary(test)$standard.errors
z

test_pred <- expand_grid(
  zoop_clust = c("1", "2a", "2b", "3a", "3b"),
  ice_coverage = seq(0, 1, by = 0.1)
) %>%
  mutate(class_probs = as.data.frame(predict(test,
                                             newdata = .,
                                             type = "probs"))) %>%
  unpack(class_probs) %>%
  pivot_longer(c(Open, Marginal, Pagophilic),
               names_to = "pred_clust",
               values_to = "probs")
pred_freq <- pred_prey %>%
  group_by(pred_clust) %>%
  summarize(n = n()) %>%
  mutate(frac = n / sum(n))
ggplot(test_pred,
       aes(ice_coverage, probs, color = pred_clust)) +
  geom_segment(aes(x = 0, y = frac, xend = 1, yend = frac, color = pred_clust),
               pred_freq,
               linetype = "dashed") +
  geom_line() +
  facet_grid(cols = vars(zoop_clust)) +
  theme_classic()

# Is "open" right? --------------------------------------------------------

predators_stations %>%
  semi_join(filter(pred_prey, pred_clust == "Open"),
            by = "amlr.station") %>%
  as.data.frame() %>%
  select(amlr.station, species, count_norm) %>% View()
  pivot_wider(names_from = "species", values_from = "count_norm")
