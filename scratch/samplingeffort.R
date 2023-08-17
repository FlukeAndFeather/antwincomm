dat <- predators_clust %>%
  group_by(amlr.station) %>%
  summarize(n_sp = n_distinct(species),
            effort = survey_nmi[1],
            pred_clust = pred_clust[1])

ggplot(dat, aes(effort, n_sp, color = pred_clust)) +
  geom_point(alpha = 0.8, position = position_jitter(width = 2, height = 0.2)) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Survey effort (nmi)", y = "# species") +
  theme_classic() +
  theme(legend.position = c(1, 0),
        legend.justification = c(1.1, -0.1),
        legend.title = element_blank())

ggplot(dat, aes(pred_clust, effort, fill = pred_clust)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Dark2") +
  labs(y = "Survey effort (nmi)") +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        legend.position = c(0, 1),
        legend.justification = c(-0.1, 1),
        legend.title = element_blank())

dat %>%
  group_by(pred_clust) %>%
  summarize(median(effort))

# dissimilarity ~ difference in survey effort
bc_dist <- as.matrix(sightings_dist) %>%
  as_tibble(rownames = "amlr.station1") %>%
  pivot_longer(-amlr.station1, names_to = "amlr.station2", values_to = "dist")
dat2 <- expand_grid(
  select(dat,
         amlr.station1 = amlr.station,
         effort1 = effort),
  select(dat,
         amlr.station2 = amlr.station,
         effort2 = effort)
) %>%
  mutate(diff_effort = abs(effort1 - effort2)) %>%
  left_join(bc_dist, by = c("amlr.station1", "amlr.station2")) %>%
  filter(amlr.station1 != amlr.station2)
ggplot(dat2, aes(diff_effort, dist)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", formula = y ~ x) +
  labs(x = "Effort difference (nmi)",
       y = "Bray-Curtis dissimilarity") +
  theme_classic()
