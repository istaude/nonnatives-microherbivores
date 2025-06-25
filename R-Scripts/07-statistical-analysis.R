source("Rscripts/00-preamble.R")

# load data ---------------------------------------------------------------

data <- read_csv("Data/mean_centroids_filled.csv")

# ensure certain data thresholds are met
# non-natives must have there minimum introduction data later than 1500 in Europe
data <- data %>% filter(min_fr > 1500)

# ensure we have non-nativs that are at least 2500 km away from center of europe
# with their centroid
data <- data %>% 
  filter(distance_to_europe_center > 2500)

# how many species have all data available
data %>% 
  select(n, area_km2, min_fr, distance_to_europe_center, woodiness,
         rel_status, family, taxon_name) %>% 
  na.omit() %>% 
  nrow()

# model -------------------------------------------------------------------

# data prep
data <- data %>% 
  select(n, area_km2, min_fr, distance_to_europe_center, woodiness,
         rel_status, family, taxon_name) %>% 
  na.omit() %>%
  mutate(
    rel_status = as.factor(rel_status),
    log10_n = log10(n),
    log10_area = log10(area_km2),
    woodiness = recode(woodiness, "Non-Woody" = "Non-woody")
  )


# model
mod_1 <- lme4::lmer(log10_n ~ log10_area * woodiness + 
                      min_fr * woodiness + 
                      distance_to_europe_center * woodiness + 
                      rel_status * woodiness + 
                      (1 | family), data)


mod_1.2 <- lmerTest::lmer(log10_n ~ log10_area * woodiness + 
                            min_fr * woodiness + 
                            distance_to_europe_center * woodiness + 
                            rel_status * woodiness + 
                          (1|family), data)

anova(mod_1.2)  
summary(mod_1.2) 
MuMIn::r.squaredGLMM(mod_1)

# model diagnostics look fine
simulationOutput <- simulateResiduals(fittedModel = mod_1, n = 1000)
plot(simulationOutput)

# understand how much variance each predictor explains
# Run partR2 (takes a bit of time)
res_grouped <- partR2(
  mod_1,
  partbatch = list(
    range_size = c("log10_area", "log10_area:Woodiness"),
    min_fr = c("min_fr", "min_fr:Woodiness"),
    distance = c("distance_to_europe_center", "distance_to_europe_center:Woodiness"),
    status = c("rel_status", "rel_status:Woodiness"),
    woodiness = "Woodiness"
  ),
  data = data,
  nboot = 1000
)

saveRDS(res_grouped, file = "Data/res_grouped.rds")
res_grouped <- readRDS("Data/res_grouped.rds")
res_grouped

# get slopes for each predictor/interaction
emtrends(mod_1, specs = "woodiness", var = "log10_area")
10^0.218
10^0.308
emtrends(mod_1, specs = "woodiness", var = "min_fr")
10^(-0.000976 * -100)
10^(-0.001402 * -100)
emtrends(mod_1, specs = "woodiness", var = "distance_to_europe_center")
emmeans(mod_1, ~ rel_status | woodiness)
summary(contrast(emmeans(mod_1, ~ rel_status | woodiness), method = "pairwise"))
10^0.739
10^0.766
10^0.672

10^0.424
10^0.458
10^0.445
# data visualization ------------------------------------------------------

# predicted lines for each predictor variable
# log_area
pr1 <- predict_response(mod_1, c("log10_area [1.953329:6.954327]", "woodiness")) 
pr1_data <- pr1 %>% as.tibble()
names(pr1_data)

# ok, a bit more complicated, but use this to get the range size at which
# microherbivore interaction richness is similar to natives
log10_area_seq <- seq(1.95, 6.95, length.out = 100)
new_data <- expand.grid(
  log10_area = log10_area_seq,
  woodiness = unique(data$woodiness),
  min_fr = mean(data$min_fr, na.rm = TRUE),
  distance_to_europe_center = mean(data$distance_to_europe_center, na.rm = TRUE),
  rel_status = levels(data$rel_status)[1] ,
  family = NA  # random effects are usually marginalized
)
new_data$log10_n_pred <- predict(mod_1, newdata = new_data, re.form = NA)
# get closest corresponding numbers from prediction
new_data %>%
  filter(woodiness == "Non-woody") %>%
  arrange(abs(log10_n_pred - log10(4.14))) %>%
  slice(1)
10^6.04

new_data %>%
  filter(woodiness == "Woody") %>%
  arrange(abs(log10_n_pred - log10(6.17))) %>%
  slice(1)
10^5.43



# introduction date  
pr2 <- predict_response(mod_1, c("min_fr", "woodiness"))
pr2_data <- pr2 %>% as.data.frame()

# get the range size at which microherbivore interaction richness is similar to natives
intro_date_seq <- seq(min(data$min_fr, na.rm = TRUE),
                      max(data$min_fr, na.rm = TRUE),
                      length.out = 100)

new_data_intro <- expand.grid(
  min_fr = intro_date_seq,
  woodiness = unique(data$woodiness),
  log10_area = mean(data$log10_area, na.rm = TRUE),
  distance_to_europe_center = mean(data$distance_to_europe_center, na.rm = TRUE),
  rel_status = levels(data$rel_status)[1],
  family = NA
)

new_data_intro$log10_n_pred <- predict(mod_1, newdata = new_data_intro, re.form = NA)

new_data_intro %>%
  filter(woodiness == "Non-woody") %>%
  arrange(abs(log10_n_pred - log10(4.14))) %>%
  slice(1)
2025 - 1719
new_data_intro %>%
  filter(woodiness == "Woody") %>%
  arrange(abs(log10_n_pred - log10(6.17))) %>%
  slice(1)
2025 - 1840


# geographic proximity
pr3 <- predict_response(mod_1, c("distance_to_europe_center", "woodiness"))
pr3_data <- pr3 %>% as.data.frame()
min(data$distance_to_europe_center, na.rm = TRUE) -
max(data$distance_to_europe_center, na.rm = TRUE)

exp(-2.55e-05 * -14868) # non-woody
exp(-3.46e-05 * -14868) # woody



# relatedness
pr4 <- predict_response(mod_1, c("rel_status", "woodiness"))
pr4_data <- pr4 %>% as.data.frame()



# all subplots
# plot a
pr1_occupancy <- ggplot(data = data, aes(x = area_km2, y = n)) +
  
  # Points: subtle fill, clear border
  geom_point(
    pch = 21,
    aes(fill = woodiness, col = woodiness),
    alpha = 0.4,      
    size = 2, show.legend = FALSE
  ) +
  
  # Axes in log scale
  scale_x_log10() +
  scale_y_log10() +
  
  # Smooth lines and ribbons
  geom_smooth(
    data = pr1_data,
    aes(
      x = 10^x,
      y = 10^predicted,
      ymin = 10^conf.low,
      ymax = 10^conf.high,
      colour = group,
      fill = group
    ),
    stat = "identity",
    inherit.aes = FALSE,
    size = 1.2,        
    alpha = 0.2       
  ) +
  
  # Colors
  scale_color_manual(values = c(
    "Woody" = "#6E6E6E",       # sleek dark grey line
    "Non-woody" = "#792c9e"    # elegant purple
  )) +
  scale_fill_manual(values = c(
    "Woody" = "#C8C8C8",       # mid grey fill, better contrast
    "Non-woody" = "#d1b4e0"    # softer purple fill
  )) +
  
  # Labels and theme
  labs(
    x = "Range size in Europe [km²]",
    y = "Microherbivore species per plant"
  ) +
  theme_minimal(base_family = "roboto") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(size = 11, vjust = -1),
    axis.ticks.y = element_line(),
    axis.ticks.x = element_line(),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    strip.text = element_text(size = 11, face = "italic")
  )


# plot b
pr2_firstrecord <- ggplot(data = data, aes(x = min_fr, y = n)) +
  geom_point(aes(fill = woodiness, col = woodiness), pch = 21, alpha = 0.4, size = 2, show.legend = FALSE) +
  scale_y_log10() +
  geom_smooth(data = pr2_data,
              aes(x = x, y = 10^predicted, ymin = 10^conf.low, ymax = 10^conf.high,
                  fill = group, colour = group),
              stat = "identity", inherit.aes = FALSE, alpha = 0.2, size = 1.2, 
              show.legend = FALSE) +
  scale_color_manual(values = c("Woody" = "#6E6E6E", "Non-woody" = "#792c9e")) +
  scale_fill_manual(values = c("Woody" = "#C8C8C8", "Non-woody" = "#d1b4e0")) +
  labs(x = "Introduction date", y = "Microherbivore species per plant") +
  theme_minimal(base_family = "roboto") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(size = 11, vjust = -1),
    axis.ticks = element_line(),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12, margin = margin(t = 10))
  )


# plot c
pr3_centroids <- ggplot(data = data, aes(x = distance_to_europe_center, y = n)) +
  geom_point(aes(fill = woodiness, col = woodiness), pch = 21, alpha = 0.4, size = 2, show.legend = FALSE) +
  scale_y_log10() +
  geom_smooth(data = pr3_data,
              aes(x = x, y = 10^predicted, ymin = 10^conf.low, ymax = 10^conf.high,
                  fill = group, colour = group),
              stat = "identity", inherit.aes = FALSE, alpha = 0.2, size = 1.2, show.legend = FALSE) +
  scale_color_manual(values = c("Woody" = "#6E6E6E", "Non-woody" = "#792c9e")) +
  scale_fill_manual(values = c("Woody" = "#C8C8C8", "Non-woody" = "#d1b4e0")) +
  labs(x = "Distance to Europe center [km]", y = "Microherbivore species per plant") +
  theme_minimal(base_family = "roboto") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(size = 11, vjust = -1),
    axis.ticks = element_line(),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12, margin = margin(t = 10))
  )


# plot d
data$rel_status <- factor(data$rel_status,
                          levels = c("Genus in Europe", 
                                     "Family in Europe, Genus not", 
                                     "No Relations"),
                          labels = c("Genus", "Family", "None"))

pr4_data$x <- factor(pr4_data$x,
                     levels = c("Genus in Europe", 
                                "Family in Europe, Genus not", 
                                "No Relations"),
                     labels = c("Genus", "Family", "None"))

pr4_relateness <- ggplot(data = data, aes(x = rel_status, y = n)) +
  geom_jitter(aes(fill = woodiness, col = woodiness), pch = 21, alpha = 0.4, size = 2, width = 0.15, show.legend = FALSE) +
  scale_y_log10() +
  geom_errorbar(data = pr4_data,
                  aes(x = x, y = 10^predicted, ymin = 10^conf.low, ymax = 10^conf.high,
                      colour = group),
                  inherit.aes = FALSE, size = 0.8, width = 0.15, lwd = 1, show.legend = FALSE) +

  scale_color_manual(values = c("Woody" = "#6E6E6E", "Non-woody" = "#792c9e")) +
  scale_fill_manual(values = c("Woody" = "#C8C8C8", "Non-woody" = "#d1b4e0")) +
  new_scale_fill() +
  geom_point(
    data = pr4_data,
    aes(
      x = x,
      y = 10^predicted,
      fill = group
    ),
    col = "black",
    shape = 21,
    size = 3,
    inherit.aes = FALSE,
    , show.legend = FALSE
  ) +
  scale_fill_manual(values = c("Woody" = "#6E6E6E", "Non-woody" = "#792c9e")) +
  labs(x = "Relationship level", y = "Microherbivore species per plant") +
  theme_minimal(base_family = "roboto") +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 11, vjust = -1),
    axis.ticks = element_line(),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12, margin = margin(t = 10))
  )



# plot arrangement
pr2_firstrecord <- pr2_firstrecord +
  theme(
    axis.title.y = element_blank(),
  )

pr4_relateness <- pr4_relateness +
  theme(
    axis.title.y = element_blank(),
  )


(pr1_occupancy + ggtitle("a") | pr2_firstrecord + ggtitle("b")) /
  (pr3_centroids + ggtitle("c") | pr4_relateness + ggtitle("d")) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = NULL) & 
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 12, face = "bold", family = "roboto"), 
    legend.title = element_blank()
  )

showtext_opts(dpi=600)
ggsave(filename = "Figures/explaining-non-natives.png",
       bg = "white",
       height = 8,
       width = 8,
       dpi = 600)
showtext_opts(dpi=96)


