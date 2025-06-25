source("Rscripts/00-preamble.R")

# the aim of this script is to test whether non-natives that have accumulated 
# similar number of interactions compared to native plants, still interact mostly
# with genralist microherbivores

# load data ---------------------------------------------------------------

data <- read_csv("Data/dr.csv")

# what is the mean range size of native vs non-native in Europe
data %>% names()
data <- data %>% select(taxon_name, native_to_europe, area_km2, woodiness) %>% na.omit()
# unrelated, but interesting for result section
data %>% group_by(native_to_europe) %>% 
  summarise(mean(area_km2))

# ok back to topic:
# include non-natives that have a range size of 300.000 (woody) and 980.000 (non-woody)
data <- data %>% 
  filter(native_to_europe == FALSE) %>% 
  filter(
    (woodiness == "Woody" & area_km2 >= 269153.5) |
      (woodiness == "Non-Woody" & area_km2 >= 1096478)
  ) 
nrow(data) # thats a good number to work with

# which non-native species
non_natives <- unique(data$taxon_name)

# interaction data --------------------------------------------------------

# plant microherbivore
relations <- read_delim("Data/harmonized_relations.csv", delim = ";")

# microherbivore host breadth summarized across all sp.
hostbreadth <- read_delim("Data/microherbivore-hostbreadth.csv", delim = ";")

# join
rh <- left_join(
  relations %>% select(taxon_name, microherbivore_species, native_to_europe), 
    hostbreadth %>% select(microherbivore_species, n)
)

# now calculate for each plant species the mean host breadth
rh <- rh %>% 
  group_by(taxon_name) %>% 
  summarize(avg_hostbreadth = mean(n),
            native_to_europe = first(native_to_europe))

# ok now we have to reduce the dataset to the non-natives from above and the
# natives that we had in fig 1
dt_filled <- read.csv("Data/dt_filled.csv")

# left join to eliminate all taxa with only genus level and nas etc (check preparation script)
rh <- left_join(dt_filled, rh)

# and now those with the above range size
rh_selected <- rh %>%
  filter(
    native_to_europe == TRUE | taxon_name %in% non_natives
  ) %>% 
  filter(!is.na(woodiness))

# refine for plotting
rh_selected <- rh_selected %>%
  mutate(
    woodiness = recode(woodiness, "Non-Woody" = "Non-woody"),
    woodiness = factor(woodiness, levels = c("Non-woody", "Woody")),
    plant_origin = ifelse(native_to_europe, "Native", "Non-native"),
    plant_origin = factor(plant_origin, levels = c("Native", "Non-native"))
  )

# super interesting... non-natives interact with more generalist microherbivores
ggplot(rh_selected, aes(x = native_to_europe, y = log(avg_hostbreadth), col = woodiness))+
  geom_boxplot()


# check whether the non-natives in fact interact with more similar numbers of microherbivores
ggplot(rh_selected, aes(x = native_to_europe, y = log(n), col = woodiness))+
  geom_boxplot()




# model -------------------------------------------------------------------

mod <- lmerTest::lmer(log10(avg_hostbreadth) ~ plant_origin * woodiness +
                        (1|family), data = rh_selected)

summary(mod)
anova(mod)
plot(mod)

# model diagnostics look ok
sim_res <- simulateResiduals(fittedModel = mod)
plot(sim_res)


# visualize result
# calculate counts and build expression labels
n_counts <- rh_selected %>%
  group_by(plant_origin, woodiness) %>%
  summarise(
    total = n(),
    label = total,
    .groups = "drop"
  )

# also calculate errorbars
em <- emmeans(mod, ~ plant_origin * woodiness)
em_df <- as.data.frame(summary(em, type = "response"))



# visualize ---------------------------------------------------------------

ggplot(rh_selected,
       aes(x = plant_origin, 
           y = avg_hostbreadth,
           fill = plant_origin)) +
  facet_wrap(~woodiness, scales = "free_x") +
  geom_violinhalf(position = position_nudge(x = 0.2, y = 0), color = NA) +
  geom_point(pch = 20, aes(color = plant_origin),
             position = position_jitter(width = 0.1), alpha = 0.2) +
  scale_fill_manual(values = c("Native" = "#b5e1d2", "Non-native" = "#d1b4e0")) +
  scale_color_manual(values = c("Native" = "#b5e1d2", "Non-native" = "#d1b4e0")) +
  
  new_scale_fill() +
  new_scale_color() +
  geom_errorbar(
    data = em_df,
    aes(
      x = plant_origin,
      y = response,
      ymin = asymp.LCL,
      ymax = asymp.UCL,
      col = plant_origin
    ),
    inherit.aes = FALSE,
    width = 0.15,
    lwd = 1
  ) +
  geom_point(
    data = em_df,
    aes(
      x = plant_origin,
      y = response,
      fill = plant_origin
    ),
    shape = 21,
    size = 3,
    color = "black",
    inherit.aes = FALSE
  ) +
  geom_text(
    data = n_counts,
    aes(
      x = plant_origin,
      y = 1,  # or slightly below your data range
      label = label,
      color = plant_origin
    ),
    inherit.aes = FALSE,
    vjust = 1.8,
    size = 2.8,
    family = "roboto",
    fontface = "italic"
  ) +
  
  scale_fill_manual(values = c("Native" = "#26a97d", "Non-native" = "#792c9e")) +
  scale_color_manual(values = c("Native" = "#26a97d", "Non-native" = "#792c9e")) +
  
  
  scale_y_log10() +
  labs(
    x = "", 
    y = "Host breadth of microherbivore", 
    fill = "Plant origin", 
    color = "Plant origin"
  ) +
  theme_minimal(base_family = "roboto") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    plot.margin = margin(0, 0, 0, 0),  # remove plot margins
    axis.text.x = element_text(size = 10, vjust = -1),
    axis.ticks.y = element_line(),
    axis.title.y = element_text(size = 11),
    strip.text = element_text(size = 11, face = "italic")
  )

showtext_opts(dpi=600)
ggsave(filename = "Figures/host-breadth.png",
       bg = "white",
       height = 4,
       width = 7,
       dpi = 600)
showtext_opts(dpi=96)



# for SI ------------------------------------------------------------------

# show histogram of specializations/host breadth for native plants
d <- read.csv("Data/harmonized_relations.csv", sep = ";")

# we want microherbivores that exclusively interact with native plants
d <- d %>% 
  filter(!is.na(native_to_europe)) %>% 
  group_by(microherbivore_species) %>%
  summarize(n = n_distinct(taxon_name),
            native_nonnative = n_distinct(native_to_europe),
            which_origin = first(native_to_europe))

d %>% group_by(which_origin) %>% count(native_nonnative)

# now filter which_origin = T and native_nonnative = 1
d <- d %>% filter(which_origin == T & native_nonnative == 1) 

# how many/what fraction depends on only one native plant
d %>% filter(n == 1) %>% nrow() / d %>% nrow()

ggplot(d, aes(x = n)) +
  geom_histogram(fill = "#26a97d", color = "black", bins = 40, alpha = 0.8) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    x = "Microherbivore host breadth (how many native plant sp. they use)",
    y = "Number of microherbivor species"
  ) +
  theme_minimal(base_family = "roboto") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    plot.margin = margin(0, 0, 0, 0),  # remove plot margins
    axis.text.x = element_text(size = 10),
    axis.ticks.y = element_line(),
    axis.title.y = element_text(size = 11),
    strip.text = element_text(size = 11, face = "italic")
  )

showtext_opts(dpi=600)
ggsave(filename = "Figures/si-host-breadth.png",
       bg = "white",
       height = 3.59,
       width = 6.66,
       dpi = 600)
showtext_opts(dpi=96)


# also show that using these range size values indeed results in non-natives 
# having similar levels of trophic integration in terms of numbers of inter
# acting microherbivores
mod <- lmerTest::lmer(log10(n) ~ plant_origin * woodiness +
                        (1|family), data = rh_selected)

summary(mod)
anova(mod)

# visualize result
# calculate counts and build expression labels
n_counts <- rh_selected %>%
  group_by(plant_origin, woodiness) %>%
  summarise(
    total = n(),
    label = total,
    .groups = "drop"
  )

# also calculate errorbars
em <- emmeans(mod, ~ plant_origin * woodiness)
em_df <- as.data.frame(summary(em, type = "response"))

ggplot(rh_selected,
       aes(x = plant_origin, 
           y = n,
           fill = plant_origin)) +
  facet_wrap(~woodiness, scales = "free_x") +
  geom_violinhalf(position = position_nudge(x = 0.2, y = 0), color = NA) +
  geom_point(pch = 20, aes(color = plant_origin),
             position = position_jitter(width = 0.1), alpha = 0.2) +
  scale_fill_manual(values = c("Native" = "#b5e1d2", "Non-native" = "#d1b4e0")) +
  scale_color_manual(values = c("Native" = "#b5e1d2", "Non-native" = "#d1b4e0")) +
  
  new_scale_fill() +
  new_scale_color() +
  geom_errorbar(
    data = em_df,
    aes(
      x = plant_origin,
      y = response,
      ymin = asymp.LCL,
      ymax = asymp.UCL,
      col = plant_origin
    ),
    inherit.aes = FALSE,
    width = 0.15,
    lwd = 1
  ) +
  geom_point(
    data = em_df,
    aes(
      x = plant_origin,
      y = response,
      fill = plant_origin
    ),
    shape = 21,
    size = 3,
    color = "black",
    inherit.aes = FALSE
  ) +
  geom_text(
    data = n_counts,
    aes(
      x = plant_origin,
      y = 1,  # or slightly below your data range
      label = label,
      color = plant_origin
    ),
    inherit.aes = FALSE,
    vjust = 1.8,
    size = 2.8,
    family = "roboto",
    fontface = "italic"
  ) +
  
  scale_fill_manual(values = c("Native" = "#26a97d", "Non-native" = "#792c9e")) +
  scale_color_manual(values = c("Native" = "#26a97d", "Non-native" = "#792c9e")) +
  
  
  scale_y_log10() +
  labs(
    x = "", 
    y = "Microherbivore species per plant", 
    fill = "Plant origin", 
    color = "Plant origin"
  ) +
  theme_minimal(base_family = "roboto") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    plot.margin = margin(0, 0, 0, 0),  # remove plot margins
    axis.text.x = element_text(size = 10, vjust = -1),
    axis.ticks.y = element_line(),
    axis.title.y = element_text(size = 11),
    strip.text = element_text(size = 11, face = "italic")
  )

showtext_opts(dpi=600)
ggsave(filename = "Figures/trophically-integrated-non-natives.png",
       bg = "white",
       height = 4,
       width = 7,
       dpi = 600)
showtext_opts(dpi=96)

