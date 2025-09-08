source("R-Scripts/00-preamble.R")

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


# check whether the non-natives in fact interact with similar numbers of microherbivores
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
    aes(x = plant_origin, y = Inf, label = label, color = plant_origin),
    inherit.aes = FALSE,
    vjust = 1.05,                          
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
    axis.text.x = element_blank(),
    axis.ticks.y = element_line(),
    axis.title.y = element_text(size = 11),
    strip.text = element_text(size = 11, face = "italic")
  ) -> fig3a

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


# more on generalization and specialization -------------------------------

# reviewer suggested to check what the phylogenetic make up of the hosts is
# let's use this classification from Bassi & Staude PNAS 2024 
# (based on Cane and Sipes 2007)
relations <- read_delim("Data/harmonized_relations.csv", delim = ";")

d <- relations %>% 
  mutate(plant_genus = word(taxon_name, 1)) %>% 
  # add specialization level following 
  group_by(microherbivore_species) %>%
  mutate(
    n_species = n_distinct(taxon_name),
    n_genus = n_distinct(plant_genus),
    n_family = n_distinct(plant_family)
  ) %>% # adding the number of host plant species, genera and families
  ungroup() %>%
  mutate(specialization = if_else(
    n_species == 1,
    "mono",
    if_else(
      n_genus <= 4 & n_family == 1,
      "oligo",
      if_else(n_family <= 3, "meso", "poly")
    )
  ))


d %>% 
  select(microherbivore_species, specialization) %>% 
  distinct %>% 
  count(specialization)

# now calculate proportion of each specialization level per plant species
spec_lvls <- c("mono","oligo","meso","poly")

comp_plant <- d %>%
  mutate(specialization = factor(specialization, levels = spec_lvls)) %>%
  count(taxon_name, specialization, name = "n") %>%
  complete(taxon_name,  specialization, fill = list(n = 0)) %>%
  group_by(taxon_name) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()


# ok now we have to reduce the dataset to the non-natives from above and the
# natives that we had in fig 1
dt_filled <- read.csv("Data/dt_filled.csv")

# and now those with the above range size
dt_filled <- dt_filled %>%
  filter(
    native_to_europe == TRUE | 
      taxon_name %in% non_natives # "non_natives" object comes from above
  ) %>% 
  filter(!is.na(woodiness)) %>%
  mutate(
    woodiness = recode(woodiness, "Non-Woody" = "Non-woody"),
    woodiness = factor(woodiness, levels = c("Non-woody", "Woody")),
    plant_origin = ifelse(native_to_europe, "Native", "Non-native"),
    plant_origin = factor(plant_origin, levels = c("Native", "Non-native"))
  )

# join to relevant species, select relevant cols
comp_plant_select <- left_join(dt_filled %>% ungroup %>% 
                          select(taxon_name, woodiness, plant_origin) %>% 
                          distinct,
                        comp_plant  %>% 
                          ungroup %>% 
                          select(taxon_name, specialization, n, prop)
)


# mean composition across plants (with simple SE)
comp_mean <- comp_plant_select %>%
  group_by(plant_origin, woodiness, specialization) %>%
  summarise(
    mean_prop = mean(prop, na.rm = TRUE),
    se_prop   = sd(prop, na.rm = TRUE) / sqrt(dplyr::n()),
    .groups   = "drop"
  )

# factor reordering
comp_mean <- comp_mean %>%
  mutate(
    plant_origin   = factor(plant_origin, levels = c("Native","Non-native")),
    woodiness      = factor(woodiness, levels = c("Non-woody","Woody")),
    specialization = factor(specialization, levels = c("mono","oligo","meso","poly"))
  )

# n per facet x origin (to annotate sample size)
n_counts <- comp_plant %>%
  distinct(taxon_name, plant_origin, woodiness) %>%
  count(plant_origin, woodiness, name = "n_plants") %>%
  mutate(label = paste0("n=", n_plants))


# visualize
ggplot(comp_mean, aes(x = plant_origin, y = mean_prop, fill = specialization)) +
  facet_wrap(~ woodiness, scales = "free_x") +
  geom_col(width = 0.7, alpha = 0.6) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1), 
                     expand = expansion(mult = c(0, 0.02))) +
  # specialization palette (distinct from origin colors)
  scale_fill_manual(
    values = c(
      "mono" = "#b5e1d2",
      "oligo" = "#7cc7b2",
      "meso" = "#5aa7c7",
      "poly" = "#792c9e"  # echo your purple for poly if you like
    ),
    breaks = c("mono","oligo","meso","poly"),
    labels = c("Monophagous","Oligophagous","Mesophagous","Polyphagous")
  ) +
  labs(
    x = "",
    y = "Mean share across plants",
    fill = "Specialization"
  ) +
  theme_minimal(base_family = "roboto") +
  theme(
    panel.grid = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    axis.text.x = element_text(size = 10, vjust = -1),
    axis.ticks.y = element_line(),
    axis.title.y = element_text(size = 11),
    strip.text = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  ) -> fig3b



# multipanel for main text
(fig3a + ggtitle("a")) /
  (fig3b + ggtitle("b")) +
  plot_layout(heights = c(1.3, 1)) + 
  plot_annotation(tag_levels = NULL) &
  theme(
    plot.title = element_text(size = 12, face = "bold", family = "roboto"),
  )

showtext_opts(dpi=600)
ggsave(filename = "Figures/host-breadth.png",
       bg = "white",
       height = 8,
       width = 8,
       dpi = 600)
showtext_opts(dpi=96)



# finally it would be super interesting to understand whether
# specialization then increases with introduction time somehow...
d <- read_csv("Data/mean_centroids_filled.csv")

dfw <- d %>%
  filter(taxon_name %in% non_natives,
         native_to_europe == FALSE, 
         !is.na(min_fr), 
         min_fr >= 1500) %>%
  select(taxon_name, min_fr, woodiness) %>% 
  left_join(comp_plant %>%  rename(n_spec = n)) %>% 
  mutate(
    years_since_intro = 2025 - min_fr         
  ) %>%
  group_by(taxon_name) %>%
  mutate(total = sum(n_spec)) %>%
  ungroup()

dfw %>% select(taxon_name) %>% distinct %>% nrow()

cuts <- dfw %>% 
  summarise(cuts = quantile(years_since_intro, 
                            probs = seq(0, 1, 0.1), na.rm = TRUE)) %>% pull(cuts)

binned <- dfw %>%
  mutate(bin = cut(years_since_intro, breaks = unique(cuts), include.lowest = TRUE)) %>%
  group_by(woodiness, specialization, bin) %>%
  summarise(
    n_tot = sum(total),
    prop_mean = weighted.mean(prop, w = total),
    .groups = "drop"
  ) 

# for plotting
spec_lvls <- c("mono","oligo","meso","poly")

binned <- binned %>%
  mutate(
    woodiness = recode(woodiness, "Non-Woody" = "Non-woody")
  ) %>% 
  mutate(
    specialization = factor(specialization, levels = spec_lvls),
    woodiness      = factor(woodiness, levels = c("Non-woody","Woody"))
  )

spec_cols <- c(
  mono  = "#b5e1d2",
  oligo = "#7cc7b2",
  meso  = "#5aa7c7",
  poly  = "#792c9e"   # echoes your purple for a “more generalist” highlight
)

ggplot(binned, aes(x = bin, y = prop_mean, fill = specialization)) +
  facet_grid(woodiness ~ specialization, scales = "free_x") +
  geom_col(width = 0.9, alpha = 0.9, color = NA) +
  scale_fill_manual(values = spec_cols) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(
    x = "Years since introduction (binned)",
    y = "Weighted mean proportion"   # or: "Weighted mean per-plant proportion"
  ) +
  theme_minimal(base_family = "roboto") +
  theme(
    panel.grid       = element_blank(),
    legend.position  = "none",
    plot.margin      = margin(0, 0, 0, 0),
    axis.text.x      = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
    axis.ticks.y     = element_line(),
    axis.title.y     = element_text(size = 11),
    strip.text       = element_text(size = 11, face = "italic")
  )

showtext_opts(dpi=600)
ggsave(filename = "Figures/specialization-residencetime.png",
       bg = "white",
       height = 7,
       width = 9,
       dpi = 600)
showtext_opts(dpi=96)
