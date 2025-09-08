source("R-Scripts/00-preamble.R")

# statistical analysis ----------------------------------------------------

# first prep data
dt_filled <- read.csv("Data/dt_filled.csv")
#View(dt_filled)

dt_filled <- dt_filled %>%
  filter(!is.na(woodiness)) %>%
  mutate(
    woodiness = recode(woodiness, "Non-Woody" = "Non-woody"),
    woodiness = factor(woodiness, levels = c("Non-woody", "Woody")),
    plant_origin = ifelse(native_to_europe, "Native", "Non-native"),
    plant_origin = factor(plant_origin, levels = c("Native", "Non-native"))
  )

# model
# with log_transformed response
conflicts_prefer(lme4::lmer)
mod_r <- robustlmm::rlmer(log10(n) ~ plant_origin * woodiness +
              (1|family), data = dt_filled)
tab_model(mod_r)
tab_model(mod_r, file = "model_output.doc")

plot(mod_r)

mod <- lmerTest::lmer(log10(n) ~ plant_origin * woodiness +
                          (1|family), data = dt_filled)

summary(mod)
anova(mod)
plot(mod)

# model diagnostics look ok
sim_res <- simulateResiduals(fittedModel = mod)
plot(sim_res)


# visualize result
# calculate counts and build expression labels
n_counts <- dt_filled %>%
  group_by(plant_origin, woodiness) %>%
  summarise(
    total = n(),
    label = total,
    .groups = "drop"
  )

# also calculate errorbars
em <- emmeans(mod, ~ plant_origin * woodiness)
em
# back-transform to original scale (i.e. undo log10)
em_df <- as.data.frame(summary(em, type = "response"))

# plot
ggplot(dt_filled,
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
ggsave(filename = "Figures/plant-origin.png",
       bg = "white",
       height = 4,
       width = 7,
       dpi = 600)
showtext_opts(dpi=96)


# for result writing ------------------------------------------------------
summary(mod)
emmeans(mod, ~ plant_origin * woodiness) %>% 
  as.data.frame() %>% 
  mutate(back_transformed = 10^emmean)


