source("R-Scripts/00-preamble.R")

# the aim of this script is to test whether the effect of range size on
# microherbivore interaction richness is actually driven by traits
# use german flora because trait avaliability is good.


# load data ---------------------------------------------------------------

# this data set has been created by Miriam Wahl as part of her BSc thesis
# it integrates data from the German RL, Eichenberg et al (GCB), try, EIVE
data <- read.csv("Data/german-flora-traits-etc.csv", sep = ";")
#View(data)


# model -------------------------------------------------------------------
data$log10AOO <- log10(data$AOO)
data$log10n <- log10(data$number_herb)
data <- data %>% 
  filter(!is.na(log10AOO)) %>% 
  filter(!is.na(EIV.N)) %>% 
  filter(!is.na(Woodiness))

mod <- lmerTest::lmer(log10n ~ log10AOO * Woodiness + 
                      EIV.N * Woodiness + 
                      (1 | family), data = data)
summary(mod)
anova(mod)
tab_model(mod)
tab_model(mod, file = "model_output1.doc")


# data visualization ------------------------------------------------------

# predicted lines for each predictor variable
# log_area
range(data$log10AOO)
pr1_data <- 
  bind_rows(
    predict_response(mod, c("log10AOO [0.32:3.33]", "Woodiness")) %>%  as_tibble(),
    predict_response(mod, c("log10AOO [3.1:4.1]", "Woodiness")) %>%  as_tibble())
print(pr1_data)

(fig_s6a <- ggplot(data = data, aes(x = AOO, y = number_herb)) +
  
  # Points: subtle fill, clear border
  geom_point(
    pch = 21,
    aes(fill = Woodiness, col = Woodiness),
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
    "Non-Woody" = "#78c1b8"    # elegant purple
  )) +
  scale_fill_manual(values = c(
    "Woody" = "#C8C8C8",       # mid grey fill, better contrast
    "Non-Woody" = "#a3ffda"    # softer purple fill
  )) +
  
  # Labels and theme
  labs(
    x = "Range size in Germany\n[P(occurrence) summed across 5 x 5 km cells]",
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
)

# and now for EIV.N
range(data$EIV.N)
pr2_data <- predict_response(mod, c("EIV.N", "Woodiness")) %>%  as_tibble()
print(pr2_data)


(fig_s6b <- ggplot(data = data, aes(x = EIV.N, y = number_herb)) +
  geom_point(
    pch = 21,
    aes(fill = Woodiness, col = Woodiness),
    alpha = 0.4,      
    size = 2, show.legend = FALSE
  ) +
  scale_y_log10() +
  geom_smooth(
    data = pr2_data,
    aes(
      x = x,
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
  scale_color_manual(values = c(
    "Woody" = "#6E6E6E",       # sleek dark grey line
    "Non-Woody" = "#78c1b8"    # elegant purple
  )) +
  scale_fill_manual(values = c(
    "Woody" = "#C8C8C8",       # mid grey fill, better contrast
    "Non-Woody" = "#a3ffda"    # softer purple fill
  )) +

  labs(
    x = "Ecological indicator value for nutrients",
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
)

fig_s6a / fig_s6b



# now for leaf nitrogen ---------------------------------------------------
data <- read.csv("Data/german-flora-traits-etc.csv", sep = ";")


# model -------------------------------------------------------------------
data$log10AOO <- log10(data$AOO)
data$log10n <- log10(data$number_herb)
data <- data %>% 
  filter(!is.na(log10AOO)) %>% 
  filter(!is.na(mean_N_per_LDM)) %>% 
  filter(!is.na(Woodiness))

mod <- lmerTest::lmer(log10n ~ log10AOO * Woodiness + 
                        mean_N_per_LDM * Woodiness + 
                        (1 | family), data = data)
summary(mod)
anova(mod)
tab_model(mod)
tab_model(mod, file = "model_output2.doc")


# data visualization ------------------------------------------------------

# predicted lines for each predictor variable
# log_area
range(data$log10AOO)
pr1_data <- 
  bind_rows(
    predict_response(mod, c("log10AOO [0.32:3.33]", "Woodiness")) %>%  as_tibble(),
    predict_response(mod, c("log10AOO [3.1:4.1]", "Woodiness")) %>%  as_tibble())
print(pr1_data)

(fig_s6c <- ggplot(data = data, aes(x = AOO, y = number_herb)) +
    
    # Points: subtle fill, clear border
    geom_point(
      pch = 21,
      aes(fill = Woodiness, col = Woodiness),
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
      "Non-Woody" = "#78c1b8"    # elegant purple
    )) +
    scale_fill_manual(values = c(
      "Woody" = "#C8C8C8",       # mid grey fill, better contrast
      "Non-Woody" = "#a3ffda"    # softer purple fill
    )) +
    
    # Labels and theme
    labs(
      x = "Range size in Germany\n[P(occurrence) summed across 5 x 5 km cells]",
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
)

# and now for EIV.N
pr2_data <- predict_response(mod, c("mean_N_per_LDM", "Woodiness")) %>%  as_tibble()
print(pr2_data)


(fig_s6d <- ggplot(data = data, aes(x = mean_N_per_LDM, y = number_herb)) +
    geom_point(
      pch = 21,
      aes(fill = Woodiness, col = Woodiness),
      alpha = 0.4,      
      size = 2, show.legend = FALSE
    ) +
    scale_y_log10() +
    geom_smooth(
      data = pr2_data,
      aes(
        x = x,
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
    scale_color_manual(values = c(
      "Woody" = "#6E6E6E",       # sleek dark grey line
      "Non-Woody" = "#78c1b8"    # elegant purple
    )) +
    scale_fill_manual(values = c(
      "Woody" = "#C8C8C8",       # mid grey fill, better contrast
      "Non-Woody" = "#a3ffda"    # softer purple fill
    )) +
    
    labs(
      x = "N per leaf dry matter",
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
)

fig_s6c / fig_s6d


# create one plot ---------------------------------------------------------
(fig_s6a + ggtitle("a") + fig_s6c + ggtitle("b")) / (fig_s6b + fig_s6d) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = NULL) & 
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 12, face = "bold", family = "roboto"), 
    legend.title = element_blank()
  )


showtext_opts(dpi=600)
ggsave(filename = "Figures/german-flora.png",
       bg = "white",
       height = 8,
       width = 8,
       dpi = 600)
showtext_opts(dpi=96)
