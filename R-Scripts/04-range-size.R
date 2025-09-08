source("R-Scripts/00-preamble.R")

# load data
dt_filled <- read_csv("Data/dt_filled.csv")

# get gbif distribution in europe -----------------------------------------

# set up the species vector
species <- unique(dt_filled$taxon_name)


# initialize an empty data frame to store results
species_keys_df <- data.frame(species = character(), 
                              usageKey = integer(), 
                              stringsAsFactors = FALSE)


# loop over each species to find GBIF usage keys
for (i in 1:length(species)) {
  # retrieve the usageKey from GBIF
  taxon_result <- name_backbone(species[i], rank = "species")
  
  # extract the usageKey
  usage_key <- ifelse("usageKey" %in% names(taxon_result), taxon_result$usageKey, NA)
  
  # add species name and usageKey to the dataframe
  species_keys_df <- rbind(species_keys_df, data.frame(species = species[i], 
                                                       usageKey = usage_key))
  # print progress
  print(paste("Processed species", i, "of", length(species)))
}

# check if all species have keys
species_keys_df %>% filter(is.na(usageKey))

# manually add the usageKeys for the (nine) species, some of these are synonyms 
species_keys_df[species_keys_df$species == "Apocissus antarctica", "usageKey"] <- 7130688     #usageKey of Cissus antartica
species_keys_df[species_keys_df$species == "Fritillaria messanensis", "usageKey"] <- 2752224
species_keys_df[species_keys_df$species == "Genista etnensis", "usageKey"] <- 5347587     # usageKey of Genista aetnensis
species_keys_df[species_keys_df$species == "Gyrophyllum verticillatum", "usageKey"] <- 3133887     # usageKey of Coreopsis verticillata 
species_keys_df[species_keys_df$species == "Iris orientalis", "usageKey"] <- 2748312
species_keys_df[species_keys_df$species == "Janochloa antidotalis", "usageKey"] <- 2705141     # usageKey of Panicum antidotale
species_keys_df[species_keys_df$species == "Salix alba", "usageKey"] <- 5372513
species_keys_df[species_keys_df$species == "Scoparia dulcis", "usageKey"] <- 3171787
species_keys_df[species_keys_df$species == "Weniomeles bodinieri", "usageKey"] <- 11687463     # usageKey of Stranvaesia bodinieri

# write and read
write.csv(species_keys_df, "Data/species_keys_df.csv", row.names = FALSE)
species_keys_df <- read_csv("Data/species_keys_df.csv")


# europe demarcation
europe <- get_wgsrpd3_codes("Europe")
europe <- data.frame(LEVEL3_COD = europe)

# get shape for these bot countries
tdwg <- st_read("bot_countries/wgsrpd-master/level3/level3.shp")
europe_shape <- tdwg %>% 
  filter(LEVEL3_COD %in% europe$LEVEL3_COD) %>% 
  st_geometry() %>% 
  st_transform(8857) %>%
  st_make_valid()

# check
plot(europe_shape)

# Initialize an empty list to store results
results_list <- list()


# loop over each species
for (i in 1:nrow(species_keys_df)) {
  species_name <- species_keys_df$species[i]
  # fetch species polygons from GBIF
  gbif_proj <- mvt_fetch(taxonKey = species_keys_df$usageKey[i], 
                         bin = "square", 
                         squareSize = 8,
                         hasCoordinateIssue = FALSE,
                         basisOfRecord = c("HUMAN_OBSERVATION", 
                                           "MACHINE_OBSERVATION", 
                                           "OBSERVATION"))
  
  # check if the fetch result is empty or NULL
  if (is.null(gbif_proj) || nrow(gbif_proj) == 0) {
    print(
      paste("No GBIF data for species", species_keys_df$species[i],
            "- skipping to next.")
      )
    next  # skip to the next iteration of the loop
  }
  
  # project and make valid
  gbif_proj <- gbif_proj %>% 
    mutate(species = species_name) %>%
    st_transform(8857) %>%
    st_make_valid()
  
  # crop distribution to occurrences only inside Europe
  gbif_in_europe <- st_intersection(gbif_proj, europe_shape)
  
  # check if the intersection resulted in any occurrences
  if (nrow(gbif_in_europe) == 0) {
    print(
      paste("No GBIF data for species", 
            species_keys_df$species[i], 
            "in Europe - skipping to next.")
      )
    next  # skip to the next iteration of the loop
  }
  
  # calculate the number of tiles
  number_of_tiles <- nrow(gbif_in_europe)
  
  # calculate the area occupied by the species (assuming square grid, in the same projection as Europe)
  total_area <- sum(st_area(gbif_in_europe))
  
  results_list[[i]] <- list(species = species_name, 
                            tiles = number_of_tiles, 
                            area = total_area)
  
  # output the results for this species
  print(
    paste("Species:",
          species_name, "has", 
          number_of_tiles, "tiles in Europe and occupies an area of", 
          total_area, "square meters.")
    )
  
}

# bind list and write as csv
europe_species_ranges <- bind_rows(results_list)
write.csv(europe_species_ranges, "Data/europe_species_ranges.csv", row.names = F)



# harmonize ---------------------------------------------------------------

# read data
europe_species_ranges <- read.csv("Data/europe_species_ranges.csv")

# transform area from m2 in km2
europe_species_ranges <- europe_species_ranges %>%
  mutate(area_km2 = area / 1e6) %>%       
  select(-area) 

# wcvp harmonization
range_wcvp_match <- wcvp_match_names(europe_species_ranges, 
                                            name_col = "species",
                                            fuzzy = FALSE)

# give preference to accepted (and then synonyms) names, 
# if there are multiple rows for one and 
# the same species
wcvp_acc <- range_wcvp_match %>% 
  select(species, 
         wcvp_status, 
         wcvp_accepted_id)

wcvp_acc <- wcvp_acc %>% 
  arrange(desc(wcvp_status %in% c("Accepted", "Synonym")), 
          desc(wcvp_status == "Accepted")) %>%
  distinct(species, .keep_all = TRUE)

names <- wcvp_names %>% 
  select(wcvp_accepted_id = plant_name_id, 
         taxon_name)

wcvp_acc_range <- left_join(wcvp_acc, names) 

europe_species_ranges <- left_join(europe_species_ranges, wcvp_acc_range)

europe_species_ranges %>% 
  count(taxon_name) %>% 
  arrange(desc(n))



# join all datasets so far ------------------------------------------------

dr <- left_join(dt_filled, europe_species_ranges, by = c("taxon_name" = "species"))
dr <- left_join(dr, min_first_record_alien_t)

# save data
write.csv(dr, "data/dr.csv", row.names = FALSE)
dr <- read_csv("Data/dr.csv")


View(dr)
# inspect data ------------------------------------------------------------

# some first viz
ggplot(dr %>% 
         filter(native_to_europe == FALSE) %>% 
         filter(!is.na(Woodiness)), aes(x = area_km2, 
                                        y = n, 
                                        col = Woodiness)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  scale_x_log10() +
  scale_y_log10()


# connection between min first record and range size
ggplot(dr %>% 
         filter(native_to_europe == FALSE) %>% 
         filter(min_fr >= 1500) %>% 
         filter(!is.na(Woodiness)), aes(x = min_fr, 
                                        y = area_km2, 
                                        col = Woodiness)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  scale_y_log10()




# this would make an interesting SI plot, make it pub-ready
ggplot(dr %>% 
         mutate(Woodiness = recode(Woodiness, "Non-Woody" = "Non-woody")) %>% 
         filter(native_to_europe == FALSE) %>% 
         filter(min_fr >= 1500) %>% 
         filter(!is.na(Woodiness)), aes(x = min_fr, 
                                        y = area_km2, 
                                        col = Woodiness)) +
  
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
  geom_smooth(method = "lm", 
              size = 1.2,        
              alpha = 0.2  ) +
  
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
    y = "Range size in Europe [km²]",
    x = "Introduction date"
  ) +
  theme_minimal(base_family = "roboto") +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 11, vjust = -1),
    axis.ticks.y = element_line(),
    axis.ticks.x = element_line(),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    strip.text = element_text(size = 11, face = "italic")
  )

# model
mod <- lmerTest::lmer(log10(area_km2) ~  
                            min_fr * woodiness + 
                            (1|family),
                          dr %>% 
  mutate(Woodiness = recode(Woodiness, "Non-Woody" = "Non-woody")) %>% 
  filter(native_to_europe == FALSE) %>% 
  filter(min_fr >= 1500) %>% 
  filter(!is.na(Woodiness))
)
summary(emtrends(mod, specs = "woodiness", var = "min_fr"))

# save
showtext_opts(dpi=600)
ggsave(filename = "Figures/fig-si-range-date.png",
       bg = "white",
       height = 3.59,
       width = 6.66,
       dpi = 600)
showtext_opts(dpi=96)


  
