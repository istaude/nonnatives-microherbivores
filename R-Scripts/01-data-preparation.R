source("R-Scripts/00-preamble.R")


# load microherbivore data ------------------------------------------------


# herbivory interaction data from https://bladmineerders.nl/
relations <- read_csv("Data/relations.csv", col_names = FALSE)
names(relations)

# add col names
relations <- relations %>% 
  rename(
    microherbivore_species = X1,
    microherbivore_family = X2,
    organ = X3,
    role = X4,
    phase = X5,
    plant_species = X6,
    plant_family = X7
  )

head(relations)
n_distinct(relations$microherbivore_species)
n_distinct(relations$plant_species)


# use powo data to find out whether the plant is only native outside Europe
# can be loaded from the rWCVPdata package:
data(wcvp_distributions, package = "rWCVPdata")
head(wcvp_distributions)    #Information about the global distribution of vascular plants species 

# get Europe's botanical country abbreviations
europe <- get_wgsrpd3_codes("Europe")

# classify species whether species are native to Europe
kew_dis_europe_native <- wcvp_distributions %>% 
  # assign status
  # for every species id, if it has at least one europe  occurrence
  # where it is "native", this species is classified as native
  # is a Europe native
  group_by(plant_name_id) %>% 
  summarize(native_to_europe = any(area_code_l3 %in% europe & introduced == 0))

# first check
kew_dis_europe_native %>% count(native_to_europe)

# save
write.csv(kew_dis_europe_native, "Data/kew_dis_europe_native.csv", row.names = FALSE)



# taxonomic harmonization -------------------------------------------------

# distinct list of plants species from the interaction data
relations_sp <- relations %>% 
  select(plant_species) %>% 
  distinct
nrow(relations_sp)

# taxonomic harmonization
# perform check with rwcvp, fuzzy name matching with the list of unique plant 
# species (interaction data) and the wcvp data
relations_wcvp_match <- wcvp_match_names(relations_sp, 
                              name_col = "plant_species",
                              fuzzy = TRUE)

# read and write
write_delim(relations_wcvp_match, 
            file = "Data/relations_wcvp_match.csv", 
            quote = "none", delim = ";")

relations_wcvp_match <- read_delim("Data/relations_wcvp_match.csv", delim = ";")

# give preference to accepted (and then synonyms) names, 
# if there are multiple rows for one and the same species
wcvp_acc <- relations_wcvp_match %>% 
  select(plant_species, 
         wcvp_status, 
         wcvp_accepted_id)

wcvp_acc <- wcvp_acc %>% 
  arrange(desc(wcvp_status %in% c("Accepted", "Synonym")), 
          desc(wcvp_status == "Accepted")) %>%
  distinct(plant_species, .keep_all = TRUE)

# plant names and id from rwcvp
names <- wcvp_names %>% 
  select(wcvp_accepted_id = plant_name_id, 
         ipni_id, 
         taxon_name)

# connect the checked and arranged list of plant names from the interaction data 
# with the wcvp names 
wcvp_acc_relations <- left_join(wcvp_acc, names) 

# check that it's the same number of species
wcvp_acc_relations %>% nrow()
wcvp_acc %>% nrow()


# merge microherbivore data with powo europe native -----------------------

# attach to each species in the herbivore data whether the plant is native
d <- left_join(wcvp_acc_relations, 
               kew_dis_europe_native, 
               by = c("wcvp_accepted_id" = "plant_name_id"))

# how many plant species are native/non-native to Europa
d %>% 
  group_by(native_to_europe) %>% 
  count 

# combine with the actual interaction data
d <- relations %>% 
  left_join(d %>% 
              select(plant_species, 
                     taxon_name, 
                     wcvp_accepted_id, 
                     native_to_europe)
            )

# some check, number of rows matches?
nrow(relations)
nrow(d)

# write for later
write_delim(d, 
            file = "Data/harmonized_relations.csv", 
            quote = "none", delim = ";")


# number of microherbivores per plant species -----------------------------

# summary of microherbivore species per plant species and their origin
d_summary <- d %>% group_by(taxon_name) %>% 
  summarize(n = n_distinct(microherbivore_species),
            native_to_europe = first(native_to_europe),
            wcvp_accepted_id = first(wcvp_accepted_id))
nrow(d_summary)

# summary of number of interaction on plant species per microherbivore species
dm_summary <- d %>% group_by(microherbivore_species) %>%
  summarize(n = n_distinct(taxon_name),
            organ = first(organ),
            role = first(role),
            phase = first(phase))
nrow(dm_summary)
View(dm_summary)

# write for later
write_delim(dm_summary, 
            file = "Data/microherbivore-hostbreadth.csv", 
            quote = "none", delim = ";")

# obtain plant genera and family ------------------------------------------
d_summary <- left_join(d_summary,
                       wcvp_names %>% 
                         select(family, 
                                genus, 
                                wcvp_accepted_id = plant_name_id)
                       )

# exclude genera from this analysis, exclude nas in native
d_summary_sp_only <- d_summary %>%
  # filter out rows where species name is a single word
  filter(str_count(taxon_name, "\\s+") > 0)
nrow(d_summary_sp_only)

# how many for method text...
d_summary_sp_only %>% nrow()
View(d_summary_sp_only)

d_summary_sp_only <- d_summary_sp_only %>% 
  # filter out rows where native_to_europe is na
  filter(!is.na(native_to_europe))
nrow(d_summary_sp_only)

d_summary_sp_only %>% count(native_to_europe)     

# save the data 
write.csv(d_summary_sp_only, "Data/d_summary_sp_only.csv", row.names = FALSE)
d_summary_sp_only <- read.csv("Data/d_summary_sp_only.csv")


# include growth form data ------------------------------------------------
# data from https://www.nature.com/articles/s41559-018-0787-9
# load and prepare
life_forms <- read_csv("Data/LifeForm_Trait_Database.csv") %>% 
  rename(Species = `Accepted Name`)
glimpse(life_forms)

# ensure all species names have only the first letter capitalized and the rest lowercase
capitalize_first <- function(s) {
  sub("^(\\w)(\\w*)", "\\U\\1\\L\\2", s, perl = TRUE)
}

life_forms <- life_forms %>% 
  mutate(Species = sapply(Species, capitalize_first))

# okay, so these classfications are a bit rudimentary but sufficient
life_forms %>% select(LifeCycle) %>% distinct
life_forms %>% select(Woodiness) %>% distinct

# taxonomic harmonization with wcvp
# match with wcvp names not fuzzy, that would take ages with so many sp
life_forms_wcvp_match <- wcvp_match_names(life_forms, 
                                          name_col = "Species",
                                          fuzzy = FALSE)
# that's really good
# again give preference to accepted (and then synonyms) names, 
# if there are multiple rows for one and the same species
wcvp_acc <- life_forms_wcvp_match %>% select(Species, 
                                             wcvp_status, 
                                             wcvp_accepted_id)
wcvp_acc <- wcvp_acc %>% 
  arrange(desc(wcvp_status %in% c("Accepted", "Synonym")), 
          desc(wcvp_status == "Accepted")) %>%
  distinct(Species, .keep_all = TRUE)

names <- wcvp_names %>% 
  select(wcvp_accepted_id = plant_name_id, 
         ipni_id, 
         taxon_name)

wcvp_acc_life_forms <- left_join(wcvp_acc, names) 

# check that it's the same number of species
wcvp_acc_life_forms %>% nrow()

# connect with life form data
life_forms_harmonized <-left_join(life_forms, wcvp_acc_life_forms)
nrow(life_forms_harmonized)

# now there may be several species that lead to an accepted species, e.g.,
# Bassia scoparia
# in such cases simply pick the accepted species
life_forms_harmonized_reduced <- life_forms_harmonized %>% 
  group_by(taxon_name) %>% 
  mutate(status_priority = ifelse(wcvp_status == "Accepted", 1, 2)) %>%
  slice_min(status_priority) %>% 
  ungroup

# check if there are some taxon_name that still occur more often than 1
life_forms_harmonized_reduced %>% count(taxon_name) %>% arrange(desc(n))
life_forms_harmonized_reduced %>% View()

# ok some of that comes due to the synonyms in life-form, now only select
# the "correct" columns
life_forms_harmonized_reduced <- life_forms_harmonized_reduced %>% 
  select(taxon_name, LifeCycle, Woodiness) %>% 
  distinct

# now redundancy comes from genuine differences in the lifecycle and woodiness
# classification, we excluded these cases because we really don't know
# whats correct
life_forms_harmonized_reduced %>% count(taxon_name) %>% arrange(desc(n)) %>% 
  filter(n > 1) %>% nrow()

life_forms_harmonized_reduced <- 
  anti_join(life_forms_harmonized_reduced, 
            life_forms_harmonized_reduced %>% count(taxon_name) %>% arrange(desc(n)) %>% 
              filter(n > 1))
# check again
life_forms_harmonized_reduced %>% count(taxon_name) %>% arrange(desc(n))

# join with microherbivore data
dt <- left_join(d_summary_sp_only,life_forms_harmonized_reduced)

# how many NAs in Woodiness
1- 
dt %>% filter(is.na(Woodiness)) %>% nrow /
dt %>% nrow

# do some form of gap filling
# find out what is the most frequent type per genus, gap fill for missing
# function to find the most frequent value
most_frequent <- function(x) {
  x <- na.omit(x)
  if (length(x) == 0) return(NA)
  return(names(sort(table(x), decreasing = TRUE)[1]))
}

# find the most frequent woodiness and lifecycle per genus
genus_summary <- dt %>%
  group_by(genus) %>%
  summarize(
    Woodiness_mode = most_frequent(Woodiness),
    LifeCycle_mode = most_frequent(LifeCycle)
  )

# join the most frequent values back to the original data
dt_filled <- dt %>%
  left_join(genus_summary, by = "genus") %>%
  mutate(
    woodiness = ifelse(is.na(Woodiness), Woodiness_mode, Woodiness),
    lifecycle = ifelse(is.na(LifeCycle), LifeCycle_mode, LifeCycle)
  ) %>%
  select(-Woodiness_mode, -LifeCycle_mode)

# how many NAs in Woodiness
dt_filled %>% filter(is.na(woodiness)) %>% nrow
dt_filled %>% filter(!is.na(woodiness)) %>% nrow

# save the filled data
write.csv(dt_filled, "Data/dt_filled.csv", row.names = FALSE)





# si figures --------------------------------------------------------------


# viz of europe definition for si
# get a map out of shapefiles from bot countries 
# WGS84: Level 3 regions
tdwg <- st_read("bot_countries/wgsrpd-master/level3/level3.shp")

# bot countries
europe_codes <- get_wgsrpd3_codes("Europe") 
europe_df <- data.frame(LEVEL3_COD = europe_codes)

# correcting
europe_shape <- tdwg %>%
  filter(LEVEL3_COD %in% europe_df$LEVEL3_COD) %>%
  st_transform(8857) %>%
  st_make_valid()

# plot with filtered data
ggplot(data = europe_shape) +
  geom_sf(aes(fill = LEVEL3_NAM), color = "black", size = 0.2) +
  scale_fill_viridis_d(name = "Botanical countries", 
                       guide = guide_legend(ncol = 2)) +
  theme_minimal(base_family = "roboto", base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.5, "cm"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.caption = element_text(size = 9, hjust = 0.5)
  )

showtext_opts(dpi=600)
ggsave(dpi = 600,
       bg = "white",
       filename = "Figures/europe.png",
       height = 4.91,
       width = 8.15)
showtext_opts(dpi=96)




# histogram of interactions of plants with microherbivores
ggplot(d_summary_sp_only, aes(x = n)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 40, alpha = 0.8) +
  scale_x_log10(breaks = c(1, 3, 10, 30, 100, 300, 1000)) +
  labs(
    title = "Interactions between Plant and Microherbivore Species",
    x = "Number of Interactions (log10)",
    y = "Number of Plant Species"
  ) +
  theme_classic(base_size = 14)

ggsave(filename = "Figures/Hist_plants.png",
       height = 7,
       width = 8)

# histogram of interactions of microherbivores with plants
ggplot(dm_summary, aes(x = n)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 40, alpha = 0.8) +
  scale_x_log10(breaks = c(1, 3, 10, 30, 100, 300, 1000)) +
  labs(
    title = "Interactions between Microherbivore and Plant Species",
    x = "Number of Interactions (log10)",
    y = "Number of Microherbivor Species"
  ) +
  theme_classic(base_size = 14)

ggsave(filename = "Figures/Hist_microherbivores.png",
       height = 7,
       width = 8)

d_summary_sp_only <- read.csv("Data/d_summary_sp_only.csv")




# inspect the mherbivore data a bit more ----------------------------------

# what are the top plant species in terms of interactions with microherbivores
d_summary_sp_only %>% arrange(desc(n)) %>% head

# what are the top microherbivore species in terms of interaction with plant species
dm_summary %>% arrange(desc(n)) %>% head

# how many microherbivore species in total?
d_summary_sp_only %>% 
  select(taxon_name) %>% 
  left_join(d) %>% 
  select(microherbivore_species) %>% 
  distinct %>% 
  nrow
# vs. with genera data
d %>% 
  select(microherbivore_species) %>% 
  distinct %>% 
  nrow

# gain insights into the role of the herbivores
d_summary_sp_only %>% 
  select(taxon_name) %>% 
  left_join(d) %>% 
  select(microherbivore_species, 
         role, 
         taxon_name) %>% 
  distinct %>% 
  group_by(role) %>% 
  count() %>% 
  arrange(desc(n)) 

# most are vagrant: 
# living freely on the leaves; phrase used in particular for gall mites
# next are borers and galls

# it seems that some microherbivores can also have several roles
# as this doesn't add up to the number of species in the data
d_summary_sp_only %>% 
  select(taxon_name) %>% 
  left_join(d) %>% 
  select(microherbivore_species, 
         role, 
         taxon_name) %>% 
  distinct %>% 
  group_by(microherbivore_species, 
           role) %>% 
  count() %>% 
  arrange(desc(n))

# so some species are in there many times because they eat on many different
# plant species, makes sense
d_summary_sp_only %>% 
  select(taxon_name) %>% 
  left_join(d) %>% 
  select(microherbivore_species, 
         role) %>% 
  distinct %>% 
  group_by(microherbivore_species, 
           role) %>% 
  count() %>% 
  arrange(desc(n))

# and each one has just one function
d_summary_sp_only %>% 
  select(taxon_name) %>% 
  left_join(d) %>% 
  select(microherbivore_species, 
         role) %>% 
  distinct %>% 
  group_by(role) %>% 
  count() %>% 
  arrange(desc(n))


# which plant organs are targeted most?
d_summary_sp_only %>% 
  select(taxon_name) %>% 
  left_join(d) %>% 
  select(microherbivore_species, organ) %>% 
  distinct %>% 
  group_by(organ) %>% 
  count() %>% arrange(desc(n))
# most use leaves, and then stems, flower, roots, unknown, fruit,
# leaf bud, root collar, dead wood (very few, interesting)


# which roles do the microherbivores have in the data
roles <- read_csv("Data/codes.csv", col_names = FALSE) %>% 
  filter(X1 == "role") %>% 
  rename(role = X2) %>% 
  select(role, X4)

relations <- read_csv("Data/relations.csv", col_names = FALSE)
relations <- relations %>% 
  rename(
    microherbivore_species = X1,
    microherbivore_family = X2,
    organ = X3,
    role = X4,
    phase = X5,
    plant_species = X6,
    plant_family = X7
  )

roles <- left_join(relations, roles) %>% 
  select(microherbivore_species, role = X4) %>% distinct %>% 
  count(role)

# explanatory labels
role_labels <- c(
  "borer"              = "Borers (larvae tunneling in stems, seeds, or wood)",
  "cancer"             = "Cankers (woody tissue deformities; fungi or bacteria)",
  "down"               = "Downy mildews (oomycetes)",
  "film"               = "Surface fungal films",
  "gall"               = "Galls (tissue swellings; mostly insects or mites)",
  "hidden"             = "Concealed feeders or infections (inside tissues; signs unclear)",
  "inquiline"          = "Inquilines (living in galls or mines made by others)",
  "leaf spot"          = "Leaf spots (necrotic lesions; fungi or bacteria)",
  "macro fungus"       = "Macro fungi",
  "miner"              = "Leaf miners (larvae feeding within leaves)",
  "miner > borer"      = "Miner to borer (starts as mine, then bores)",
  "oviposition scar"   = "Oviposition scars (egg laying marks)",
  "predator, parasite" = "Predators or hyperparasites (within galls or mines)",
  "pustule"            = "Pustules (fungal eruptions; rusts or smuts)",
  "scale"              = "Scale insects (sap feeders)",
  "stripe"             = "Stripe diseases (linear lesions; mostly fungal)",
  "unknown"            = "Unclassified plant damage",
  "vagrant"            = "Vagrants (eriophyid mites deforming tissues)",
  "witches' broom"     = "Witches' broom (dense shoot proliferation)",
  "NA"                 = "Unspecified"
)

roles_plot <- roles %>%
  filter(!is.na(role)) %>%
  mutate(
    role_exp = recode(role, !!!role_labels)
  ) %>%
  arrange(n) %>%
  mutate(role_exp = factor(role_exp, levels = role_exp))


col_point <- "#26a97d"   
col_seg   <- "grey70"    
col_line  <- "black"    

# constant offset in y-units (before coord_flip is applied)
offset <- 0.04 * diff(range(roles_plot$n, na.rm = TRUE))

ggplot(roles_plot, aes(x = role_exp, y = n)) +
  geom_segment(aes(xend = role_exp, y = 0, yend = n),
               linewidth = 0.8, color = col_seg) +
  geom_point(shape = 21, size = 4, stroke = 0.6,
             fill = col_point, color = col_line) +
  geom_text(
    aes(label = scales::comma(n)),
    hjust = 0,                 
    nudge_y = offset,          
    vjust = 0.5,
    size = 2.8,
    family = "roboto",
    fontface = "italic"
  ) +
  expand_limits(y = max(roles_plot$n, na.rm = TRUE) + 4*offset) +
  coord_flip(clip = "off") +
  labs(
    x = NULL,
    y = "Number of microherbivore species",
    title = NULL,
    subtitle = NULL
  ) +
  theme_minimal(base_family = "roboto") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    plot.margin = margin(0, 10, 0, 0),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 11)
  ) +
  expand_limits(y = max(roles_plot$n, na.rm = TRUE) * 1.08)

# microherbivore roles figure
showtext_opts(dpi=600)
ggsave(filename = "Figures/microherbivore-roles.png",
       bg = "white",
       height = 4,
       width = 7,
       dpi = 600)
showtext_opts(dpi=96)

