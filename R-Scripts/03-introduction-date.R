source("R-Scripts/00-preamble.R")

# temporal factors in microherbivores adaption --------------------------------

# load data, access: https://zenodo.org/records/10039630
GlobalAlienSpeciesFirstRecordDatabase_v3_1_freedata <- read_excel(
  "Data/GlobalAlienSpeciesFirstRecordDatabase_v3.1_freedata.xlsx", 
  sheet = "FirstRecords"
)

GlobalAlienSpeciesFirstRecordDatabase <- GlobalAlienSpeciesFirstRecordDatabase_v3_1_freedata
rm(GlobalAlienSpeciesFirstRecordDatabase_v3_1_freedata)
unique(GlobalAlienSpeciesFirstRecordDatabase$LifeForm)

# reduce the table to vascular plants
PlantAlienSpeciesFR <- GlobalAlienSpeciesFirstRecordDatabase %>%
  filter(LifeForm == "Vascular plants")

#inspect data
str(PlantAlienSpeciesFR)
summary(PlantAlienSpeciesFR)

unique(PlantAlienSpeciesFR$Region)    # view all regions in the alien species first record table

# looked up all european regions and created a value by hand for all european regions
european_regions3 <- c("United Kingdom", 
                       "Ukraine", 
                       "Switzerland", 
                       "Sweden", 
                       "Svalbard and Jan Mayen", 
                       "Spain", 
                       "Slovenia", 
                       "Slovakia", 
                       "Sicily", 
                       "Saint Paul (France)", 
                       "Russia", 
                       "Romania", 
                       "Portugal", 
                       "Poland", 
                       "Norway", 
                       "Netherlands", 
                       "Malta", 
                       "Luxembourg", 
                       "Lithuania", 
                       "Liechtenstein", 
                       "Latvia", 
                       "Italy", 
                       "Ireland", 
                       "Iceland", 
                       "Hungary", 
                       "Greece", 
                       "Germany", 
                       "France", 
                       "Finland", 
                       "Faroe Islands", 
                       "Estonia", 
                       "Denmark", 
                       "Czech Republic", 
                       "Croatia", 
                       "Corse", 
                       "Bulgaria", 
                       "Bosnia and Herzegovina", 
                       "Belgium", 
                       "Balearic Island", 
                       "Austria", 
                       "Andorra", 
                       "Albania")

# see if there is data for every European region
PlantAlienSpeciesFR %>% 
  select(Region) %>% 
  distinct %>% 
  mutate(present = 1) %>% 
  right_join(data.frame(Region = european_regions3)) %>% 
  View()


# filter by European regions
alien_species_europe <- PlantAlienSpeciesFR %>%
  filter(Region %in% european_regions3)
unique(alien_species_europe$Region)

# identifying the first record in Europe for every alien plant species
min_first_record_alien <- alien_species_europe %>%
  group_by(TaxonName) %>%                         
  summarise(min_first_record = min(FirstRecord, na.rm = TRUE))
View(min_first_record_alien)

#taxonomic harmonization
first_record_wcvp_match <- wcvp_match_names(min_first_record_alien, 
                                            name_col = "TaxonName", 
                                            fuzzy = TRUE)

# give preference to accepted (and then synonyms) names, 
# if there are multiple rows for one and 
# the same species
wcvp_acc <- first_record_wcvp_match %>% 
  select(TaxonName,
         wcvp_status, 
         wcvp_accepted_id)
                                                      
wcvp_acc <- wcvp_acc %>% 
  arrange(desc(wcvp_status %in% c("Accepted", "Synonym")), 
          desc(wcvp_status == "Accepted")) %>%
  distinct(TaxonName, .keep_all = TRUE)

# join wcvp names with harmonized names from first record data
names <- wcvp_names %>% 
  select(wcvp_accepted_id = plant_name_id, 
         taxon_name)

wcvp_acc_fr <- left_join(wcvp_acc, names) 

min_first_record_alien <- left_join(min_first_record_alien, 
                                    wcvp_acc_fr)

# check for duplications
min_first_record_alien %>% 
  count(taxon_name) %>% 
  arrange(desc(n))

# take min introduction name for duplications
min_first_record_alien_t <- min_first_record_alien %>% 
  group_by(taxon_name) %>% 
  summarise(min_fr = min(min_first_record )) %>% 
  filter(!is.na(taxon_name))

min_first_record_alien_t %>% 
  count(taxon_name) %>% 
  arrange(desc(n))

# save min first record data
write.csv(min_first_record_alien_t, "Data/min_first_record_alien_t.csv", row.names = FALSE)
min_first_record_alien_t <- read_delim("Data/min_first_record_alien_t.csv", delim = ";")

# inspect -----------------------------------------------------------------

# load microherbivore data (with plant origin data, woodiness etc.)
dt_filled <- read_csv("Data/dt_filled.csv")

# join interaction data with first records data
dx <- left_join(dt_filled, min_first_record_alien_t)

# save merged data table
write.csv(dx, "Data/dx.csv", row.names = FALSE)

# check the spp. with an earlier than 1500 BC introduction date
dx <- read_csv("Data/dx.csv")
filtered_dx <- dx %>% 
  filter(min_fr >= 1500) %>% 
  filter(native_to_europe == F)

after_dx <- dx %>% 
  filter(min_fr <= 1500) %>% 
  filter(native_to_europe == F)

after_dx$taxon_name

sum(!is.na(dx$min_fr) & dx$native_to_europe == FALSE)

# some first viz
ggplot(dx %>% 
         filter(min_fr > 1500) %>% 
         filter(native_to_europe == FALSE), 
       aes(x = min_fr, 
           y = log(n), 
           col = Woodiness)) +
  geom_point() + 
  geom_smooth(method = "lm")
