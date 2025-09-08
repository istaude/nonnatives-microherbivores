source("R-Scripts/00-preamble.R")

# load data --------------------------------------------------------------------
dr <- read_csv("Data/dr.csv")
kew_dis_europe_native <- read_csv("Data/kew_dis_europe_native.csv")

# do non-native plants with close relatives (same genus) in Europe 
# interact with more microherbivore species compared to those without close relatives? 
# this could e.g., be due to pre-adapted herbivores that can exploit similar traits 
# in the introduced plants
europe_natives <- kew_dis_europe_native %>% 
  filter(native_to_europe == T) %>% 
  left_join(
  wcvp_names %>% select(plant_name_id, 
                        taxon_name, 
                        genus, 
                        family)
  ) %>% 
  select(genus, family) %>% 
  distinct


# how many genera and families
length(unique(europe_natives$genus))
length(unique(europe_natives$family))

# split microherbivore data in native and non native to europe
dr_non_native <- dr %>% 
  filter(native_to_europe == FALSE)
# save
write.csv(dr_non_native, "Data/dr_non_native.csv", row.names = FALSE) 

# create 3 groups: related via genus, via family and not related
genus_in_europe <- unique(europe_natives$genus)
family_in_europe <- unique(europe_natives$family)

related_genus <- dr_non_native %>% 
  filter(genus %in% genus_in_europe)

related_family <- dr_non_native %>% 
  filter(!(genus %in% genus_in_europe) & family %in% family_in_europe)

no_relations <- dr_non_native %>% 
  filter(!(genus %in% genus_in_europe) & !(family %in% family_in_europe))

nrow(related_genus)    # 2484
nrow(related_family)    # 911
nrow(no_relations)    # 140
# the most plants have relatives on genus level

# rename the three relationship status and bind them in a data table
related_genus$rel_status <- "Genus in Europe"
related_family$rel_status <- "Family in Europe, Genus not"
no_relations$rel_status <- "No Relations"

relatedness <- rbind(related_genus, related_family, no_relations)

relatedness<- left_join(relatedness, dr %>% select(taxon_name, n))
names(relatedness)

# save table
write.csv(relatedness, "Data/relatedness.csv", row.names = FALSE)



# inspect -----------------------------------------------------------------

relatedness <- read_csv("Data/relatedness.csv")
# reorder rel_status based on the desired order: "no_relations", "family", "genus"
relatedness$rel_status <- factor(relatedness$rel_status, 
                                 levels = c("No Relations", 
                                            "Family in Europe, Genus not", 
                                            "Genus in Europe"))

# exclude NAs in spatial distribution of non-native plants
relatedness %>% filter(is.na(area_km2))
summary(relatedness$area_km2)

# some first viz
# considering also woodiness, and spatial distribution (area)
ggplot(relatedness %>% filter(!is.na(Woodiness)),
       aes(x = rel_status,
           y = n/log(area_km2)) )+
  facet_wrap(~Woodiness)+
  geom_jitter(alpha=0.1)+
  geom_boxplot(alpha=0.3, outliers = F) +
  scale_y_log10() +
  coord_flip() 






