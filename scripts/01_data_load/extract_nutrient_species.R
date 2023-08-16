pacman::p_load(tidyverse, mermaidr, install=FALSE)

## Script to match all WCS reef species to nutrient predictions

## load species list
all<-mermaid_get_reference(reference = c("fishspecies", 'fishgenera', 'fishfamilies'))

sp<-all$fishspecies %>% mutate(species = display)

## load micronutrient predictions
nut<-read.csv('data/Species_Nutrient_Predictions.csv', header=TRUE) 
tax<-rfishbase::load_taxa() %>%  mutate(species = Species) %>% as.data.frame()
nut<-left_join(nut, tax, by = 'species')

# # ## match - how many species?
length(sp$species[sp$species %in% nut$species]) # 2501 reef species with predictions
length(sp$species[sp$species %in% nut$species])/length(unique(sp$species))*100 # 76% prediction success

sp$calcium.mg<-nut$Calcium_mu[match(sp$species,nut$species)]
sp$iron.mg<-nut$Iron_mu[match(sp$species,nut$species)]
sp$selenium.mug<-nut$Selenium_mu[match(sp$species,nut$species)]
sp$zinc.mg<-nut$Zinc_mu[match(sp$species,nut$species)]
sp$omega3.g<-nut$Omega3_mu[match(sp$species,nut$species)]
sp$vitamin_a.mug<-nut$Vitamin_A_mu[match(sp$species,nut$species)]

## get family averages
fam <- nut %>% group_by(Family) %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

## get genus averages
genus <- nut %>% group_by(Genus) %>% 
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))


save(nut, sp, fam, genus, file = 'data/WCS_nutrient_profiles.rds')
write.csv(sp, file = 'data/WCS_species_nutrient_profiles.csv')
