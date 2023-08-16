#Extract traits (Lmax and data for reef position) from FishBase

#Load libraries
require(rfishbase)
require(rvest)
require(stringr)
require(xml2)
require(tidyverse)
require(xlsx)

library(mermaidr)

#Load functions 
source("scripts/trait/traits_from_fishbase_functions.R")

#Get fish Taxonomy from Fishbase 
tax <- load_taxa() %>% 
        filter(!is.na(SpecCode)) %>% 
        filter(!SpecCode %in% c('<p>', '', '.')) %>% 
        collect()
tax<-tax[!duplicated(tax),]

## dupes in SpecCode cause problems - duplicated SpecCodes can have NA for taxonomic fields
dupe<-tax %>% as.data.frame() %>% janitor::get_dupes(SpecCode) 

# count the NAs for each dupe, then select the dupe with fewest NAs (ie most taxa info)
dupe$NAsum<-rowSums(is.na(dupe[,3:9]))
duped<-dupe %>% group_by(SpecCode) %>% slice(which.min(NAsum)) %>% select(-dupe_count, -NAsum)

## remove dupes in tax and replace with the single, most informative value from duped
tax<-tax %>% filter(!SpecCode %in% dupe$SpecCode) %>% rbind(duped) %>% 
      mutate(Species_corrected = Species)

#Import species list here
# sp <- read.csv("data/trait/all_species.csv")
sp<-mermaid_get_reference(reference = c("fishspecies", 'fishgenera', 'fishfamilies'))
# wcs_lmax<-sp$fishspecies$max_length # where does this come from? 72 NAs

## save new data with species names
species_list <- data.frame(Species = sort(sp$fishspecies$species)) # 3,308 species
species_list<-species_list %>% left_join(tax)

# validate names for incorrect species
incor<-species_list$Species[is.na(species_list$SpecCode)]
sp_data <- getTaxo(sp = incor , tax = tax)
species_list$Species_corrected[is.na(species_list$SpecCode)]<-sp_data$Species_corrected
species_list$Genus[is.na(species_list$SpecCode)]<-sp_data$Genus
species_list$Family[is.na(species_list$SpecCode)]<-sp_data$Family
species_list$SpecCode[is.na(species_list$SpecCode)]<-sp_data$SpecCode

## data checks
species_list[duplicated(species_list),]
species_list %>% filter(is.na(SpecCode))
species_list<- species_list[!duplicated(species_list),]
species_list<- species_list %>% filter(Species != 'Pterocaesio spp.')

# save 
save(species_list, file = 'data/trait/wcs_sp_data.rds')


#Get Lmax (split into 2 dfs bc of timeout error)
lmax_1500 <- getLmax(species_list[1:1500,])
lmax_2500 <- getLmax(species_list[1501:2500,])
lmax_3307<-getLmax(species_list[2501:3307,])
lmax<-rbind(lmax_1500, lmax_2500, lmax_3307)

# change incorrect value (1) for Acanthurus bahianus
lmax$lmax[lmax$Species_corrected %in% c('Acanthurus bahianus', 'Choerodon melanostigma')]<-NA

## get Diet (Parravicini et al. 2020, PLoS ONE)
diet<-read.csv('data/trait/parravicini_trophic_guilds_2020.csv') %>% janitor::clean_names() %>% 
      mutate(species = str_replace_all(species, '_', '\\ '))

## Paravicinni db has good resolution on invertivores, leading to many groups. Condense into mobile and sessile to be closer with Morais db
diet <- diet %>% mutate(dietP = recode(trophic_guild_predicted_text, 'microinvertivore' = 'Mobile invertivore',
                                       'macroinvertivore' = 'Mobile invertivore',
                                       'crustacivore' = 'Mobile invertivore',
                                       'corallivore' = 'Sessile invertivore',
                                       'sessile invertivores' = 'Sessile invertivore',
                                       'planktivore' = 'Planktivore',
                                       'piscivore' = 'Piscivore'))

lmax$diet<-diet$dietP[match(lmax$Species_corrected, diet$species)]

#rename and save
trait<-lmax
save(trait, file = 'data/trait/wcs_sp_lmax_diet.rds')

# export incorrect species
trait[trait$Species != trait$Species_corrected,] %>% write.csv('data/trait/wcs_species_incorrect.csv')
#END
