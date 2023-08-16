library(janitor)


fish<-read.csv('data/wcs/fish_individuals.csv') %>% 
        distinct(fish_taxon) %>% pull(fish_taxon)
length(fish) # 561 species

## read and combine the TLinf and Kmax datasets
mor<-read.csv('data/kmax/Supporting_Information_DS1.csv') %>% clean_names()
chagos<-read.csv('data/kmax/Chagos_kmax2.csv') %>% clean_names() %>% mutate(method=NA)
sey<-read.csv('data/kmax/Species traits_Seychelles_exactMaxSizeTL.csv') %>% clean_names()

## combine all 3 datasets
mast<-rbind(mor %>% select(names(sey)), sey, chagos %>% select(names(sey)))
mast %>% get_dupes()  ## dupes because Mark + Casey have species from Renato's original dataset
mast<-mast[!duplicated(mast),] %>% mutate(species = str_replace_all(species, '_', '\\ '))

## get unique size, diet and position for each species (ie ignore method + sst)
mast_sp<-mast %>% distinct(species, max_size_tl, diet, position)
write.csv(mast_sp, file = 'data/species_lmax_diet.csv', row.names = FALSE)

# how many species with Kmax data?
n_distinct(mast$species) # 702

## how many species missing ?
fish[!fish%in%mast_sp$species] ## 321

## how many total species (renato + wcs)
all_sp<-c(fish, mast_sp$species)
all_sp<-all_sp[!duplicated(all_sp)]
length(all_sp)

# save missing species csv
data.frame(species = fish[!fish%in%mast$species]) %>% 
    write.csv(file = 'data/trait/missing_species.csv', row.names=FALSE)

data.frame(species = sort(all_sp)) %>% 
  write.csv(file = 'data/trait/all_species.csv', row.names=FALSE)

