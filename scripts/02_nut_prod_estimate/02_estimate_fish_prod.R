source('scripts/0_plot_theme.R')
library(rfishprod)
library(tidyverse)


## Script to estimate productivity of all individual fish in WCS dataset

##--------------------------------------------------##
## Data prep of fish and db (Morais) before modelling ##
##--------------------------------------------------##

fish<-read.csv('data/wcs/wcs_nutrients_individuals.csv') %>% 
    mutate(dietP = diet) %>% 
    filter(!is.na(biomass_kgha)) %>%  ## 1 fish in Belize with no L-W conversion
    filter(!is.na(lmax)) %>%  ## 0.6% of biomass / 1.4% abundance dropped
    filter(size >= 5) %>% ## 0.01% of biomass dropped
    filter(!fish_taxon %in% c('Herklotsichthys quadrimaculatus', 'Gnathodentex aureolineatus', 'Lutjanus malabaricus')) %>% 
    filter(!fish_family %in% c('Pomacentridae')) %>% ## 0.35% of biomass dropped [50 species]
    filter(!fish_family %in% c('Ginglymostomatidae', 'Myliobatidae', 'Dasyatidae', 'Carcharhinidae')) ## 5.9% of biomass dropped [8 species]
    
summary(fish$size)
#min = 5 cm, max = 280 cm, median = 18.0cm

hist(fish$size)
hist(log10(fish$size))

##--------------------------------------------------##
# 1. Check if any individuals are larger than species max size:
##--------------------------------------------------##
fish %>% filter(size >= lmax) %>% dim() ## 690 observations
fish %>% filter(size >= lmax) %>% summarise(sum(count)) ## 1611 fishes

# These need to be reduced to equal lmax (prod= exactly 0) 
# OR 0.1cm below lmax (tiny prod values)

fish$size2 <- ifelse(fish$size >= fish$lmax, fish$lmax-0.1, fish$size)
#Now use size2 for further analyses

##--------------------------------------------------##
# 2. Need to update db (rfishprod) with our diet cats
##--------------------------------------------------##
load(file = 'data/trait/wcs_sp_lmax_diet.rds')
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

db$dietP<-diet$dietP[match(db$Species, diet$species)] ## 111 missing species
db$trophic_group<-fish$trophic_group[match(db$Species, fish$fish_taxon)] ## >300 missing species

## ok big hack here but filling all Renato NA trophic_group with their original Diet values
db$trophic_group[is.na(db$trophic_group)]<-as.character(db$Diet[is.na(db$trophic_group)])
db$trophic_group[db$trophic_group == ' ']<-as.character(db$Diet[db$trophic_group == ' '])

db<-db %>% mutate(trophic_group = recode(trophic_group, 'InvMob' = 'invertivore-mobile',
                                           'InvSes' = 'invertivore-sessile',
                                           'Plktiv' = 'planktivore',
                                            'Omnivr' = 'omnivore',
                                            'HerDet' = 'herbivore-detritivore',
                                            'HerMac' = 'herbivore-macroalgae',
                                           'FisCep' = 'piscivore'))


## check how Renato diets correspond with Vale diets
cat_key<-db %>% filter(!is.na(dietP)) %>% distinct(Species, Diet, dietP) %>% 
          group_by(Diet) %>% mutate(tot = length(Species)) %>%
          group_by(Diet, dietP, tot) %>% summarise(n = length(dietP)) %>% ungroup() %>% 
          mutate(prop = n / tot * 100)

pdf(file = 'fig/explore/diet_categories_Kmax.pdf', height=6, width=10)
ggplot(cat_key, aes(dietP, prop)) + geom_bar(stat='identity') + facet_wrap(~Diet) +
  coord_flip() +
  labs(x = 'Parravicini diet', y = 'Number of species', 
       subtitle = 'Number of Parravicini diet categories (bars) within Morais diet categories (panels)')
dev.off()

## assign missing species using Morais categories and cat_key fig
db$dietP[is.na(db$dietP) & db$Diet == 'Plktiv']<-'Planktivore'
db$dietP[is.na(db$dietP) & db$Diet == 'InvMob']<-'Mobile invertivore'
db$dietP[is.na(db$dietP) & db$Diet == 'FisCep']<-'Piscivore'
db$dietP[is.na(db$dietP) & db$Diet == 'HerDet']<-'Herbivores Microvores Detritivores'
db$dietP[is.na(db$dietP) & db$Diet == 'HerMac']<-'Herbivores Microvores Detritivores'

## omnivores by species (fishbase)
db$dietP[is.na(db$dietP) & db$Diet == 'Omnivr' & db$Species == 'Diplodus cervinus']<-'Mobile invertivore'
db$dietP[is.na(db$dietP) & db$Diet == 'Omnivr' & db$Species == 'Hyporhamphus australis']<-'Herbivores Microvores Detritivores'

db %>% filter(is.na(dietP)) %>% distinct(Species, Diet)

##--------------------------------------------------##
## 3. add SST (see morais_sst_script.R)
##--------------------------------------------------##
sst<-read.csv('data/env/sst_mean_extracted.csv')
fish$sstmean<-sst$sst

##--------------------------------------------------##
# 4. Predicting Kmax, the standardised VBGF parameter (Recommendation: use 100s to 1000s iterations) 
##--------------------------------------------------##
# change db names to match fish names
db$lmax<-db$MaxSizeTL

## drop levels
db<-droplevels(db)
fish<-droplevels(fish)

## check overlap of species
length(unique(fish$fish_taxon[!fish$fish_taxon %in% db$Species])) # 371 sp in WCS not in Morais db
length(unique(fish$fish_taxon)) # 561 sp in WCS - 4 countries
371/561*100

# adapt formula from Morais & Bellwood 2018
fmod <- formula (~ sstmean + lmax + trophic_group) 

# fit xgboost model to predict Kmax
fishp <- predKmax (fish,
                     dataset = db,
                     fmod = fmod,
                     niter = 1000,
                     return = 'pred')

# save Kmax predictions
fishp <- fishp$pred
hist(fishp$Kmax)

# # Positioning fish in their growth trajectory 
# # i.e. what's the size they're supposed to have on the next day? 
fishp$L.1day <- with (fishp, applyVBGF (Lmeas = size2,
                                            Lmax = lmax,
                                            Kmax = Kmax))

head(fishp) #each fish has grown a tiny amount (in length).


#Calculate age estimates:
## 4. productivity equation
lplus<-function(lmax, Kmax, age, days=1/365){lmax*(1 - exp(-Kmax*(age+days)))} ## Renato approach, VBGF growth based on age

# estimate age of each fish (eq. 3 in Depczynski et al. 2007)
age_est<-function(lmax, lcensus, K, l0=0){(1/K)*log(lmax/((1-lcensus)*lmax))}
fishp$age<-age_est(lmax=fishp$lmax, lcensus=fishp$size2/fishp$lmax, K = fishp$Kmax)

# convert length to mass
lwp<-read.csv('data/wcs/mermaid_length_weight_params.csv') 
fishp<-left_join(fishp, lwp)
fishp$mass<-fishp$biomass_constant_a * fishp$size2 ^ fishp$biomass_constant_b

## estimate productivity of each fish
fishp$size_nextday<-lplus(lmax = fishp$lmax, K = fishp$Kmax, age = fishp$age )
fishp$prod_mass_g<-somaGain(a = fishp$biomass_constant_a, b = fishp$biomass_constant_b,
                             Lmeas = fishp$size2, Lmax = fishp$lmax, Kmax = fishp$Kmax)

## estimate natural mortality
fishp$Z<-predM(fishp$size2, Kmax = fishp$Kmax, Lmax = fishp$lmax) ## estimated mortality rate by species
fishp$per_capita_mortality<-somaLoss(fishp$Z, fishp$size2, t = 1) ## daily per capita loss from natural mortality

# remove mortality from mass gain 
fishp$prod_mass_g <- fishp$prod_mass_g - fishp$per_capita_mortality
fishp$prod_mass_g <- ifelse(fishp$prod_mass_g < 0, 0, fishp$prod_mass_g)

fishp<-fishp %>% mutate(
        prod_cm_day_perfish = size_nextday - size2,
        prod_g_day = prod_mass_g * count,
        prod_g_day_ha = prod_g_day * (10000 / transect_area)) ## convert transect to hectare

fishp$fg<-fishp$functional_group
fishp$fg[fishp$fg %in% c('corallivore', 'spongivore')]<-'invertivore-sessile'
fishp$fg[fishp$fg %in% c('scraper', 'excavator')]<-'scraper-excavator'
fishp$fg[fishp$fg %in% c('macro-invertivore', 'micro-invertivore')]<-'invertivore-mobile'
fishp$fg[fishp$fg %in% c('pisci-invertivore')]<-'mixed-diet feeder'

save(fishp, file = 'results/wcs_productivity.rds')