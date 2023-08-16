pacman::p_load(tidyverse, funk, install=FALSE)
theme_set(theme_bw())

## load datasets
load(file = 'results/wcs_productivity.rds')
load(file = 'results/wcs_nut_prod.rds')
load('data/wcs/wcs_fish_benthic.rds')

prod_reef<-prod_reef %>% 
  mutate(nutrient_lab = recode(nutrient, 'calcium.mg' = 'Calcium', 'iron.mg' = 'Iron', 'zinc.mg' = 'Zinc',
                               'selenium.mug' = 'Selenium', 'vitamin_a.mug' = 'Vitamin A', 'omega3.g' = 'Omega-3\nfatty acids')) 

## recode and estimate nutrient proportion per site per fg
prod_fg<-prod_fg %>%
  mutate(nutrient_lab = recode(nutrient, 'calcium.mg' = 'Calcium', 'iron.mg' = 'Iron', 'zinc.mg' = 'Zinc',
                               'selenium.mug' = 'Selenium', 'vitamin_a.mug' = 'Vitamin A', 'omega3.g' = 'Omega-3\nfatty acids')) %>% 
  group_by(country,site, nutrient,nutrient_lab) %>%
  mutate(tnut = sum(nut_prod_day_ha), 
         nutprop = nut_prod_day_ha / tnut,
         tprod = sum(biomass_turnover),
         prodprop = biomass_turnover / tprod,
         tbiom = sum(biomass_kgha),
         biomprop = biomass_kgha / tbiom) 

## looping by nutrient
nut.vec<-unique(prod_fg$nutrient)

# model data - extract for fg
for(i in 1:length(nut.vec)){
  nut<-nut.vec[i]
  print(paste('Data mangle + scale for', nut))
  source('scripts/03_brms_mod/data_extract.R')
}

## biomass or productivity only
nut = 'productivity'
source('scripts/03_brms_mod/data_extract.R')

nut = 'biomass'
source('scripts/03_brms_mod/data_extract.R')