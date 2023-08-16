library(tidyverse)
source('scripts/0_plot_theme.R')
# join mean nutrient concentrations with productivity estimates
## pivot and estimate nutrient productivity per nutrient
# definitions: https://github.com/jpwrobinson/nut-prod/issues/1
load(file = 'results/wcs_productivity.rds')
load(file = 'results/wcs_nut_prod.rds')

## estimate productivity, accounting for edible yield
prod<-fishp %>% rowwise() %>% 
  mutate(nscore3 = sum(ca_rda, fe_rda, zn_rda)) %>% 
  pivot_longer(calcium.mg:vitamin_a.mug, names_to = 'nutrient', values_to = 'conc') %>% 
  mutate(nut_prod_day_ha = conc / 100 * prod_g_day_ha * 0.87, ## nutrients produced per day per hectare
         nut_biomass_kgha = conc * 10 * 0.87 * biomass_kgha) ## nutrient yield per hectare

## change here to set the base FG for all analyses
prod<-prod %>% mutate(fg = trophic_group)
## species level estimates of nut prod metrics
prod_sp<-prod %>% 
  group_by(country, management_rules, site, year, sample_date, 
           fish_taxon, transect_number, depth, nutrient, fg) %>% 
  summarise(
    nut_prod_day_ha = sum(nut_prod_day_ha), 
    nut_biomass_kgha = sum(nut_biomass_kgha),
    prod_g_day_ha = sum(prod_g_day_ha),
    biomass_kgha = sum(biomass_kgha)) %>% 
  ungroup() %>% 
  dplyr::select(country, site,transect_number,year, management_rules,  fish_taxon, 
                nutrient, fg, nut_prod_day_ha:biomass_kgha) %>% 
  group_by(country, management_rules, site, year, fish_taxon, nutrient, fg) %>%
  summarise(
    nut_prod_day_ha = mean(nut_prod_day_ha), 
    nut_biomass_kgha = mean(nut_biomass_kgha),
    prod_g_day_ha = mean(prod_g_day_ha),
    biomass_kgha = mean(biomass_kgha)) %>% 
  group_by(country, fish_taxon, nutrient, fg) %>% 
  summarise(nut_prod_day_ha = mean(nut_prod_day_ha), 
            nut_biomass_kgha = mean(nut_biomass_kgha),
            prod_g_day_ha = mean(prod_g_day_ha),
            biomass_kgha = mean(biomass_kgha)) %>% 
  mutate(nut_turnover = ((nut_prod_day_ha/1000) / (nut_biomass_kgha)) * 100,
         biomass_turnover = ((prod_g_day_ha/1000) / (biomass_kgha)) * 100) %>% 
  mutate(nut_turnover = ifelse(nut_prod_day_ha == 0, 0, nut_turnover),
         biomass_turnover = ifelse(prod_g_day_ha == 0, 0, biomass_turnover))


# FG level nutrient productivity
prod_fg<-prod %>% 
  mutate(id = paste(site, year, sep = '_')) %>% 
  ## drop invert sessile as these are small proportion, consistently, and not fished
  filter(!fg %in% c('invertivore-sessile', 'detritivore')) %>% 
  group_by(country, site, year, id, transect_number,fg, nutrient) %>% 
  summarise(
    nut_prod_day_ha = sum(nut_prod_day_ha), 
    nut_biomass_kgha = sum(nut_biomass_kgha),
    prod_g_day_ha = sum(prod_g_day_ha),
    biomass_kgha = sum(biomass_kgha)) %>% 
  group_by(country) %>%
  complete(fg, nesting(nutrient, site, transect_number, id),
           fill = list(nut_prod_day_ha = 0, nut_biomass_kgha = 0, prod_g_day_ha =0, biomass_kgha = 0))  %>% 
  ungroup() %>% 
  group_by(country,fg, site, nutrient) %>% 
  summarise(
    nut_prod_day_ha = mean(nut_prod_day_ha), 
    nut_biomass_kgha = mean(nut_biomass_kgha),
    prod_g_day_ha = mean(prod_g_day_ha),
    biomass_kgha = mean(biomass_kgha))  %>% 
  mutate(nut_turnover = ((nut_prod_day_ha/1000) / (nut_biomass_kgha)) * 100,
         biomass_turnover = ((prod_g_day_ha/1000) / (biomass_kgha)) * 100) %>% 
  mutate(nut_turnover = ifelse(nut_prod_day_ha == 0, 0, nut_turnover),
         biomass_turnover = ifelse(prod_g_day_ha == 0, 0, biomass_turnover))


## labelling for plots
prod_sp$fg_lab<-fg.cols$FG_lab[match(prod_sp$fg, trophic.cols$FG)]

save(prod_reef, prod_fg,prod_sp, file = 'results/wcs_nut_prod_Figure2.rds')
