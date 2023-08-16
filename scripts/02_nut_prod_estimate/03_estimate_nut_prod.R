library(tidyverse)
source('scripts/0_plot_theme.R')
# join mean nutrient concentrations with productivity estimates
## pivot and estimate nutrient productivity per nutrient
# definitions: https://github.com/jpwrobinson/nut-prod/issues/1
load(file = 'results/wcs_productivity.rds')

## estimate productivity, accounting for edible yield
prod<-fishp %>% rowwise() %>% 
    mutate(nscore3 = sum(ca_rda, fe_rda, zn_rda)) %>% 
    pivot_longer(calcium.mg:vitamin_a.mug, names_to = 'nutrient', values_to = 'conc') %>% 
    mutate(nut_prod_day_ha = conc / 100 * prod_g_day_ha * 0.87, ## nutrients produced per day per hectare
           nut_biomass_kgha = conc * 10 * 0.87 * biomass_kgha) ## nutrient yield per hectare

## change here to set the base FG for all analyses
prod<-prod %>% mutate(fg = trophic_group)
  
## reef level estimates of nutrient productivity metrics
prod_reef<-prod %>% group_by(country, management, site, year, sample_date, transect_number, depth, nutrient) %>% 
            summarise(
              nscore = weighted.mean(nscore, w = biomass_kgha),
              nscore3 = weighted.mean(nscore3, w = biomass_kgha),
              nut_prod_day_ha = sum(nut_prod_day_ha), 
              nut_biomass_kgha = sum(nut_biomass_kgha),
              prod_day_ha = sum(prod_g_day_ha),
              biomass_kgha = sum(biomass_kgha),
              depth = mean(depth)) %>% 
            group_by(country, management, site, year, nutrient) %>% 
            summarise(
              depth = mean(depth),
              nut_prod_day_ha = mean(nut_prod_day_ha), 
              nut_biomass_kgha = mean(nut_biomass_kgha),
              prod_day_ha = mean(prod_day_ha),
              biomass_kgha = mean(biomass_kgha),
              nscore = mean(nscore),
              nscore3 = mean(nscore3))  %>% 
            ## turnover is per year
      mutate(nut_turnover = ((nut_prod_day_ha/1000) / (nut_biomass_kgha)) * 100,
         biomass_turnover = ((prod_day_ha/1000) / (biomass_kgha)) * 100) 


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
            biomass_kgha = mean(biomass_kgha))
  
## Rows are filled with zeroes if species were observed in country-year-management_rule and are not in site
# prod_sp %>% group_by(site, country, year, management_rules) %>% summarise(n_distinct(fish_taxon))

others<-c('invertivore-sessile', 'detritivore', 'herbivore-macroalgae')

# FG level nutrient productivity
prod_fg<-prod %>% 
  mutate(id = paste(site, year, sep = '_')) %>% 
  ## drop invert sessile as these are small proportion, consistently, and not fished
  filter(!fg %in% c('invertivore-sessile', 'detritivore')) %>% 
  mutate(fg = ifelse(str_detect(fg, 'herb'), 'herbivore', fg)) %>% 
  ## transect level: total metric by diet group
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


# scale nutrient productivity, estimate summed nutrient productivity across 6 nutriens
prod_scale<-prod_sp %>% group_by(nutrient) %>% 
      mutate(nut_prod_day_ha_scale = scale(nut_prod_day_ha)) %>% 
      ungroup() %>% 
      group_by(fish_taxon, fg_lab) %>% 
      summarise(nut_prod_score = sum(nut_prod_day_ha_scale),
                biomass_kgha = unique(biomass_kgha))


save(prod, prod_reef, prod_sp, prod_fg, prod_scale, file = 'results/wcs_nut_prod.rds')
