## Data prep for fitting dirichlet models in brms

# ## set up model data
# nut<-'calcium.mg'

# Load reef pressure data
# https://github.com/WCS-Marine/local-reef-pressures
# devtools::load_all('../local-reef-pressures') ## fails
threat<-read.csv('data/wcs/sites-threats.csv')  %>% 
  rename_at(vars(starts_with('andrello')), ~str_replace_all(., 'andrello_', '')) %>% 
  mutate(nutrient_load = nutrient, id2=paste(site, country, sep='_')) %>% 
  dplyr::select(-nutrient)

threat$management<-prod$management[match(threat$site, prod$site)]
fish_avg$management<-prod$management[match(fish_avg$site, prod$site)]

# get country averages for filling
threat_co<-threat %>% group_by(country) %>% summarise_at(vars(grav_nc:nutrient_load), mean, na.rm=TRUE)
threat_manage<-threat %>% group_by(management) %>% summarise_at(vars(grav_nc:nutrient_load), mean, na.rm=TRUE)
# fish_avg_manage<-fish_avg %>% group_by(reef_type, reef_zone, management_rules, management) %>%
#   summarise_at(vars(rubble:fish_richness), mean, na.rm=TRUE)

fish_avg_site<-fish_avg %>% group_by(site, country, id2, reef_type, reef_zone, management_rules) %>%
  summarise_at(vars(biomass_kgha, rubble:fish_richness), mean, na.rm=TRUE)

summary(threat) ## 11 NAs in Fiji + Madagascar
threat$grav_nc[is.na(threat$grav_nc)]<-threat_co$grav_nc[match(threat$country[is.na(threat$grav_nc)], threat_co$country)]
threat$sediment[is.na(threat$sediment)]<-threat_co$sediment[match(threat$country[is.na(threat$sediment)], threat_co$country)]
threat$nutrient_load[is.na(threat$nutrient_load)]<-threat_co$nutrient_load[match(threat$country[is.na(threat$nutrient_load)], threat_co$country)]
threat$pop_count[is.na(threat$pop_count)]<-threat_co$pop_count[match(threat$country[is.na(threat$pop_count)], threat_co$country)]
threat$country<-NULL

## transform skwewed predictors
threat$grav_nc<-log10(threat$grav_nc)
threat$pop_count<-log10(threat$pop_count+1)
threat$sediment<-log10(threat$sediment+1)
threat$nutrient_load<-log10(threat$nutrient_load+1)

# management
manage<-read.csv(file = 'data/wcs/mermaid_management_clean.csv') %>% dplyr::select(-country)

# join prod estimates with benthic + fishing covariates
focal<-left_join(data.frame(prod_fg) %>% select(-biomass_kgha) %>% 
                   mutate(#id = paste(site,year, country, sep='_'),
                          id2=paste(site, country, sep='_')),
                   # mutate(id = management, id2=management), 
                 fish_avg_site %>% ungroup() %>%  
                   dplyr::select(reef_type, reef_zone, management_rules, biomass_kgha,
                          hard_coral, macroalgae, turf_algae, bare_substrate, rubble, depth, fish_richness, id2),
                 by=c('id2')) %>% 
  left_join(threat, by = 'id2') %>%
  # left_join(threat_manage, by = 'management') %>% 
  mutate(management_rules = recode(management_rules, 'periodic closure' = 'restriction',
                                   'gear restriction' = 'restriction',
                                   'periodic closure; access restriction' = 'restriction',
                                   'open access' = 'open-access',
                                   'no take' = 'no-take',
                                   'access restriction' = 'restriction')) %>%
  filter(!is.na(depth)) %>%  # dropping 2 sites (NK02 in Madasgascar and WaiE1 in Fiji)
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(id2,
    nutprop, prodprop,biomprop, nutrient, nutrient_lab, country, fg, biomass_kgha,
    hard_coral, macroalgae, turf_algae, bare_substrate, rubble, reef_type, reef_zone, depth,
    management_rules, grav_nc, sediment, nutrient_load, pop_count) %>%  
  mutate(management_rules = fct_relevel(management_rules, 'open-access', after=0))


if(nut == 'productivity'){
  focal<-focal %>% filter(nutrient=='calcium.mg') %>% mutate(nutprop=prodprop) %>% select(-biomprop, -prodprop)
  } else 

if(nut == 'biomass'){
  focal<-focal %>% filter(nutrient=='calcium.mg') %>% mutate(nutprop=biomprop) %>% select(-biomprop, -prodprop)
  } else {
  focal<-focal %>% filter(nutrient==nut) %>% select(-prodprop, -biomprop)
  }

## check reponse hist, bounded 0 - 1
hist(focal$nutprop, main = nut, xlab = 'Proportion of productivity')


focal<-  focal %>% pivot_wider(names_from = fg, values_from = nutprop) 

write.csv(focal %>% janitor::clean_names(), file = paste0('data/mod/', nut, '_unscaled.csv'))
