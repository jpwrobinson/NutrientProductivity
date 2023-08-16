## dirichlet
library(brms)
library(janitor)
library(rethinking)
bind <- function(...) cbind(...)

## load file
filename<-paste0('data/mod/', nut, '_unscaled.csv')
focal<-read.csv(filename) %>% clean_names() 

focal$planktivore[focal$planktivore==0]<-0.001
focal$omnivore[focal$omnivore==0]<-0.001
# focal$herbivore_detritivore[focal$herbivore_detritivore==0]<-0.001
focal$herbivore[focal$herbivore==0]<-0.001
focal$piscivore[focal$piscivore==0]<-0.001
# focal$other[focal$other==0]<-0.001

## rescale proportions for fished functional groups
focal$tot<-rowSums(focal[,c('herbivore', #'other',
                            'invertivore_mobile', 'omnivore',
                            'piscivore' #'planktivore'
                            )])

focal$omnivore<-focal$omnivore / focal$tot
focal$herbivore<-focal$herbivore / focal$tot
focal$invertivore_mobile<-focal$invertivore_mobile / focal$tot
focal$piscivore<-focal$piscivore / focal$tot

# scale covariates and log10 biomass
focal$biomass_kgha<-scale(log10(focal$biomass_kgha))
focal$hard_coral<-scale(focal$hard_coral)
focal$macroalgae<-scale(focal$macroalgae)
focal$bare_substrate<-scale(focal$bare_substrate)
focal$rubble<-scale(focal$rubble)
focal$turf_algae<-scale(focal$turf_algae)
focal$depth<-scale(focal$depth)

fit <- brm(bind(omnivore, invertivore_mobile, herbivore, piscivore) ~ 
             biomass_kgha + depth +
             hard_coral + macroalgae + turf_algae + bare_substrate + rubble +
             (1 + biomass_kgha | country) + (1 | country : management_rules), 
           data=focal, dirichlet)

post_obs<-posterior_predict(fit, newdata = focal)
name<-dimnames(post_obs)[[3]]
for(i in 1:length(name)){
  t<-post_obs[,,i]
  mu<-apply(t, 2, median)
  # lower<-apply(t, 2, HPDI, 0.95)[1,]
  # upper<-apply(t, 2, HPDI, 0.95)[2,]
  focal$mu<-mu
  # focal$lower<-lower
  # focal$upper<-upper
  colnames(focal)[colnames(focal)=='mu']<-paste(name[i], 'mu', sep = '-')
  # colnames(focal)[colnames(focal)=='lower']<-paste(name[i], 'lower', sep = '-')
  # colnames(focal)[colnames(focal)=='upper']<-paste(name[i], 'upper', sep = '-')
}

## predictor dat
nd<-expand.grid(biomass_kgha = seq(min(focal$biomass_kgha), max(focal$biomass_kgha), 100), 
                country = unique(focal$country), 
                management_rules = unique(focal$management_rules)[1],
                depth=mean(focal$depth),
                hard_coral = mean(focal$hard_coral),
                macroalgae = mean(focal$macroalgae),
                turf_algae = mean(focal$turf_algae),
                bare_substrate = mean(focal$bare_substrate),
                rubble = mean(focal$rubble) 
                )

# sample predictions
pred<-posterior_epred(fit, newdata = nd,  re_formula=NULL)
name<-dimnames(pred)[[3]]
for(i in 1:length(name)){
  t<-pred[,,i]
  mu<-apply(t, 2, median)
  lower<-apply(t, 2, HPDI, 0.95)[1,]
  upper<-apply(t, 2, HPDI, 0.95)[2,]
  nd$mu<-mu
  nd$lower<-lower
  nd$upper<-upper
  colnames(nd)[colnames(nd)=='mu']<-paste(name[i], 'mu', sep = '-')
  colnames(nd)[colnames(nd)=='lower']<-paste(name[i], 'lower', sep = '-')
  colnames(nd)[colnames(nd)=='upper']<-paste(name[i], 'upper', sep = '-')
}

ndl<-nd %>% pivot_longer(-c(biomass_kgha, country, management_rules, depth,
                            hard_coral, macroalgae, turf_algae, bare_substrate, rubble),
                                    names_to = c(".value", "var"),
                                    names_sep = "-") %>% 
  pivot_longer(-c(biomass_kgha, country, management_rules, depth,hard_coral, macroalgae, 
                  turf_algae, bare_substrate, rubble,var), names_to = 'fg', values_to = 'pred') %>% 
  pivot_wider(names_from = 'var', values_from = 'pred')

print(
ggplot(ndl, aes(biomass_kgha, mu, fill=fg)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.5) +
  geom_line(aes(col=fg)) +
  facet_wrap(~country) +
  labs(subtitle = nut)
)


pp_cats<-as_draws_df(fit, variable = "country|management", regex=TRUE) %>%
  select(-.chain, -.iteration, -.draw) %>%
  pivot_longer(everything(), names_to = 'var', values_to = 'mu') %>%
  group_by(var) %>%
  summarise(med = median(mu),
            lw = HPDI(mu)[1], hi = HPDI(mu)[2],
            lw50 = HPDI(mu, prob=.5)[1], hi50 = HPDI(mu, prob=.5)[2]) %>% 
  filter(str_detect(var, 'r_country*|r_management*')) %>% 
  filter(!str_detect(var, 'cor_country')) %>% 
  mutate(cat = ifelse(str_detect(var, 'r_country'), 'Country', 'Management rules')) %>% 
  mutate(#var2 = str_replace_all(var, 'r_', ''),
    var2 = str_replace_all(var, 'mu', ''),
    fg = str_split_fixed(var2, '__', 2)[,2],
    var2 = str_split_fixed(var2, '__', 2)[,1],
    covariate_type = str_split_fixed(fg, '\\[', 2)[,2],
    fg = str_split_fixed(fg, '\\[', 2)[,1],
    level = str_split_fixed(covariate_type, '\\,', 2)[,1],
    covariate_type = str_split_fixed(covariate_type, '\\,', 2)[,2],
    covariate_type = str_replace_all(covariate_type, '\\]', ''),
    covariate_term = str_replace_all(var2, 'r_', ''),
    country = ifelse(covariate_term =='country', level, str_split_fixed(level,'_', 2)[,1]),
    manage = ifelse(covariate_term =='country', 'Country', str_split_fixed(level,'_', 2)[,2]),
    level = ifelse(covariate_term =='country', level, str_replace_all(level, '_', ': '))) %>% select(-var2, -var)


save(fit, ndl, focal, pp_cats, file  = paste0('results/mod/', nut, '_brms.Rdata'))
