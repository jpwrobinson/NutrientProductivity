## Functional group nutrient production by country
source('scripts/0_plot_theme.R')
library(rethinking)
library(brms)
library(ggradar)
library(janitor)
nuts<-c('zinc.mg','calcium.mg','iron.mg','vitamin_a.mug','selenium.mug','omega3.g')

colcol<-trophic_cols.named3[c(1,2,4,6)]
post<-numeric()
covs<-numeric()
pyramid<-numeric()

for(i in 1:length(nuts)){
  nut<-nuts[i]
  load(paste0('results/mod/', nut, '_brms.Rdata'))
  pp<-as_draws_df(fit, variable = "depth|coral|algae|rubble|bare", regex=TRUE) %>% 
    select(-.chain, -.iteration, -.draw) %>% 
    pivot_longer(everything(), names_to = 'fg', values_to = 'mu') %>% 
    mutate(fg = str_replace_all(fg, 'b_mu', ''),
           var = str_split_fixed(fg, '_', 2)[,2],
           fg = str_split_fixed(fg, '_', 2)[,1]) %>% 
    group_by(fg, var) %>% 
    summarise(med = median(mu), 
              lw = HPDI(mu)[1], hi = HPDI(mu)[2],
              lw50 = HPDI(mu, prob=.5)[1], hi50 = HPDI(mu, prob=.5)[2]) 
  
  post<-rbind(post, ndl %>% mutate(nutrient = nut))
  pyramid<-rbind(pyramid, focal %>% clean_names() %>% select(id2, nutrient, country, herbivore_mu, piscivore_mu,invertivore_mobile_mu)) 
  covs<-rbind(covs, pp %>% as.data.frame() %>% mutate(nutrient = nut))
}

post<-post %>% mutate(nutrient_lab = recode(nutrient, 'calcium.mg' = 'Calcium', 'iron.mg' = 'Iron', 'zinc.mg' = 'Zinc',
                                            'selenium.mug' = 'Selenium', 'vitamin_a.mug' = 'Vitamin A', 'omega3.g' = 'Omega-3'))
post$nutrient_lab<-factor(post$nutrient_lab, levels=c('Calcium', 'Iron', 'Zinc', 'Selenium', 'Vitamin A', 'Omega-3'))

covs<-covs %>% mutate(nutrient_lab = recode(nutrient, 'calcium.mg' = 'Calcium', 'iron.mg' = 'Iron', 'zinc.mg' = 'Zinc',
                                            'selenium.mug' = 'Selenium', 'vitamin_a.mug' = 'Vitamin A', 'omega3.g' = 'Omega-3'))
covs$nutrient_lab<-factor(covs$nutrient_lab, levels=c('Calcium', 'Iron', 'Zinc', 'Selenium', 'Vitamin A', 'Omega-3'))

pyramid<-pyramid %>% mutate(nutrient_lab = recode(nutrient, 'calcium.mg' = 'Calcium', 'iron.mg' = 'Iron', 'zinc.mg' = 'Zinc',
                                                  'selenium.mug' = 'Selenium', 'vitamin_a.mug' = 'Vitamin A', 'omega3.g' = 'Omega-3'))
pyramid$nutrient_lab<-factor(pyramid$nutrient_lab, levels=c('Calcium', 'Iron', 'Zinc', 'Selenium', 'Vitamin A', 'Omega-3'))

# truncate biomass
foc<-read.csv(paste0('py-notebook/', nut[1], '_unscaled.csv'))
rr<-foc %>% 
  mutate(biomass_kgha = scale(log10(biomass_kgha))) %>% 
  group_by(country) %>% 
  summarise(min = min(biomass_kgha),max = max(biomass_kgha))

post<-left_join(post, rr, by = 'country') %>% filter(fg != 'omnivore') %>% 
  mutate(country = recode(country, 'Solomon Islands' = 'Solomon\nIslands'))

post$biomass_kgha[post$biomass_kgha < post$min]<-NA
post$biomass_kgha[post$biomass_kgha > post$max]<-NA

## original scale biomass
ss<-scale(log10(foc$biomass_kgha))
post$biomass_kgha_org<-10^(post$biomass_kgha * attr(ss, 'scaled:scale') + attr(ss, 'scaled:center')) 

## get post averages by country - nutrient
post_avg<-post %>% group_by(country, fg) %>% 
  summarise(mu = mean(mu)*100) %>% 
  mutate(fg_lab = recode(fg, herbivore = 'Herbivore', invertivore_mobile = 'Invertivore', piscivore = 'Piscivore'))

## get top vs bottom heavy
pys<-pyramid %>% mutate(tb = piscivore_mu / herbivore_mu)
write.csv(pys, file = 'results/pyramid_preds.csv')

## biomass pyramids
load(paste0('results/mod/biomass_brms.Rdata'))
pypy<-focal %>% clean_names() %>% 
  mutate(tb = piscivore_mu / herbivore_mu , nutrient = 'Standing biomass', nutrient_lab = 'Standing biomass')
pyramids<-rbind(pyramid, pypy %>%  select(names(pyramid)))

load(paste0('results/mod/productivity_brms.Rdata'))
pypy<-rbind(pypy, focal %>% clean_names() %>% 
              mutate(tb = piscivore_mu / herbivore_mu, nutrient = 'Biomass turnover', nutrient_lab = 'Biomass turnover'))
pyramids<-rbind(pyramids, pypy %>%  select(names(pyramids)))

write.csv(pypy, file = 'results/pyramid_preds_biom_prod.csv')

pyramids_agg<-pyramids %>% group_by(country, nutrient_lab) %>% 
      summarise(se_herb = funk::se(herbivore_mu), herbivore_mu = mean(herbivore_mu),
                se_pisc = funk::se(piscivore_mu), piscivore_mu = mean(piscivore_mu)) %>% 
    mutate(lower_herb = herbivore_mu - 2*se_herb, upper_herb = herbivore_mu + 2*se_herb,
           lower_pisc = piscivore_mu - 2*se_pisc, upper_pisc = piscivore_mu + 2*se_pisc)

write.csv(pyramids_agg, file = 'results/pyramid_uncertainty.csv')

pyramids$nutrient_lab<-factor(pyramids$nutrient_lab, levels=levels(pyramids$nutrient_lab)[c(7,8,1:6)])
pys_agg$nutrient_lab<-factor(pys_agg$nutrient_lab, levels=levels(pyramids$nutrient_lab))


labber<-data.frame(lab = c('Bottom-heavy', 'Top-heavy'),
                   nutrient_lab='Standing biomass',
                   piscivore_mu = c(0.17, 0.63), herbivore_mu = c(0.78, 0.08), country='Belize')
labber$nutrient_lab<-factor(labber$nutrient_lab, levels=levels(pyramids$nutrient_lab))

gl<-ggplot(pyramids, aes(piscivore_mu, herbivore_mu, colour=nutrient_lab)) + 
  geom_point(alpha=0.5) +
  geom_errorbarh(data = pyramids_agg, aes(xmin = lower_pisc, xmax = upper_pisc), colour='black') +
  geom_pointrange(data = pyramids_agg, aes(ymin = lower_herb, ymax = upper_herb, fill=nutrient_lab), pch=21, size=0.5, colour='black') +
  geom_abline(intercept = 0, slope = 1, linetype=5) +
  geom_text_repel(data = pyramids_agg, aes(label = country), col='black', size=2, force=4, force_pull = 0) + 
  scale_colour_manual(values=nut.cols) +
  scale_fill_manual(values=nut.cols) +
  scale_y_continuous(expand=c(0, 0), labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(expand=c(0, 0), labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~nutrient_lab, nrow=2) +
  labs(x = 'piscivore contribution', y = 'herbivore contribution') +
  guides(colour='none', fill='none') +
  geom_text(data = labber, aes(label = lab), colour='grey50', size = 3, hjust=0.5, vjust = 0.5)

pdf(file = 'fig/Figure3.pdf', width=9, height = 5)
gl
dev.off()
