pacman::p_load(tidyverse, skimr, cowplot, here, funk,disco, patchwork, install=FALSE)
source('scripts/0_plot_theme.R')



## Figure viz of nutrient density and K of reef fishery target species

## load Kmax predictions for WCS fish
load(file = 'results/wcs_productivity.rds')
## get average Kmax, as these have uncertainty and were predicted for each observed species in fish
fish2<-fishp %>% group_by(fish_taxon, trophic_group, dietP, lmax, nscore,
                            calcium.mg, iron.mg, selenium.mug, zinc.mg, omega3.g, vitamin_a.mug) %>% 
    summarise(Kmax =mean(Kmax)) 

fish2$trophic_lab<-trophic.cols$FG_lab[match(fish2$trophic_group, trophic.cols$FG)]
fish2$dietP_lab<-diet.cols$dietP_lab[match(fish2$dietP, diet.cols$dietP)]

## top species by biomass in each fg
topsp<-fishp %>% 
  group_by(fish_taxon, trophic_group, site, country, transect_number) %>% 
  summarise(biomass_kgha = sum(biomass_kgha)) %>% 
  group_by(fish_taxon, trophic_group, site, country) %>% 
  summarise(biomass_kgha = mean(biomass_kgha)) %>% 
  group_by(fish_taxon, trophic_group, country) %>% 
  summarise(biomass_kgha = mean(biomass_kgha)) %>% 
  group_by(fish_taxon, trophic_group) %>% 
  summarise(biomass_kgha = mean(biomass_kgha)) %>% 
  ungroup() %>% 
  group_by(trophic_group) %>% 
  slice_max(n =2, order_by = biomass_kgha, with_ties = FALSE) %>%
  distinct(trophic_group, fish_taxon) %>%
  left_join(fish2, by = 'fish_taxon')

## species labs
sp<-str_split_fixed(fish2$fish_taxon, '\\ ', 2)
# fish2$species.lab<-paste0(substring(sp[,1],1,5),  '. ', sp[,2], '.')
fish2$species.lab<-paste0(sp[,1],'\n', sp[,2])

# long version
fish2l <- fish2 %>% pivot_longer(calcium.mg:vitamin_a.mug, names_to = 'nutrient', values_to = 'conc') %>% 
  mutate(nutrient_lab = recode(nutrient, 'calcium.mg' = 'Calcium', 'iron.mg' = 'Iron', 'zinc.mg' = 'Zinc',
                               'selenium.mug' = 'Selenium', 'vitamin_a.mug' = 'Vitamin A', 'omega3.g' = 'Omega-3\nfatty acids'))
fish2l$nutrient_lab<-factor(fish2l$nutrient_lab, levels=(unique(fish2l$nutrient_lab)))

## histogram labs
se<-function(x) {sd(x) / sqrt(length(x))}
hlabs<-fish2 %>% group_by(trophic_lab) %>% mutate(Kmax = log10(Kmax)) %>% 
  summarise(sek = se(Kmax), sen = se(nscore), Kmax = mean(Kmax), nscore = mean(nscore)) %>% 
  mutate(Klo = Kmax - 2*sek, Khi = Kmax + 2*sek, nhi = nscore + 2*sen, nlo = nscore - 2*sen)

gg<-ggplot(fish2, aes(Kmax, nscore, col=trophic_lab)) + 
  geom_point(size=2.5, pch=21, col='black', alpha=0.8, aes(fill=trophic_lab)) +
  # geom_pointrange(data = hlabs, fatten=5.5, aes(ymin = nlo, ymax = nhi, fill=trophic_lab),col='black', pch=21) +
  # geom_errorbarh(data = hlabs, aes(xmin = Klo, xmax = Khi)) +
  # geom_label_repel(data = fish2 %>% filter(nscore>400), aes(label = species.lab), fontface=1,size=2, show.legend=FALSE) +
  # geom_label_repel(data = fish2 %>% filter(Kmax>0.4), aes(label = species.lab), fontface=1,size=2, show.legend=FALSE) +
  geom_label_repel(data = topsp, aes(label = fish_taxon), fontface=3,size=2, min.segment.length = 0, 
                   box.padding = .5,max.overlaps=Inf, point.padding = 0.5, force = 30, show.legend=FALSE) +
  labs(x = 'Derived growth coefficent (Kmax)', y = 'Nutrient density, %') +
  th +
  theme(legend.position = 'none',
        panel.border=element_rect(color='black'),
        axis.line = element_blank()) +
  scale_fill_manual(values = trophic_cols.named) +
  scale_colour_manual(values = trophic_cols.named) +
  scale_x_log10()
  # scale_x_continuous(breaks=c(-1.5, -1, -.5, 0, .5), labels=c(0.03, 0.1, 0.3, 1, 3.2))

gpan<-ggplot(fish2l, aes(Kmax, conc)) +
  geom_point(size=1.5, pch=21, col='black', aes(fill=trophic_lab)) +
  labs(x = 'Derived growth coefficent (Kmax)', y = conc_lab) +
  # scale_x_continuous(breaks=seq(0, 0.6, by = 0.1)) +
  th +
  facet_grid(nutrient_lab~., scales='free_y') +
  theme(legend.position = 'none', strip.text.y = element_text(angle=360)) +
  scale_fill_manual(values = trophic_cols.named) +
  scale_x_log10() +
  scale_colour_manual(values = trophic_cols.named) +
  geom_smooth(method = 'gam', col='black')

gsup<-ggplot(fish2l, aes(lmax, conc)) +
  geom_point(size=1.5, pch=21, col='black', aes(fill=trophic_lab)) +
  labs(x = 'maximum body size (Lmax, cm)', y = conc_lab) +
  # scale_x_continuous(breaks=seq(0, 0.6, by = 0.1)) +
  th +
  facet_wrap(nutrient_lab~., scales='free_y', nrow=3) +
  theme(legend.position = 'none', strip.text.y = element_text(angle=360)) +
  scale_fill_manual(values = trophic_cols.named) +
  scale_x_log10() +
  scale_colour_manual(values = trophic_cols.named) +
  geom_smooth(method = 'gam', col='black')

pdf(file = 'fig/Figure1.pdf', height=6, width=11)
print(plot_grid(
  ggExtra::ggMarginal(gg, type='histogram', fill='grey50', col='white', bins=20),
  gpan,
  labels=c('(a)', '(b)'),
  ncol=2, rel_widths=c(1, 0.5)))
dev.off()

pdf(file = 'fig/FigureS1.pdf', height=8, width=11)
print(gsup)
dev.off()



