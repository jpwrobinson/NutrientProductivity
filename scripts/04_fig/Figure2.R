
## FG level nutrient productivity
source('scripts/0_plot_theme.R')
th_ticks<-theme(axis.ticks.x = element_line(colour='black'))

# load(file = 'results/wcs_productivity.rds')
# load(file = 'results/wcs_nut_prod.rds')
load(file = 'results/wcs_nut_prod_Figure2.rds')

# fishp$fg_lab<-trophic.cols$FG_lab[match(fishp$fg, trophic.cols$FG)]
prod_fg$fg_lab<-trophic.cols$FG_lab[match(prod_fg$fg, trophic.cols$FG)]
prod_sp$fg_lab<-trophic.cols$FG_lab[match(prod_sp$fg, trophic.cols$FG)]

# fishp$fg_lab<-fg.cols$FG_lab[match(fishp$fg, fg.cols$FG)]
# prod_sp$fg_lab<-fg.cols$FG_lab[match(prod_sp$fg, fg.cols$FG)]
# prod_fg$fg_lab<-fg.cols$FG_lab[match(prod_fg$fg, fg.cols$FG)]

prod_sp<-prod_sp %>% ## drop invert sessile as these are small proportion, consistently, and not fished
             filter(!fg %in% c('invertivore-sessile', 'detritivore')) %>% 
              group_by(nutrient, country) %>% 
            # arrange((nut_prod_day_ha))  %>% 
              mutate(tnut = sum(nut_prod_day_ha), 
                  nutprop = nut_prod_day_ha / tnut * 100) %>% 
              mutate(nutrient_lab = recode(nutrient, 'calcium.mg' = 'Calcium', 'iron.mg' = 'Iron', 'zinc.mg' = 'Zinc',
                                     'selenium.mug' = 'Selenium', 'vitamin_a.mug' = 'Vitamin A', 'omega3.g' = 'Omega-3\nfatty acids'))


prod_fg_avg<-prod_fg %>% 
                  filter(!fg %in% c('invertivore-sessile', 'detritivore')) %>%
                  mutate(nutrient_lab = recode(nutrient, 'calcium.mg' = 'Calcium', 'iron.mg' = 'Iron', 'zinc.mg' = 'Zinc',
                               'selenium.mug' = 'Selenium', 'vitamin_a.mug' = 'Vitamin A', 'omega3.g' = 'Omega-3\nfatty acids')) %>% 
                  mutate(nutrient_lab2 = recode(nutrient, 'calcium.mg' = 'Ca', 'iron.mg' = 'Fe', 'zinc.mg' = 'Zn',
                               'selenium.mug' = 'Se', 'vitamin_a.mug' = 'Vit-A', 'omega3.g' = 'O-3')) %>% 
                  group_by(country, nutrient, site,nutrient_lab, nutrient_lab2) %>% 
                  mutate(nut_prod_day_ha_scaled = scale(nut_prod_day_ha)[,1]) %>% 
                  arrange((nut_prod_day_ha))  %>% 
                  mutate(tnut = sum(nut_prod_day_ha), 
                         nutprop = nut_prod_day_ha / tnut * 100) %>% 
                  group_by(fg_lab, nutrient, nutrient_lab, nutrient_lab2) %>% 
                  summarise(nutprop = mean(nutprop)) %>% 
                  mutate(id = paste(fg_lab, nutrient))

prod_fg_country<-prod_fg %>% 
  filter(!fg %in% c('invertivore-sessile', 'detritivore')) %>% 
  mutate(nutrient_lab = recode(nutrient, 'calcium.mg' = 'Calcium', 'iron.mg' = 'Iron', 'zinc.mg' = 'Zinc',
                               'selenium.mug' = 'Selenium', 'vitamin_a.mug' = 'Vitamin A', 'omega3.g' = 'Omega-3\nfatty acids'),
         nutrient_lab2 = recode(nutrient, 'calcium.mg' = 'Ca', 'iron.mg' = 'Fe', 'zinc.mg' = 'Zn',
                                       'selenium.mug' = 'Se', 'vitamin_a.mug' = 'Vit-A', 'omega3.g' = 'O-3')) %>% 
  group_by(country, fg_lab,nutrient,nutrient_lab, nutrient_lab2) %>% 
  summarise(
    nut_prod_day_ha = sum(nut_prod_day_ha),
    prod_g_day_ha = sum(prod_g_day_ha),
    biomass_kgha = sum(biomass_kgha)) %>% 
  group_by(nutrient, country) %>% 
  mutate(nut_prod_day_ha_scaled = scale(nut_prod_day_ha)) %>% 
  arrange((nut_prod_day_ha))  %>% 
  mutate(tnut = sum(nut_prod_day_ha), nutprop = nut_prod_day_ha / tnut * 100)

## average across nutrients for panel A
prod_fg2<-prod_fg_avg %>% group_by(fg_lab) %>% summarise(se = funk::se(nutprop), nutprop = mean(nutprop)) %>% 
        mutate(lower = nutprop - 2*se, upper = nutprop + 2*se)

## average nut contr by country + nutrient
prod_fg_co<-prod_fg_country %>% group_by(nutrient,nutrient_lab, nutrient_lab2, country, fg_lab) %>% 
  summarise(se = funk::se(nutprop), nutprop = mean(nutprop)) %>% 
  mutate(lower = nutprop - 2*se, upper = nutprop + 2*se)

prod_fg_co_avg<-prod_fg_co %>% group_by(fg_lab, country) %>% summarise(se = funk::se(nutprop), nutprop = mean(nutprop)) %>% 
  mutate(lower = nutprop - 2*se, upper = nutprop + 2*se)

prod_fg_co_biom<-prod_fg_country %>% filter(nutrient=='calcium.mg') %>% 
        group_by(country) %>% mutate(tb = sum(biomass_kgha), tp = sum(prod_g_day_ha)) %>% 
        group_by(tb, tp, country, fg_lab) %>% 
        summarise(biomass = sum(biomass_kgha), prod = sum(prod_g_day_ha)) %>% 
        mutate('Standing biomass' = biomass / tb * 100, 
               'Biomass turnover' = prod / tp * 100) %>% 
        pivot_longer('Standing biomass':'Biomass turnover', values_to = 'nutprop', names_to = 'nutrient') %>% 
        mutate(nutrient_lab = nutrient)

prod_fg_co<-rbind(prod_fg_co, prod_fg_co_biom) 
# prod_fg_co$trophic_lab<-factor(prod_fg_co$trophic_lab,levels=rev(unique(prod_fg_co$trophic_lab)[c(7,2,1,4,3,5,6)]))
prod_fg_co$fg_lab<-factor(prod_fg_co$fg_lab,levels=rev(unique(prod_fg_co$fg_lab)[c(8,3,4,6,2,1,5,7,9)]))
prod_fg_co$nutrient_lab<-factor(prod_fg_co$nutrient_lab, levels=rev(unique(prod_fg_co$nutrient_lab)[c(7,8,1,2,4,6,3,5)]))

## rank country by median biomass
meds<-prod_reef %>% filter(nutrient == 'calcium.mg') %>% group_by(country) %>% 
        summarise(med = median(biomass_kgha)) %>% 
        arrange(desc(med))

prod_fg_co$country<-factor(prod_fg_co$country, levels = meds$country)
prod_reef$country<-factor(prod_reef$country, levels = meds$country)

# ggplot(prod_fg, aes(scale(prod_day_ha), 
#                     nut_prod_day_ha_scaled, col=trophic_lab)) + 
#           geom_point() + geom_abline(intercept=0, slope=1 ,col='grey', linetype=5) +
#           # geom_smooth(method='lm', se = FALSE) +
#           scale_colour_manual(values = trophic_cols.named)
# 

## order prod_fg by average nutprop
# levs<-prod_fg_avg %>% group_by(fg_lab) %>% summarise(m=mean(nutprop)) %>% arrange(m) %>%  pull(fg_lab)
levs<-c('Herbivore (macroalgae)', 'Herbivore (detritivore)', 'Planktivore', 'Invertivore (mobile)','Omnivore', 'Piscivore')
prod_fg_avg$fg_lab<-factor(prod_fg_avg$fg_lab, levels = levs)
prod_fg2$fg_lab<-factor(prod_fg2$fg_lab, levels = levs)
prod_fg_co$fg_lab<-factor(prod_fg_co$fg_lab, levels = (levs))

# plabs<-c('Herbivore/detritivore zinc.mg', 'Herbivore/detritivore vitamin_a.mug', 'Invertivore (mobile) vitamin_a.mug')
g1<-ggplot(prod_fg_avg, aes(fg_lab ,nutprop, fill=fg_lab),col='black') + 
  # geom_segment(aes(x = fct_reorder(fg_lab, nut_prod_day_ha), xend =fct_reorder(fg_lab, nut_prod_day_ha), y =-Inf, yend = nut_prod_day_ha), col='grey') +
  geom_point(size=2, pch=21) +
  # geom_text_repel(aes(label=nutrient_lab, col=fg_lab), size=2, point.padding=0, force=0.4, force_pull=2) +
  geom_text(aes(label=nutrient_lab2, col=fg_lab), size=3, nudge_x = -.25) +
  geom_pointrange(data = prod_fg2, aes(ymin = lower, ymax = upper), size=1, fatten=4.5, pch=21) +
  # geom_label_repel(data = prod_fg %>% filter(id %in% plabs), aes(label=nutrient_lab), fill='white', size=2) +
  coord_flip() +
  th + theme(legend.position = 'none') +
  scale_fill_manual(values = trophic_cols.named)  +
  scale_color_manual(values = trophic_cols.named) +
  scale_y_continuous(breaks=seq(0, 80, by = 10), labels=seq(0, 80, by = 10)) +
  labs(x = '', y = "proportion of nutrient productivity, %") +
  th_ticks

g2<-ggplot(prod_fg_co, aes(nutrient_lab, nutprop, fill=fg_lab)) + 
  geom_bar(stat='identity', col='black',size=0.1) +
  coord_flip() +
  theme(legend.position = 'none') +
  labs(x = '', y = 'proportion of fish assemblage, %') +
  scale_fill_manual(values = trophic_cols.named) +
  scale_color_manual(values = 'white') +
  scale_y_continuous(expand=c(0,0), breaks=seq(25, 100,25)) +
  facet_grid(~country, scales='free') + th +
  theme(legend.position = 'none', strip.text.x = element_blank(), plot.margin=unit(c(0, 0.5, 0.1, 0.1), 'cm')) +
  th_ticks

## biom distributions
blabs<-prod_reef %>% filter(nutrient == 'calcium.mg') %>% group_by(country) %>% summarise(x = median(biomass_kgha)) %>% mutate(y =c(40,60,20,35))
g3<-ggplot(prod_reef %>% filter(nutrient == 'calcium.mg') %>% mutate(x = log10(biomass_kgha)),
                aes(x = x)) +
  stat_summary(aes(x = 1, y = x, xintercept = stat(y), group = country),
               fun = median, geom = "vline", linetype = 5, col='#cb181d',size = 0.8) +
        geom_histogram(col = 'black', fill='grey60', bins = 10) +
        facet_grid(~country) +
        # labs(x = expression(paste('Log'[10], ' biomass kg ha'^-1)), y = 'N sites') +
        labs(x = expression(paste('biomass kg ha'^-1)), y = 'N sites') +
        scale_y_continuous(expand=c(0,0)) +
        scale_x_continuous(expand=c(0,0), breaks=c(1:3), labels=c(10,100, '1,000')) +
      coord_cartesian(clip='off') +
      geom_text(data = blabs, aes(x = log10(x), y = y, 
                            label = paste0(round(x, 0), '~kg~ha',"^-1")), 
            parse=TRUE, hjust=c(1.1, -.1, -.1,-0.1), col='#cb181d', size=2.4) + 
        th + th_ticks
        

## sup fig
g4<-ggplot(prod_sp, aes(log10(biomass_kgha), nutprop, col=fg_lab)) + 
  geom_point(alpha=0.8, size=1.5) + facet_wrap(~nutrient, scales='free') +
  scale_color_manual(values = fg_cols.named2) +
  labs(x = expression(paste('Log'[10], ' biomass kg ha'^-1)), y = 'proportion of nutrient productivity, %') +
  th + theme(legend.position = 'none')

g5<-ggplot(prod_fg_co %>% filter(!nutrient %in% c('Standing biomass', 'Biomass turnover')), aes(fg_lab, nutprop, fill=fg_lab)) + 
  geom_point(size=2, pch=21) +
  # geom_text_repel(aes(label=nutrient_lab, col=fg_lab), size=2, point.padding=0, force=0.4, force_pull=2) +
  geom_text(aes(label=nutrient_lab2, col=fg_lab), size=3, nudge_x = -.25) +
  geom_pointrange(data = prod_fg_co_avg, aes(ymin = lower, ymax = upper), size=1, fatten=3, pch=21) +
  # geom_label_repel(data = prod_fg %>% filter(id %in% plabs), aes(label=nutrient_lab), fill='white', size=2) +
  coord_flip() +
  facet_grid(.~country, scales = 'fixed') +
  th + theme(legend.position = 'none') +
  scale_fill_manual(values = trophic_cols.named)  +
  scale_color_manual(values = trophic_cols.named) +
  scale_y_continuous(breaks=seq(0, 80, by = 10), labels=seq(0, 80, by = 10)) +
  labs(x = '', y = "proportion of nutrient productivity, %") +
  th_ticks


pdf(file='fig/Figure2.pdf', height=5, width=14)
rp<-plot_grid(g3, g2, nrow =2, rel_heights=c(0.5, 1), align='v', labels=c('', '(c)'), vjust=0)
print(
  plot_grid(g1, rp, nrow=1, labels=c('(a)', '(b)'), rel_widths=c(0.7, 1))
)
dev.off()

pdf(file='fig/FigureS2.pdf', height=6, width=10)
print(g4)
dev.off()

pdf(file='fig/FigureS3.pdf', height=4, width=14)
print(g5)
dev.off()

