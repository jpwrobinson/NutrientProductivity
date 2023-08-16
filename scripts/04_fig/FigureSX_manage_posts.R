## Functional group nutrient production by country
source('scripts/0_plot_theme.R')
library(rethinking)
library(brms)
library(ggradar)
nuts<-c('zinc.mg','calcium.mg','iron.mg','vitamin_a.mug','selenium.mug','omega3.g')

colcol<-trophic_cols.named3[c(1,2,4,6)]
ranef<-numeric()

for(i in 1:length(nuts)){
  nut<-nuts[i]
  load(paste0('results/mod/', nut, '_brms.Rdata'))
  ranef<-rbind(ranef, pp_cats %>% mutate(nutrient = nut))
}

ranef<-ranef %>% mutate(nutrient_lab = recode(nutrient, 'calcium.mg' = 'Calcium', 'iron.mg' = 'Iron', 'zinc.mg' = 'Zinc',
                                            'selenium.mug' = 'Selenium', 'vitamin_a.mug' = 'Vitamin A', 'omega3.g' = 'Omega-3'))
ranef$nutrient_lab<-factor(ranef$nutrient_lab, levels=c('Calcium', 'Iron', 'Zinc', 'Selenium', 'Vitamin A', 'Omega-3'))

ranef<-ranef %>% 
  mutate(fg = recode(fg, 'invertivoremobile' = 'invertivore_mobile'),
         level = recode(level, 'Solomon.Islands' = 'Solomon Islands'),
         country = recode(country, 'Solomon.Islands' = 'Solomon Islands'))

ranef$level <- factor(ranef$level, levels=unique(ranef$level))

g1<-ggplot(ranef %>% filter(covariate_type=='Intercept'),
       aes(fct_reorder2(level, country, manage, .desc=TRUE), med, col = fg, ymin = lw, ymax = hi)) + 
  geom_hline(yintercept= 0, linetype = 5) +
  geom_pointrange(position = position_dodge(width=0.5)) +
  coord_flip() +
  scale_colour_manual(values=colcol) +
  facet_grid(~nutrient_lab) +
  labs(x = '', y = 'Posterior median ± 95% HPDI') +
  theme(legend.title = element_blank())


pdf(file = 'fig/FigureSX_manage_covs.pdf', width=10, height = 8)
print(g1)
dev.off()
