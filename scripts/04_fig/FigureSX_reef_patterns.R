
library(tidyverse)
load(file = 'results/wcs_productivity.rds')

g1<-ggplot(prod_reef %>% filter(nutrient=='calcium.mg'), 
       aes(biomass_kgha, biomass_turnover*100, col=country)) + geom_point() +
  labs(x ='', y = 'biomass turnover, %') +
  theme(legend.position = c(0.8, 0.8), legend.title = element_blank())

g2<-ggplot(prod_reef %>% filter(nutrient=='calcium.mg'), 
       aes(biomass_kgha, prod_day_ha, col=country)) + geom_point() + guides(colour='none') +
  labs(x = expression(paste('biomass kg ha'^-1)), y = expression(paste('productivity g day'^-1,'ha'^-1)))
  
g3<-ggplot(prod_reef, aes(biomass_kgha, nut_prod_day_ha, col=country)) + 
    geom_point() + guides(colour='none') + facet_wrap(~nutrient, scales='free') +
  labs(x = expression(paste('biomass kg ha'^-1)), y = expression(paste('nutrient productivity day'^-1,'ha'^-1))) 
  

pdf(file = 'fig/FigureSX_reef_services.pdf', width=12, height = 5)
plot_grid(
  plot_grid(g1, g2, nrow=2, labels=c('(a)', '(b)'), label_size=10),
  g3, nrow=1, rel_widths=c(0.6, 1), labels=c('', '(c)'), label_size=10
  )
dev.off()