library(tidyverse)
library(brms)
source('scripts/0_plot_theme.R')

ben.cols<-data.frame(col = c('#498FC9','#9CE5FA','#B6B400','#a1d99b', 'grey', 'grey'),
                     benthic = c('hard_coral', 'bare_substrate', 'macroalgae', 'turf_algae', 'rubble', 'algae'),
                     benthic_lab = c('Hard coral', 'Bare substrate', 'Macroalgae', 'Turf algae', 'Rubble', 'Total algae'))

ben_cols.named<-setNames(as.character(ben.cols$col), ben.cols$benthic)

load('data/wcs/wcs_fish_benthic.rds')

ben<-fish_avg %>% filter(country %in% c('Belize', 'Fiji', 'Madagascar', 'Solomon Islands')) %>% 
      ungroup() %>% 
      # mutate(algae = macroalgae + turf_algae) %>% 
      select(site, country, hard_coral, macroalgae, rubble, turf_algae) %>% 
      pivot_longer(-c(site, country, hard_coral), names_to = 'sub', values_to = 'cover') %>% 
      mutate(id = paste0(country, sub)) %>% 
      filter(site != 'NK02') %>% 
      mutate(cover = ifelse(cover == 0, 0.001, cover))

# Determine posterior slopes for each country - benthic cat
subs<-unique(ben$sub)
slopes<-numeric()
intercepts<-numeric()

# fitting gamma-GLM with hard coral slope by country, extracting posterior summary for slope
for(i in 1:length(subs)){
  foc<-ben %>% filter(sub ==subs[i]) %>% mutate(hard_coral_scale = scale(hard_coral))
  brm_glm_reg <- brm(cover~(1 + hard_coral_scale | country), 
                     data = foc, 
                     family=Gamma(link="log"),
                     chains=2,iter=1000, cores=4)
  
  plot((brm_glm_reg))
  slope<-as.data.frame(coef(brm_glm_reg)$country)[,c(5,7,8)] %>% mutate(sub = subs[i])
  slope$country<-rownames(slope)
  rownames(slope)<-NULL
  slopes<-rbind(slopes, slope)
  
  intercept<-as.data.frame(coef(brm_glm_reg)$country)[,c(1,3,4)] %>% mutate(sub = subs[i])
  intercept$country<-rownames(intercept)
  rownames(intercept)<-NULL
  intercepts<-rbind(intercepts, intercept)
}

colnames(slopes)<-str_replace_all(colnames(slopes), '.hard_coral_scale', '')
colnames(intercepts)<-str_replace_all(colnames(intercepts), '.hard_coral_scale', '')
slopes$Intercept<-intercepts$Estimate.Intercept
slopes$cover_lab<-ben.cols$benthic_lab[match(slopes$sub, ben.cols$benthic)]

g1<-ggplot(slopes, aes(fct_rev(country), Estimate, ymin = Q2.5, ymax = Q97.5)) +
          geom_hline(yintercept=0, col='grey', linetype=5) +
          geom_pointrange(fatten=0, aes(colour=sub), position = position_dodge(width=0.5)) + 
          geom_point(size = 2.5, pch=21, colour='black', aes( fill=sub), position = position_dodge(width=0.5)) +
          coord_flip() + 
          facet_wrap(~1) +
          theme(legend.position = 'none', axis.text.y=element_blank(),
                strip.text.x = element_text(colour='white'),
                axis.ticks.y = element_blank(),
                plot.margin = unit(c(.1,.5,.1,-.75), 'cm')) +
          scale_fill_manual(values = ben_cols.named)  +
          scale_color_manual(values = ben_cols.named)  +
          labs(y = 'slope with hard coral cover', x = '') 

ben$cover_lab<-ben.cols$benthic_lab[match(ben$sub, ben.cols$benthic)]

g2<-ggplot(ben, aes(hard_coral, cover)) + 
  geom_point(alpha=0.8, size=1.5, pch=21, col='black', aes(fill=sub)) + 
  stat_smooth(method = 'lm', se = FALSE, col='#e41a1c') +
  facet_grid(country~cover_lab, scales='free') +
  scale_y_continuous(limits=c(0, NA)) +
  scale_fill_manual(values = ben_cols.named) +
  theme(legend.position = 'none', strip.text.y = element_text(angle=360)) +
  labs(x = 'hard coral cover, %', y = 'benthic cover, %')

pdf(file = 'fig/FigureSX_benthic_cov.pdf', height=5, width=9)
print(cowplot::plot_grid(g2, g1, nrow=1, rel_widths=c(1,0.3), labels=c('a', 'b')))
dev.off()


## hard coral distributions by country
focal<-read.csv('py-notebook/zinc.mg_reef_unscaled.csv')
# 
# ggplot(focal, aes(hard_coral)) + 
#     geom_histogram(breaks=seq(0, 80, 5)) +
#     facet_grid(country ~., scales='free_y') 

pdf(file = 'fig/FigureSX_hard_coral_range.pdf', height=2, width=6)
ggplot(focal %>% group_by(country) %>% summarise(mi =min(hard_coral), ma = max(hard_coral)) %>% 
         mutate(yy = 1:4)) + 
  geom_segment(aes(x = mi, xend = ma, y = yy, yend =yy))  +
  geom_label(aes(ma, yy+0.4, label=country)) +
  scale_y_continuous(limits=c(0.5,4.5)) +
  labs(x = 'hard coral, %') +
  theme_void() +
  theme(axis.line.x = element_line(colour='black'), 
        axis.text.x = element_text(colour='black'),
        axis.title.x = element_text(colour='black'))
dev.off()