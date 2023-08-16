## depth 

pypy<-read.csv(file = 'results/pyramid_preds_biom_prod.csv') %>% 
    select(-nutrient_lab) %>% 
    rename(nutrient_lab = nutrient) %>% 
    select(id2, nutrient_lab:country, hard_coral:depth, tb) 

py<-read.csv(file = 'results/pyramid_preds.csv') %>% 
  select(id2:country, nutrient_lab, tb) %>% 
  left_join(pypy %>% select(id2, hard_coral:depth), by = 'id2', multiple = 'all')

pys<-rbind(pypy, py %>% select(colnames(pypy))) 

## get unscaled benthic covariates
foc<-read.csv('py-notebook/calcium.mg_unscaled.csv')

pys_unscaled<-left_join(pys %>% select(id2, nutrient_lab, country, tb),
                        foc %>% select(id2, hard_coral:rubble, depth), multiple='all') %>% 
  pivot_longer(-c(id2, nutrient_lab, country, tb), names_to = 'benthic', values_to = 'value') %>% 
  mutate(type = ifelse(tb > 0, 'Top-heavy', 'Bottom-heavy'))

pys_unscaled$nutrient_lab<-factor(pys_unscaled$nutrient_lab, levels = unique(pys_unscaled$nutrient_lab)[c(1,2,4,5,7,3,6,8)])
pys_unscaled$benthic<-factor(pys_unscaled$benthic, levels = unique(pys_unscaled$benthic))

g1<-ggplot(pys_unscaled, aes(value, tb, col = type)) + 
  geom_hline(yintercept = 0, linetype=5, col='grey') +
  geom_point(size=1, alpha=0.5) + 
  scale_colour_manual(values = trophic.cols$col[c(4,2)]) +
  facet_grid(nutrient_lab~benthic, scales='free_x')  +
  labs(y = 'log(piscivore/herbivore)', x ='benthic cover (%) or depth (m)') +
  guides(colour = guide_legend(override.aes = list(size=3, alpha=1))) +
  theme(strip.text.y=element_text(angle=360), 
        legend.position=c(0.08, 0.8), 
        legend.title = element_blank(),
        legend.key.size = unit(.3, "cm"),
        legend.text = element_text(size = 7))

pdf(file = 'fig/FigureSX_benthic_pyramids.pdf', height = 9, width=8)
print(g1)
dev.off()

