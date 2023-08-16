source('scripts/0_plot_theme.R')

nn<-numeric()

for(i in 1:length(nuts)){
  load(file  = paste0('results/mod/', nuts[i], 'benthic_grad_post.Rdata'))
  ndl_all$nutrient <- nuts[i]
  nn<-rbind(nn, ndl_all)
}

nn<-nn %>% mutate(fg = recode(fg, 'herbivore' = 'herbivore-detritivore',
                              'invertivore_mobile' = 'invertivore-mobile'))
nn$fg_lab<-trophic.cols$FG_lab[match(nn$fg, trophic.cols$FG)]

gg<-ggplot(nn, aes(focal_x, mu, fill=fg_lab)) + 
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.5) +
    geom_line(aes(col=fg_lab)) +
    facet_grid(focal~nutrient) +
  scale_fill_manual(values = trophic_cols.named)  +
  scale_color_manual(values = trophic_cols.named) 


pdf(file='fig/FigureSX_benthic_gradients.pdf', height=5, width=14)
print(gg)
dev.off()