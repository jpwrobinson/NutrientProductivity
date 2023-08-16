pacman::p_load(here, funk, knitr, tidyverse, skimr, cowplot, readxl, gghighlight, ggrepel, disco, ggpubr)


theme_set(theme_sleek())

th<-theme(axis.text.y=element_text(size=11, colour='black'),
          axis.text.x=element_text(size=11, colour='black'),
          axis.title=element_text(size=12, colour='black'),
          axis.line = element_line(colour = "black"), 
          panel.border = element_blank(),
          strip.text.x = element_text(face="bold", colour='black', size=10),
          legend.position =c(0.8, 0.6), 
          legend.title=element_blank(),
          axis.ticks=element_blank())


pd<-position_dodge(width=0.4)

empty_margin<-theme(plot.margin=unit(c(0, 0, 0, 0), 'cm'))

## colour palettes
trophic.cols<-data.frame(col=c('#fdae61', '#377eb8', '#4d9221', '#a0cd10', '#bebada', '#80cdc1', '#ffd92f'),
                         FG=c('invertivore-mobile', 'piscivore','herbivore-macroalgae', 'herbivore-detritivore','planktivore', 'omnivore', 'invertivore-sessile'),
                         FG2=c('invertivore_mobile', 'piscivore','herbivore-macroalgae', 'herbivore','planktivore', 'omnivore', 'invertivore-sessile'),
                         FG_lab=c("Invertivore (mobile)", 'Piscivore', 'Herbivore (macroalgae)', 'Herbivore (detritivore)', 'Planktivore', 'Omnivore', 'Invertivore (sessile)'))

fg.cols<-data.frame(col=c('#fdae61', '#377eb8','#80cdc1', '#4d9221','#f7fcb9', '#a0cd10', '#bebada', '#a6761d', '#ffd92f'),
                    FG=c('invertivore-mobile', 'piscivore','mixed-diet feeder', 'browser','cropper/grazer', 'scraper-excavator','planktivore', 'detritivore', 'invertivore-sessile'),
                    FG_lab=c("Invertivore (mobile)", 'Piscivore','Mixed-diet feeder', 'Herbivore (browser)','Herbivore (grazer)', 'Herbivore (scraper/excavator)', 'Planktivore', 'Detritivore', 'Invertivore (sessile)'))


diet.cols<-data.frame(col=c('#fdae61', '#377eb8',  '#4d9221', '#bebada', '#ffd92f'),
                         dietP=c('Mobile invertivore', 'Piscivore','Herbivores Microvores Detritivores','Planktivore',  'Sessile invertivore'),
                         dietP_lab=c("Invertivore (mobile)", 'Piscivore', 'Herbivore/detritivore', 'Planktivore',  'Invertivore (sessile)'))

trophic_cols.named<-setNames(as.character(trophic.cols$col), trophic.cols$FG_lab)
trophic_cols.named2<-setNames(as.character(trophic.cols$col), trophic.cols$FG)
trophic_cols.named3<-setNames(as.character(trophic.cols$col), trophic.cols$FG2)
diet_cols.named<-setNames(as.character(diet.cols$col), diet.cols$dietP_lab)
fg_cols.named<-setNames(as.character(fg.cols$col), fg.cols$FG)
fg_cols.named2<-setNames(as.character(fg.cols$col), fg.cols$FG_lab)

# fg.cols<-data.frame(col=c('#1b9e77', '#bebada',),
#                     FG = c("detritivore","planktivore",
# 		"micro-invertivore", "corallivore","macro-invertivore", "pisci-invertivore", 
# 		"cropper/grazer", "scraper", "browser", "excavator" , 
# 		"spongivore", "piscivore") ,
#                     FG_lab = c("Detritivore","Planktivore",
# 		"Micro-invertivore", "Corallivore","Macro-invertivore", "Pisci-invertivore", 
# 		"Cropper/grazer", "Scraper", "Browser", "Excavator" , 
# 		"Spongivore", "Piscivore") )


# reef.cols<-data.frame(col = c(disco::disco(palette = "vibrant")[c(2,3,1)], disco(palette = "muted")[c(1,3,4,6,7)]),
#                       type = c('Gear restriction', 'No-take', 'Open-access', 'fore reef', 'fringing', 'back reef', 'patch', 'barrier'),
#                       type_lab = c('Gear\nrestriction', 'No-take', 'Open-access', 'Forereef', 'Fringing', 'Backreef', 'Patch', 'Barrier'))

# gear.cols<-data.frame(m_col = disco(palette = "vibrant")[c(2,3,1)], 
#                       management_rules = c('gear restriction', 'no take', 'open access'),
#                       manage_lab = c('Gear restriction', 'No-take', 'Open-access'),
#                       manage_lab2 = c('G-R', 'No-take', 'O-A'))

ben.cols<-data.frame(col = c('#498FC9','#9CE5FA','#B6B400','#DEF4AC','#870E00','#FBD7D5','#FAFBB2','#F2F3F3','#C2B280','#4E4E4E','#494949','#4E4E4E'),
                     benthic = c('hard_coral', 'soft_coral', 'macroalgae', 'turf_algae', 'cyanobacteria', 'crustose_coralline_algae',
                                 'rubble', 'bare_substrate', 'sand', 'other_invertebrates', 'seagrass', 'other_benthos'),
                     benthic_lab = c('Hard coral', 'Soft coral', 'Macroalgae', 'Turf algae', 
                                     'Cyanobacteria', 'Crustose coralline algae', 'Rubble', 'Bare substrate', 'Sand', 'Other invertebrates', 
                                     'Seagrass', 'Other'))

ben_cols.named<-setNames(as.character(ben.cols$col), ben.cols$benthic)

seascape_cols<-data.frame(seascape=c('Northwest', 'Southwest', 'Northeast'), 
                          s_col = c('#1b9e77','#d95f02','#7570b3'))

nut.cols<-c('Calcium'='#de2d26', 'Iron'='#636363', 'Zinc'='#3182bd', 'Vitamin A'='#31a354',
            'Omega-3' = '#F77D29', 'Selenium' = '#776EB0', "\\" = 'black', 'Standing biomass' = '#8dd3c7', 'Biomass turnover' = '#fb9a99')

conc_lab<-expression(paste('concentration 100 g'^-1))

## extract legends
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

stat_box_data <- function(y, upper_limit = max(reef_plot$biomass_kgha) * 1.15) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('N =', length(y), '\n')
      # 'mean =', round(mean(y), 1), '\n')
    )
  )
}

# filtering function - turns outliers into NAs to be removed
filter_lims <- function(x){
  l <- boxplot.stats(x)$stats[1]
  u <- boxplot.stats(x)$stats[5]
  
  for (i in 1:length(x)){
    x[i] <- ifelse(x[i]>l & x[i]<u, x[i], NA)
  }
  return(x)
}