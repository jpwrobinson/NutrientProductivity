

nuts<-c('zinc.mg','calcium.mg','iron.mg','vitamin_a.mug','selenium.mug','omega3.g', 'biomass', 'productivity')

pdf(file='fig/brms/brms_biomass_post.pdf', height=6, width=10)
for(i in 1:length(nuts)){
  nut<-nuts[i]
  source('scripts/mod/brms_dirichlet.R')
}
dev.off()
