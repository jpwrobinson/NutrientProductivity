rm(ndl_all, nd, ndl)

covs<-c('hard_coral',
         'macroalgae',
         'bare_substrate',
         'turf_algae',
         'rubble')

nuts<-c('zinc.mg','calcium.mg','iron.mg',#'vitamin_a.mug','selenium.mug','omega3.g',
        'biomass', 'productivity')

bind <- function(...) cbind(...)


for(i in 1:length(nuts)){
  nut<-nuts[i]
  load(paste0('results/mod/', nut, '_brms.Rdata'))

  ndl_all<-numeric()
  for(a in 1:length(covs)){

    ## predictor dat
    seqq<-seq(min(focal[,covs[a]]),max(focal[,covs[a]]), length.out=100)
    
    nd<-expand.grid(seqq= seqq,
                    biomass_kgha = mean(focal$biomass_kgha),
                    country = unique(focal$country)[1], 
                    management_rules = unique(focal$management_rules)[1],
                    depth=mean(focal$depth),
                    hard_coral = mean(focal$hard_coral),
                    macroalgae = mean(focal$macroalgae),
                    turf_algae = mean(focal$turf_algae),
                    bare_substrate = mean(focal$bare_substrate),
                    rubble = mean(focal$rubble) 
                    )

    nd[,covs[a]]<-nd$seqq
    nd$seqq<-NULL

    # sample predictions
    pred<-posterior_epred(fit, newdata = nd,  re_formula=NULL)
    name<-dimnames(pred)[[3]]
    for(i in 1:length(name)){
      t<-pred[,,i]
      mu<-apply(t, 2, median)
      lower<-apply(t, 2, HPDI, 0.95)[1,]
      upper<-apply(t, 2, HPDI, 0.95)[2,]
      nd$mu<-mu
      nd$lower<-lower
      nd$upper<-upper
      colnames(nd)[colnames(nd)=='mu']<-paste(name[i], 'mu', sep = '-')
      colnames(nd)[colnames(nd)=='lower']<-paste(name[i], 'lower', sep = '-')
      colnames(nd)[colnames(nd)=='upper']<-paste(name[i], 'upper', sep = '-')
    }

    ndl<-nd %>% pivot_longer(-c(biomass_kgha, country, management_rules, depth,
                                hard_coral, macroalgae, turf_algae, bare_substrate, rubble),
                                        names_to = c(".value", "var"),
                                        names_sep = "-") %>% 
      pivot_longer(-c(biomass_kgha, country, management_rules, depth,hard_coral, macroalgae, 
                      turf_algae, bare_substrate, rubble,var), names_to = 'fg', values_to = 'pred') %>% 
      pivot_wider(names_from = 'var', values_from = 'pred')


    ndl$focal<-covs[a]
    ndl$focal_x<-as.numeric(unlist(ndl[,covs[a]]))
    ndl_all<-rbind(ndl_all, ndl)
  }

  
  save(ndl_all, file  = paste0('results/mod/', nut, 'benthic_grad_post.Rdata'))
}