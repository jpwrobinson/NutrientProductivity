
########################################################################## 
#####      Supporting Information: Supplementary Script          ##### 
##### RA Morais and DR Bellwood - Global drivers of reef fish growth ##### 
########################################################################## 

#---------------------------------------------------# 
##  Calculating pelagic primary productivity from  ## 
## satellite-derived mean chlorophyl concentration ## 
#---------------------------------------------------# 

require (rgdal) 
require (raster) 
require (geosphere) 

#-----------------------------------------------------------------------------------# 
## Loading mean chlorophyl, mean sea surface temperature and photosynthetic  ##  
## active radiation rasters saved from Bio-Oracle       ##  
#-----------------------------------------------------------------------------------# 

data.sstmean <- readGDAL('data/env/Present.Surface.Temperature.Mean.asc') 

#------------------------------------------------------------------# 
## A data-frame with the geographic coordinates to extract values ## 
## It is important that these coordinates are in decimal degrees  ## 
#------------------------------------------------------------------#
focs<-c('Fiji', 'Madagascar', 'Solomon Islands', 'Belize')
fish<-read.csv('data/wcs/wcs_nutrients_individuals.csv') %>% 
              mutate(dietP = diet) %>% 
              filter(!is.na(biomass_kgha)) %>%  ## 1 fish in Belize with no L-W conversion
              filter(!is.na(lmax)) %>%  ## 0.6% of biomass / 1.4% abundance dropped
              filter(size >= 5) %>% ## 0.01% of biomass dropped
              filter(!fish_taxon %in% c('Herklotsichthys quadrimaculatus', 'Gnathodentex aureolineatus', 'Lutjanus malabaricus')) %>% 
              filter(!fish_family %in% c('Pomacentridae')) %>% ## 0.35% of biomass dropped [50 species]
              filter(!fish_family %in% c('Ginglymostomatidae', 'Myliobatidae', 'Dasyatidae', 'Carcharhinidae')) ## 5.9% of biomass dropped [8 species]

coords <- fish %>% dplyr::select(longitude, latitude)

#-------------------------------------------------# 
## Extracting the values from the loaded rasters ## 
#-------------------------------------------------# 

sstmean  <- raster::extract (raster (data.sstmean), coords, method = "bilinear") 

write.csv(data.frame(sst=sstmean), 'data/env/sst_mean_extracted.csv', row.names=FALSE)
