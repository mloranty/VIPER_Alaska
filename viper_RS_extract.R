##################################
#
# script to extract RS 
# data sets for AK borehole
# sites sampled for VIPER project
#
# ML 10 Jan 2020
##################################

library(MODISTools)
library(geosphere)
library(raster)
library(rgdal)
library(daymetr)
setwd('L:/projects/VIPER_Alaska')


#############################################
# download site coordinates directly from ADC
coord <- read.table('https://arcticdata.io/metacat/d1/mn/v2/object/urn%3Auuid%3A2c1be479-e402-4a53-b7bf-d0bb31385b3f',
                    sep = ',', header = T)

# longitude should be negative
coord$Long <- -coord$Long

# calculate the midpoint of transect 2, i.e. the center of the sample area
# coords for Transect to 0m
t2.0 <- coord[which(coord$Transect == 2 & coord$Location == 0),]

# coords for Transect to 20m
t2.20 <- coord[which(coord$Transect == 2 & coord$Location == 20),]

# calculate the midpoint between the two 
coords <- midPoint(t2.0[,c(5,4)],t2.20[,c(5,4)])

# add site identifier
coords <- as.data.frame(cbind(t2.0$Site,coords[,c(2,1)]))

# format colnames for use with MODISTools
names(coords) <- c('site_name', 'lat', 'lon')

# write csv file for daymet download
write.csv(coords, file = 'site_coords.csv',
          row.names = F)

# clean workspace
rm(coord,t2.0, t2.20)

################################################
# use MODISTools to download VIs and Daymet data

# ndvi - 250m resolution, single pixel per site
ndvi <- mt_batch_subset(df = coords,
                           product = "MOD13Q1",
                           band = '250m_16_days_NDVI',
                           internal = TRUE,
                           start = "2001-05-01",
                           end = "2019-10-30")

write.csv(ndvi,file = 'viper_ak_ndvi.csv',
          row.names = F,
          col.names = T)

# evi - 250m resolution, single pixel per site
evi <- mt_batch_subset(df = coords,
                      product = "MOD13Q1",
                      band = '250m_16_days_EVI',
                      internal = TRUE,
                      start = "2001-05-01",
                      end = "2019-10-30")

write.csv(evi,file = 'viper_ak_evi.csv',
          row.names = F,
          colnames = T)

# pixel reliability flag, values are intepreted as follows
# -1 Fill/No Data Not Processed
#  0 Good Data Use with confidence
#  1 Marginal data Useful, but look at other QA information
#  2 Snow/Ice Target covered with snow/ice
#  3 Cloudy Target not visible, covered with cloud

viqc <- mt_batch_subset(df = coords,
                      product = "MOD13Q1",
                      band = '250m_16_days_pixel_reliability',
                      internal = TRUE,
                      start = "2001-05-01",
                      end = "2019-10-30")

write.csv(viqc,file = 'viper_ak_vi_qc.csv',
          row.names = F,
          col.names = T)

# MODIS Terra Land Surface Temperature (LST), single pixel per site
lst <- mt_batch_subset(df = coords,
                       product = "MOD11A2",
                       band = 'LST_Day_1km',
                       internal = TRUE,
                       start = "2001-05-01",
                       end = "2019-10-30")

write.csv(lst,file = 'viper_ak_lst.csv',
          row.names = F)

# MODIS Terra Land Area Index (LAI), single pixel per site
lai <- mt_batch_subset(df = coords,
                       product = "MOD15A2H",
                       band = 'Lai_500m',
                       internal = TRUE,
                       start = "2001-05-01",
                       end = "2019-10-30")

write.csv(lai,file = 'viper_ak_lai.csv',
          row.names = F)

# MODIS Evapotranspiration (ET), single pixel per site
et <- mt_batch_subset(df = coords,
                       product = "MOD16A2",
                       band = 'ET_500m',
                       internal = TRUE,
                       start = "2001-05-01",
                       end = "2019-10-30")

write.csv(et,file = 'viper_ak_et.csv',
          row.names = F)

# MODIS VCF , single pixel per site
tree <- mt_batch_subset(df = coords,
                      product = "MOD44B",
                      band = 'Percent_Tree_Cover',
                      internal = TRUE,
                      start = "2001-05-01",
                      end = "2019-10-30")

write.csv(tree,file = 'viper_ak_vcf.csv',
          row.names = F)


# use the daymetr package to download all daymet params from 2001-2018 for all sites
# note the daymetr package uses a different webclient, and is much faster than the MODIStools
df_batch <- download_daymet_batch(file_location = 'site_coords.csv',
                                  start = 2001,
                                  end = 2018,
                                  internal = TRUE,
                                  silent = TRUE)

# create single data frame of daymet data
dm <- df_batch[[1]]$data

for(i in 2:length(df_batch))
{
  dm <- rbind(dm,df_batch[[i]]$data)
}

# write daymet to csv
write.csv(dm,file = 'viper_ak_daymet_2001_2018.csv',
          row.names = F)


