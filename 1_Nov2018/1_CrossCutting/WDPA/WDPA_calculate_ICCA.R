# 
# code: Calculate total area of Indigenous people's and community conserved territories and areas (ICCAs) 
# 
# author: Colleen Nell, collnellphd@gmail.com
# created: October 2018
# 
# 
# ---- inputs ----
#  1) World database on protected areas (WPDA) shapefiles (points and polygons). 
#     - Source: Accessed in August 2018 from www.protectedplanet.com. 
#     - Manual: https://www.unep-wcmc.org/system/dataset_file_fields/files/000/000/432/original/Manual_FINAL_EN.pdf?1486562872  
#     - Files: 1_Nov2018/2_FlatDataFiles/ConsDB_Input/WDPA_Aug2018-shapefile  
#     - Notes: This is an incomplete database of ICCAs. The WDPA contained sites that are recognized by the IUCN definition of protected area, which does not always encapsulate ICCAs. ICCAs that are not government recognized or whose locations are sensitive may not be included in the WDPA. Moreover, whether information on ICCAs is stored in the WDPA is the decision of the local community or indigenous people.  
#
# ---- outputs ----
#  1) ICCA_by_year.shp - Polygon shapefile of state recognized ICCA sites documented in WDPA aggregated by year designated    
#  2) ICCA_timeseries.csv - Data file with total coverage of ICCAs over time in M ha and as a percent of the estimated global extent of ICCAs (400 - 800 M ha)    
# 
# ---- code sections ----
#
#  1) Load libraries  
#  2) Clean WDPA data and filter to ICCAs
#  3) Aggregate ICCA areas
#  4) Calculate total ICCA area  
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: Load libraries ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


library(sf)
library(units)
library(dplyr)
library(stringr)
library(tidyr)

# set working directory to input data '/ConsDB_Input/'
setwd("/Users/colleennell/Dropbox/ConsDB_Input")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Clean WDPA data and filter to ICCAs  ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# polygon data
wdpa.raw.polys <- st_read(dsn = 'WDPA_Aug2018-shapefile/', layer = 'WDPA_Aug2018-shapefile-polygons')

# point data
wdpa.raw.pts <- st_read(dsn = 'WDPA_Aug2018-shapefile/', layer ='WDPA_Aug2018-shapefile-points')

# Filter to ICCAs
ICCA.polys<-wdpa.raw.polys%>%
  filter(GOV_TYPE %in% c('Indigenous peoples', 'Local communities'))%>%
  select(WDPAID, NAME, DESIG_ENG, STATUS_YR, ISO3, REP_AREA)%>%
  st_transform('+proj=moll')

ICCA.pts<-wdpa.raw.pts%>%
  filter(GOV_TYPE %in% c('Indigenous peoples', 'Local communities'))%>%
  filter(REP_AREA > 0)%>%
  select(WDPAID, NAME, DESIG_ENG, STATUS_YR, ISO3, REP_AREA)%>%
  st_transform('+proj=moll')

## Buffer protected areas reported as points
# Use reported area to give circular geodesic point buffer
ICCA.pts.buff <- st_buffer(ICCA.pts, dist = set_units(sqrt(ICCA.pts$REP_AREA/pi), 'km')) # radius for buffer in km2

# clean environment
rm(wdpa.raw.polys)
rm(wdpa.raw.pts)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Aggregate ICCA areas  ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# combine into single feature
ICCA.combine<-do.call(rbind, list(ICCA.pts.buff, ICCA.polys))%>%st_buffer(0)

# Spatially aggregate each year - eliminates overlapping areas
ICCA.year<-ICCA.combine%>%group_by(STATUS_YR)%>%summarize()%>%st_buffer(0)

# save shapefile - ICCAs aggregated by year 
st_write(ICCA.year, 'ICCA_by_year', driver='ESRI Shapefile', delete_layer=TRUE)

# clean up
rm(ICCA.combine)
rm(ICCA.pts.buff)
rm(ICCA.polys)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: Calculate total ICCA area  ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ICCA.area<-ICCA.year%>%mutate(AREA_YR = set_units(st_area(.), 'ha'))

# calculate coverage in M ha and as a percent of estimated global extent of ICCAs
ICCA.df<-ICCA.area%>%
  st_set_geometry(NULL)%>%
  transform(AREA_HA=as.numeric(AREA_YR))%>%
  mutate(AREA_HA_CUM = cumsum(AREA_HA), AREA_MHA_TIME = AREA_HA_CUM/1000000)%>%
  mutate(AREA_EST_MHA_MIN = 400, AREA_EST_MHA_MAX = 800)%>%
  mutate(ICCA_PERCENT_EST = 100*(AREA_MHA_TIME/((AREA_EST_MHA_MIN+AREA_EST_MHA_MIN)/2)), 
         ICCA_PERCENT_LOW = 100*(AREA_MHA_TIME/AREA_EST_MHA_MAX), ICCA_PERCENT_HI = 100*(AREA_MHA_TIME/AREA_EST_MHA_MIN))

write.csv(ICCA.df, 'ICCA_timeseries.csv', row.names=FALSE)

# clean up
rm(ICCA.year)
rm(ICCA.area)
rm(ICCA.df)
