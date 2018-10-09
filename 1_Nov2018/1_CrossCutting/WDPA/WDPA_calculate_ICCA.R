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
#     - Metadata: Manual: https://www.protectedplanet.net/c/wdpa-manual  
#     - Files: 1_Nov2018/2_FlatDataFiles/ConsDB_Input/WDPA_Aug2018-shapefile
#
# ---- outputs ----
#  1) ICCA_by_year.shp - Polygon shapefile of state recognized ICCA sites documented in WDPA aggregated by year designated    
#  2) ICCA_timeseries.csv - Data file with total coverage of ICCAs over time in M ha and as a percent of the estimated global extent of ICCAs (400 - 800 M ha)    
# 
# ---- code sections ----
#
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
#

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
  select(WDPAID, NAME, DESIG_ENG, STATUS_YR, ISO3)%>%
  st_transform('+proj=moll')

ICCA.pts<-pas.pts%>%
  filter(GOV_TYPE %in% c('Indigenous peoples', 'Local communities'))%>%
  filter(REP_AREA > 0)%>%
  select(WDPAID, NAME, DESIG_ENG, STATUS_YR, ISO3, REP_AREA)%>%
  st_transform('+proj=moll')

## Buffer protected areas reported as points
# Use reported area to give circular geodesic point buffer
ICCA.pts.buff <- st_buffer(ICCA.pts, dist = set_units(sqrt(ICCA.pts$REP_AREA/pi), 'km')) # radius for buffer in km2


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Aggregate ICCA areas  ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# combine into single feature
ICCA.combine<-do.call(rbind, list(ICCA.pts.buff, ICCA.polys))

# Spatially aggregate each year
ICCA.year<-ICCA.combine%>%group_by(STATUS_YR)%>%summarize()%>%st_buffer(0)

# save shapefile - ICCAs aggregated by year 
st_write(ICCA.year, 'ICCA_by_year', driver='ESRI Shapefile', delete_layer=TRUE)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: Calculate total ICCA area  ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ICCA.area<-ICCA.year%>%mutate(AREA_YR = set_units(st_area(.), 'ha'))

# calculate coverage in M ha and as a percent of estimated global extent of ICCAs
ICCA.df<-icca.area%>%
  st_set_geometry(NULL)%>%
  transform(AREA_HA=as.numeric(AREA_YR))%>%
  mutate(AREA_HA_CUM = cumsum(AREA_HA), AREA_MHA_TIME = AREA_HA_CUM/1000000)%>%
  mutate(AREA_EST_HA_MIN = 400000000, AREA_EST_HA_MAX = 800000000,
         ICCA_PERCENT_EST_MIN = 100*(AREA_HA_CUM/AREA_EST_HA_MIN), ICCA_PERCENT_EST_MAX = 100*(AREA_HA_CUM/AREA_EST_HA_MAX))
str(ICCA.df)

write.csv(ICCA.df, 'ICCA_timeseries.csv', row.names=FALSE)

