# 
# code: Calculate global protected area over time
# 
# author: Colleen Nell, collnellphd@gmail.com
# created: September 2018
# 
# 
# ---- inputs ----
#  1) World database on protected areas (WPDA) shapefiles (points and polygons). 
#     - Source: Accessed in August 2018 from www.protectedplanet.com. 
#     - Metadata: Manual: https://www.protectedplanet.net/c/wdpa-manual  
#     - Files: 1_Nov2018/2_FlatDataFiles/ConsDB_Input/WDPA_Aug2018-shapefile
#  2) Exclusive Economic Zones and terrestrial country boundaries. Hosted at https://datadryad.org/resource/doi:10.5061/dryad.6gb90.2.
#     - Source: UNEP-WCMC (2015). Dataset combining Exclusive Economic Zones (EEZ; VLIZ 2014) and terrestrial country boundaries (World Vector   
#         Shoreline, 3rd edition, National Geospatial-Intelligence Agency). Cambridge (UK): UNEP World Conservation Monitoring Centre.
#     - Dataset: Brooks TM, Akçakaya HR, Burgess ND, Butchart SHM, Hilton-Taylor C, Hoffmann M, Juffe-Bignoli D, Kingston N, MacSharry B, Parr M, 
#         Perianin L, Regan EC, Rodrigues ASL, Rondinini C, Shennan-Farpon Y, Young BE (2016) Data from: Analysing biodiversity and conservation 
#         knowledge products to support regional environmental assessments. Dryad Digital Repository. https://doi.org/10.5061/dryad.6gb90.2
#     - Metadata: https://datadryad.org/resource/doi:10.5061/dryad.6gb90.2/15.2?show=full  
#     - Files: 1_Nov2018/2_FlatDataFiles/ConsDB_Input/EEZ_WVS_layer/
# 
# ---- outputs ----
#  1) Shapefiles of protected area aggregated up until 1995 at subregional level
#  2) Shapefiles of protected area aggregated at year for 1996-2018 at subregional level
#  3) Marine, terrestrial, and global protected area from 1995-2018 (csv). 
#       - Annual protect area calcualted in M ha and as a percent of total EEZ area, land area, and global (not including ABNJ), respectively.  
# 
# ---- code sections ----
#
#  Steps follow WCMC-UNEP: https://www.protectedplanet.net/c/calculating-protected-area-coverage 
#  
#  1) Load libraries  
#  2) Clean WDPA data
#  3) Break up WDPA by ISO3
#  4) Intersect with global boundaries
#  5) 
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
library(beepr)

# set working directory to input data '/ConsDB_Input/'
setwd("/Users/collnell/Dropbox/ConsDB_Input")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Clean WDPA data  ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# polyogn data
wdpa.raw.polys <- st_read(dsn = 'WDPA_Aug2018-shapefile/', layer = 'WDPA_Aug2018-shapefile-polygons')

# point data
wdpa.raw.pts <- st_read(dsn = 'WDPA_Aug2018-shapefile/', layer ='WDPA_Aug2018-shapefile-points')

## Filter data 
# include protected areas with Status = designated, inscribed, and established
# include only points with a reported area
# exclude UNESCO Man and Biosphere Reserves

# Filter polygon data
polys.clean <- wdpa.raw.polys%>%
  filter(STATUS %in% c('Designated','Inscribed','Established'))%>%
  filter(DESIG_ENG != "UNESCO-MAB Biosphere Reserve")%>%
  st_transform('+proj=moll')

# Save
st_write(polys.clean,  dsn = 'WDPA/GLOBAL', layer = 'wdpa_polygon_clean.shp', driver='ESRI Shapefile')

# Filter point data
pts.clean <- wdpa.raw.pts%>%
  filter(REP_AREA > 0)%>%
  filter(STATUS %in% c('Designated','Inscribed','Established'))%>%
  filter(DESIG_ENG != "UNESCO-MAB Biosphere Reserve")%>%
  st_transform('+proj=moll')

## Buffer protected areas reported as points
# Use reported area to give circular geodesic point buffer
pts.buff <- st_buffer(pts.clean, dist = set_units(sqrt(pts.clean$REP_AREA/pi), 'km')) # radius for buffer in km2

# Save
st_write(pts.buff, 'WDPA/GLOBAL/wdpa_pts_clean.shp', driver='ESRI Shapefile')

## Combine polygons with buffered points 
#pts.buff<-st_read('WDPA/GLOBAL/wdpa_pts_clean.shp')
#polys.clean<-st_read('WDPA/GLOBAL/wdpa_polygon_clean.shp')

col.list<-intersect(colnames(pts.buff), colnames(polys.clean)) # match column names

wdpa.combo<-do.call(rbind, list(
  pts.buff%>%select(col.list), 
  polys.clean%>%select(col.list)))

# Save to use in other analyses (METT area, ICCAs)
st_write(wdpa.combo, 'WDPA/GLOBAL/wdpa_all_clean.shp', driver='ESRI Shapefile');beep('ping')

## clean working directory
rm(wdpa.raw.polys)
rm(wdpa.raw.pts)
rm(pts.clean)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Break up WDPA by ISO3  ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# wdpa.small<-st_read('WDPA/GLOBAL/wdpa_clean.shp')

## reduce size of polygon file
wdpa.small<-wdpa.combo%>%select(WDPAID, ISO3, STATUS_YR)
rm(wdpa.combo)

## split WDPA by ISO3
iso3<-unique(wdpa.small$ISO3)

for (i in 1:length(iso3)){ 
  # filter to sites in given ISO3, aggregate by WDPAID
  ISO3.shp<-wdpa.small%>%filter(ISO3 == iso3[i])%>%group_by(WDPAID, ISO3, STATUS_YR)%>%summarize();beep('ping')
  
  # write file for ISO3
  st_write(ISO3.shp,  paste0('WDPA/ISO3_ID/', iso3[i]), driver='ESRI Shapefile');beep('facebook')
}
rm(wdpa.small)

## aggregate at year
ISO3.list<-list.files('WDPA/ISO3_ID')

## For every ISO3, read in, filter, and aggregate by STATUS_YR
for (i in 1:length(ISO3.list)){
  ISO3.in<-st_read(paste0('WDPA/ISO3_ID/',ISO3.list[i]))
  year.list<-unique(ISO3.in$STATUS_YR)
  
  for (j in 1:length(year.list)){
    ISO3.year<-ISO3.in%>%
      filter(STATUS_YR == year.list[j])%>%
      st_buffer(0)%>%
      group_by(ISO3, STATUS_YR)%>%
      summarize()%>%
      st_buffer(0);beep('ping')
    st_write(ISO3.year, paste0('WDPA/ISO3_YEAR/',ISO3.list[i],'_', year.list[j]),delete_layer=TRUE, driver='ESRI Shapefile');beep('facebook')
  }
  
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: Intersect with global boundaries  ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Intersect each ISO3.year with subregional coastline for terrestrial, marine, ABNJ
ISO3.yr.list<-list.files('WDPA/ISO3_YEAR/')\
#trans<-read.csv('WDPA/wdpa_transboundary.csv')

## Break up WDPA by UN subregion
boundaries<-st_read('EEZ_WVS_layer/EEZv8_WVS_DIS_V3_ALL_final_v7disUNEP') # or EEZv8_WVS_DIS_V3_ALL_final_v7disIPBES
sub.list<-unique(boundaries$G_UNEP_sub)

EEZ<-boundaries%>%filter(type=='EEZ')%>%st_transform('+proj=moll')
Land<-boundaries%>%filter(type=='Land')%>%st_transform('+proj=moll')
ABNJ<-boundaries%>%filter(type=='ABNJ')%>%st_transform('+proj=moll')

# clean
rm(boundaries)

ISO3.yr.list<-list.files('WDPA/ISO3_YEAR/')
grep('AUS_2004', ISO3.yr.list)
#'AUS_2013'

## started at 4 pm
## intersect each ISO3_YEAR with EEZ boundaries
for (i in 306:length(ISO3.yr.list)){
  ISO3.yr.in <- st_read(paste0('WDPA/ISO3_YEAR/', ISO3.yr.list[i]))
  ISO3_EEZ<-st_intersection(ISO3.yr.in, EEZ)%>%st_buffer(0)
  subr.list<-unique(ISO3_EEZ$G_UNEP_sub)
  if (dim(ISO3_EEZ)[1] == 0){
  } else if (dim(ISO3_EEZ)[1] > 0 & length(subr.list == 1)){
    st_write(ISO3_EEZ, paste0('WDPA/ISO3_EEZ/EEZ_', ISO3.yr.list[i],'_', subr.list[1]), driver = 'ESRI Shapefile')
  } else if (dim(ISO3_EEZ)[1] > 0 & length(subr.list > 1)){
    for (k in 1:length(subr.list)){
      ISO3_EEZ<-ISO3_EEZ%>%filter(G_UNEP_sub == subr.list[k])%>%st_buffer(0)
      st_write(ISO3_EEZ, paste0('WDPA/ISO3_EEZ/EEZ_', ISO3.yr.list[i],'_', subr.list[k]), driver = 'ESRI Shapefile') 
    }
  }
}

## intersect each ISO3_YEAR with Land and ABJN - started at 4:30pm from AUS_2004
for (i in 297:length(ISO3.yr.list)){
  ISO3.yr.in <- st_read(paste0('WDPA/ISO3_YEAR/', ISO3.yr.list[i]))%>%st_buffer(0)
  ISO3_Land<-st_intersection(ISO3.yr.in, Land)%>%st_buffer(0)
  subr.list<-unique(ISO3_Land$G_UNEP_sub)
  if (dim(ISO3_Land)[1] == 0){
  } else if (dim(ISO3_Land)[1] > 0 & length(subr.list == 1)){
    st_write(ISO3_Land, paste0('WDPA/ISO3_Land/Land_', ISO3.yr.list[i],'_', subr.list[1]), driver = 'ESRI Shapefile', delete_leyer=TRUE)
  } else if (dim(ISO3_EEZ)[1] > 0 & length(subr.list > 1)){
    for (k in 1:length(subr.list)){
      ISO3_Land<-ISO3_Land%>%filter(G_UNEP_sub == subr.list[k])%>%st_buffer(0)
      st_write(ISO3_Land, paste0('WDPA/ISO3_Land/Land_', ISO3.yr.list[i],'_', subr.list[k]), driver = 'ESRI Shapefile', delete_leyer=TRUE)
    }
  }
  ISO3_EEZ<-st_intersection(ISO3.yr.in, EEZ)%>%st_buffer(0)
  subr.list<-unique(ISO3_EEZ$G_UNEP_sub)
  if (dim(ISO3_EEZ)[1] == 0){
  } else if (dim(ISO3_EEZ)[1] > 0 & length(subr.list == 1)){
    st_write(ISO3_EEZ, paste0('WDPA/ISO3_EEZ/EEZ_', ISO3.yr.list[i],'_', subr.list[1]), driver = 'ESRI Shapefile', delete_leyer=TRUE)
  } else if (dim(ISO3_EEZ)[1] > 0 & length(subr.list > 1)){
    for (k in 1:length(subr.list)){
      ISO3_EEZ<-ISO3_EEZ%>%filter(G_UNEP_sub == subr.list[k])%>%st_buffer(0)
      st_write(ISO3_EEZ, paste0('WDPA/ISO3_EEZ/EEZ_', ISO3.yr.list[i],'_', subr.list[k]), driver = 'ESRI Shapefile', delete_leyer=TRUE) 
    }
  }
  
  # ABNJ
  ISO3_ABNJ<-st_intersection(ISO3.yr.in, ABNJ)%>%st_buffer(0)
  subr.list<-unique(ISO3_ABNJ$G_UNEP_sub)
  if (dim(ISO3_ABNJ)[1] == 0){
  } else if (dim(ISO3_ABNJ)[1] > 0 & length(subr.list == 1)){
    st_write(ISO3_ABNJ, paste0('WDPA/ISO3_ABNJ/ABNJ_', ISO3.yr.list[i],'_', subr.list[1]), driver = 'ESRI Shapefile', delete_leyer=TRUE)
  } else if (dim(ISO3_ABNJ)[1] > 0 & length(subr.list > 1)){
    for (k in 1:length(subr.list)){
      ISO3_ABNJ<-ISO3_ABNJ%>%filter(G_UNEP_sub == subr.list[k])%>%st_buffer(0)
      st_write(ISO3_ABNJ, paste0('WDPA/ISO3_ABNJ/ABNJ_', ISO3.yr.list[i],'_', subr.list[k]), driver = 'ESRI Shapefile', delete_leyer=TRUE)
    }
  }
}



# for each subregion, find files with matching subregion
file.df<-data.frame(files = list.files('WDPA/ISO3_YEAR'))%>% # all files
  separate(file, '_', into=c('ISO3', 'YEAR'), remove=FALSE)%>%
  left_join(isos.in, by=c('ISO3'= 'ISO3_multi'))# match subregion

files.sub<-file.df%>%filter(GEO.sub.region )

year.subs.fast<-grep('Central and Western Europe', list.files('WDPA/year_subregion'), value=TRUE) # find list of wdpa files


## deal with transboundary sites
# all the ISO3s of PAs to intersect, some have multiple ISO3 due to transboundary sites
isos<-read.csv('ISO3_subregions.csv')%>%transform(ISO3=as.character(ISO3))

trans<-read.csv('WDPA/wdpa_transboundary.csv')
head(trans)
unique(trans$ISO3_multi)

    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #
    # ---- SECTION 5:   ----
  #
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ## Aggregate by WDPAID
  # combines areas that are parcels within larger site
  # do points and polygons separately to manage file sizes & processing time
  
  pts.id<-pts.buff%>%
    group_by(WDPAID, MARINE, ISO3, STATUS, STATUS_YR, IUCN_CAT, GOV_TYPE, DESIG_ENG)%>%
    summarize()
  
  
  #
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #
  # ---- SECTION 6:  ----
  #
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #
  
  
  
  