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
#  5) Aggregate by geographic subregion
#  6) Calculate protected area coverage  
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
# ---- SECTION 2: Clean WDPA data  ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# given the monthly release folder containing point and polygon files from www.protectedplanet.net
# filters data to sites that meet definition of protected area
# transforms projection to Molldweide
# buffers points with reported area to create polygons
# combines to single feature
# and breaks up global boundary files into marine, terrestrial and ANBJ

## inputs
# filepath for folder of WDPA monthly release, contains point and polygon files
# type = type of subregion as either 'UNEP' or 'IPBES' for global boundaries

## outputs
# WDPA/GLOBAL/wdpa_all_clean.shp - combined shapefile with cleaned WDPA sites, ready for processing
# EEZ_WVS_layer/EEZ - global EEZ shapefile
# EEZ_WVS_layer/ABNJ - ABNJ shapefile
# EEZ_WVS_layer/Land - Land boundaries shapefile

clean_wdpa<-function('WDPA_Aug2018-shapefile/', type = 'UNEP'){
  ## read in WDPA data
  # polygon data
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
  
  # Filter point data
  pts.clean <- wdpa.raw.pts%>%
    filter(REP_AREA > 0)%>%
    filter(STATUS %in% c('Designated','Inscribed','Established'))%>%
    filter(DESIG_ENG != "UNESCO-MAB Biosphere Reserve")%>%
    st_transform('+proj=moll')
  
  ## Buffer protected areas reported as points
  # Use reported area to give circular geodesic point buffer
  pts.buff <- st_buffer(pts.clean, dist = set_units(sqrt(pts.clean$REP_AREA/pi), 'km')) # radius for buffer in km2
  
  col.list<-intersect(colnames(pts.buff), colnames(polys.clean)) # match column names
  
  wdpa.combo<-do.call(rbind, list(
    pts.buff%>%select(col.list), 
    polys.clean%>%select(col.list)))
  
  # Save to use in other analyses (METT area, ICCAs)
  st_write(wdpa.combo, 'WDPA/GLOBAL/wdpa_all_clean.shp', driver='ESRI Shapefile')
  
  ## clean working directory
  rm(wdpa.raw.polys)
  rm(wdpa.raw.pts)
  rm(pts.clean)
  
  ## chop up global boundaries into marine, terrestrial, and abnj
  boundary_type<-function(type = 'UNEP'){
    if (type == 'UNEP'){
      boundaries<-st_read('EEZ_WVS_layer/EEZv8_WVS_DIS_V3_ALL_final_v7disUNEP')
    } else if (type == 'IPBES'){
      boundaries<-st_read('EEZ_WVS_layer/EEZv8_WVS_DIS_V3_ALL_final_v7disIPBES')
    }
    
    EEZ<-boundaries%>%filter(type=='EEZ')%>%st_transform('+proj=moll')%>%st_buffer(0)
    st_write(EEZ, paste0('EEZ_WVS_layer/EEZ_',type), driver='ESRI Shapefile')
    
    Land<-boundaries%>%filter(type=='Land')%>%st_transform('+proj=moll')%>%st_buffer(0)
    st_write(Land, paste0('EEZ_WVS_layer/Land_',type), driver='ESRI Shapefile')
    
    ABNJ<-boundaries%>%filter(type=='ABNJ')%>%st_transform('+proj=moll')%>%st_buffer(0)
    st_write(ABNJ, paste0('EEZ_WVS_layer/ABNJ_',type), driver='ESRI Shapefile')
    
    # clean up
    rm(boundaries)
    
  }
  
  boundary_type('UNEP')
  
}

# run
clean_wdpa('WDPA_Aug2018-shapefile/', type = 'UNEP')

# Processing time = ~2 hours

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Break up WDPA by ISO3  ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## give the combined & cleaned point and polygon files from the WDPA
# reduces object size, splits up by ISO3 code, aggregates within ISO3 by year
# output: shapefiles for each ISO3 and year combo

## inputs
# wdpa.combo - the spatial file of WDPA sites produced in step 2 as 'WDPA/GLOBAL/wdpa_all_clean.shp'

## outputs
# WDPA/ISO3_YEAR/ - folder is populated with files corresponding to each unique ISO3 code, aggregated at year from 0-present
breakup_wdpa<-function(wdpa.combo){
  ## reduce size of polygon file
  wdpa.small<-wdpa.combo%>%select(WDPAID, ISO3, STATUS_YR)
  rm(wdpa.combo)
  
  ## split WDPA by ISO3
  iso3<-unique(wdpa.small$ISO3)
  
  for (i in 1:length(iso3)){ 
    # filter to sites in given ISO3, aggregate by WDPAID
    ISO3.shp<-wdpa.small%>%filter(ISO3 == iso3[i])%>%group_by(WDPAID, ISO3, STATUS_YR)%>%summarize()
    
    # write file for ISO3
    st_write(ISO3.shp,  paste0('WDPA/ISO3_ID/', iso3[i]), driver='ESRI Shapefile', delete_layer=TRUE)
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
      st_write(ISO3.year, paste0('WDPA/ISO3_YEAR/',ISO3.list[i],'_', year.list[j]), delete_layer=TRUE, driver='ESRI Shapefile')
    }
    
  }
  
}

# run 
breakup_wdpa(wdpa.combo)

# Processing time = ~5 hours

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: Intersect with global boundaries  ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Intersect each ISO3.year with subregional coastline for terrestrial, marine, ABNJ
ISO3.yr.list<-list.files('WDPA/ISO3_YEAR/')
grep('USA_1935', ISO3.yr.list)
ISO3.yr.list[933]#`CAN_1957' and USA 1935 starting points


# given a list of shapefiles (ISO3.yr.list) and which ones to process
# intersects with terrestrial, marine, and ABNJ boundaries and saves as separate files
intersect_wdpa<-function(ISO3.yr.list, start = 1, stop = length(ISO3.yr.list)){
  
  # Divide into marine, land and ABNJ boundaries
  EEZ<-st_read('EEZ_WVS_layer/EEZ_UNEP')
  Land<-st_read('EEZ_WVS_layer/Land_UNEP')
  ABNJ<-st_read('EEZ_WVS_layer/ABNJ_UNEP')
  
  ## intersect each ISO3_YEAR with boundaries
  # create separate files for each piece for faster processing time
  for (i in start:stop){
    #LAND
    ISO3.yr.in <- st_read(paste0('WDPA/ISO3_YEAR/', ISO3.yr.list[i]))%>%st_buffer(0)
    ISO3_Land<-st_intersection(ISO3.yr.in, Land)%>%st_buffer(0)
    subr.list<-unique(ISO3_Land$G_UNEP_sub)
    
    if (dim(ISO3_Land)[1] == 0){
    } else if (dim(ISO3_Land)[1] > 0 & length(subr.list == 1)){
      st_write(ISO3_Land, paste0('WDPA/ISO3_Land/Land_', ISO3.yr.list[i],'_', subr.list[1]), driver = 'ESRI Shapefile', delete_layer=TRUE)
    } else if (dim(ISO3_EEZ)[1] > 0 & length(subr.list > 1)){
      for (k in 1:length(subr.list)){
        ISO3_Land<-ISO3_Land%>%filter(G_UNEP_sub == subr.list[k])%>%st_buffer(0)
        st_write(ISO3_Land, paste0('WDPA/ISO3_Land/Land_', ISO3.yr.list[i],'_', subr.list[k]), driver = 'ESRI Shapefile', delete_layer=TRUE)
      }
    }#Land end
    
    #EEZ
    ISO3_EEZ<-st_intersection(ISO3.yr.in, EEZ)%>%st_buffer(0)
    subr.list<-unique(ISO3_EEZ$G_UNEP_sub)
    
    if (dim(ISO3_EEZ)[1] == 0){
    } else if (dim(ISO3_EEZ)[1] > 0 & length(subr.list == 1)){
      st_write(ISO3_EEZ, paste0('WDPA/ISO3_EEZ/EEZ_', ISO3.yr.list[i],'_', subr.list[1]), driver = 'ESRI Shapefile', delete_layer=TRUE)
    } else if (dim(ISO3_EEZ)[1] > 0 & length(subr.list > 1)){
      for (k in 1:length(subr.list)){
        ISO3_EEZ<-ISO3_EEZ%>%filter(G_UNEP_sub == subr.list[k])%>%st_buffer(0)
        st_write(ISO3_EEZ, paste0('WDPA/ISO3_EEZ/EEZ_', ISO3.yr.list[i],'_', subr.list[k]), driver = 'ESRI Shapefile', delete_layer=TRUE) 
      }
    }#EEZ end
    
    # ABNJ
    ISO3_ABNJ<-st_intersection(ISO3.yr.in, ABNJ)%>%st_buffer(0)
    subr.list<-unique(ISO3_ABNJ$G_UNEP_sub)
    
    if (dim(ISO3_ABNJ)[1] == 0){
    } else if (dim(ISO3_ABNJ)[1] > 0 & length(subr.list == 1)){
      st_write(ISO3_ABNJ, paste0('WDPA/ISO3_ABNJ/ABNJ_', ISO3.yr.list[i],'_', subr.list[1]), driver = 'ESRI Shapefile', delete_layer=TRUE)
    } else if (dim(ISO3_ABNJ)[1] > 0 & length(subr.list > 1)){
      for (k in 1:length(subr.list)){
        ISO3_ABNJ<-ISO3_ABNJ%>%filter(G_UNEP_sub == subr.list[k])%>%st_buffer(0)
        st_write(ISO3_ABNJ, paste0('WDPA/ISO3_ABNJ/ABNJ_', ISO3.yr.list[i],'_', subr.list[k]), driver = 'ESRI Shapefile', delete_layer=TRUE)
      }
    } #ABNJ end
  } # close loop
} # end function

# run
intersect_wdpa(ISO3.yr.list, start=grep('USA_1935', ISO3.yr.list), stop=length(ISO3.yr.list)) # started with USA_1935 at 10 pm

# Processing time: 2 days

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++x
#
# ---- SECTION 5: Aggregate by geographic subregion  ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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
# ---- SECTION 6: Calculate protected area coverage  ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


abnj_area<-set_units(st_area(ABNJ), ha)
sum(abnj_area, na.rm=TRUE)/1000000 # M ha = 22115.2

land_area<-set_units(st_area(Land), ha)
sum(land_area, na.rm=TRUE)/1000000 # M ha = 14886.11

eez_area<-set_units(st_area(EEZ), ha)
sum(eez_area, na.rm=TRUE)/1000000 # M ha = 14117.87
