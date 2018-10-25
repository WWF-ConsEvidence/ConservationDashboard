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
# WDPA_EEZ_area.csv - timeline of global protected area coverage in Exclusive Economic Zones by subregion
# WDPA_Land_area.csv - timeline of global terrestrial protected area coverage by subregion
# WDPA_ABNJ_area.csv - timeline of global protected area coverage in Areas Beyond National Jurisdiction by subregion
# WDPA_timeseries.csv - collective timeline of protected area coverage in M ha and as percent for terrestrial, marine (EEZ) and ABNJ zones.

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
    
    ## find total area of regions in M ha
    abnj_area<-set_units(st_area(ABNJ), ha)
    sum(abnj_area, na.rm=TRUE)/1000000 # M ha = 22115.2
    
    land_area<-set_units(st_area(Land), ha)
    sum(land_area, na.rm=TRUE)/1000000 # M ha = 14886.11
    
    eez_area<-set_units(st_area(EEZ), ha)
    sum(eez_area, na.rm=TRUE)/1000000 # M ha = 14117.87
    
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
    year.list<-unique(ISO3.shp$STATUS_YR)
    
    for (j in 1:length(year.list)){
      
      ## filter, and aggregate by STATUS_YR
      ISO3.year<-ISO3.shp%>%
        filter(STATUS_YR == year.list[j])%>%
        st_buffer(0)%>%
        group_by(ISO3, STATUS_YR)%>%
        summarize()%>%
        st_buffer(0);beep('ping')
      st_write(ISO3.year, paste0('WDPA/ISO3_YEAR/',iso3[i],'_', year.list[j]), delete_layer=TRUE, driver='ESRI Shapefile')
    }
  }
  rm(wdpa.small)
}

# run 
breakup_wdpa(wdpa.combo)

# Processing time = ~4 hours


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
  
  for (i in start:stop){
    
    ## intersect each ISO3_YEAR with boundaries
    # create separate files for each piece for faster processing time
    
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
    }
    
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
    }
    
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
    }
  } 
} 

# run
intersect_wdpa(ISO3.yr.list, start=grep('USA_1935', ISO3.yr.list), stop=length(ISO3.yr.list)) #

# Processing time: 2-3 days

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++x
#
# ---- SECTION 5: Aggregate by geographic subregion  ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## inputs: list of file names for land, eez, and abnj
Land.files<-list.files('WDPA/ISO3_Land/') # land files
EEZ.files<-list.files('WDPA/ISO3_EEZ/') # marine files
ABNJ.files<-list.files('WDPA/ISO3_ABNJ/') # abnj files

aggregate_WDPA<-function(EEZ.files, Land.files, ABNJ.files){
  
  
  ##LAND
  ## read in file names by subregion
  Land.subs<-unique(str_split(Land.files, '_', simplify=TRUE)[,4]) # subregion names
  Land.subs[4]
  
  out.df<-NULL
  
  for (k in 1:length(Land.subs)){
    Land.sub.files<-grep(Land.subs[k], Land.files, value=TRUE, fixed=TRUE) # subregion files
    Land.sub.years<-unique(str_split(Land.sub.files, '_', simplify=TRUE)[,3]) # subregion years
    
    for (m in 1:length(Land.sub.years)){
      # select files for given year
      year.files<-grep(paste0('_',Land.sub.years[m],'_'), Land.sub.files, value=TRUE)
      
      # read all subregion files in by year & aggregate
      Land.in<-do.call(rbind, lapply(c(year.files), function(x) st_read(paste0('WDPA/ISO3_Land/', x))))%>%
        group_by(G_UNEP_sub, STATUS_YR)%>%
        summarize()%>%
        st_buffer(0)
      sub.df<-data.frame(subregion = Land.in$G_UNEP_sub,
                         type = 'Land',
                         year = Land.in$STATUS_YR,
                         AREA_HA = set_units(st_area(Land.in), ha),
                         AREA_KM2 = set_units(st_area(Land.in), km2),
                         AREA_MHA = set_units(st_area(Land.in), ha)/1000000)
      
      st_write(Land.in, paste0('WDPA/SUB_YEAR/Land/', Land.sub.years[m],'_', Land.subs[k]), driver='ESRI Shapefile', delete_layer=TRUE)
      out.df<-rbind(sub.df, out.df)
    }
  }
  write.csv(out.df, 'WDPA_Land_area.csv', row.names=FALSE)
  
  ## read in file names by subregion
  EEZ.subs<-unique(str_split(EEZ.files, '_', simplify=TRUE)[,4]) # subregion names
  EEZ.subs[9]
  
  out.df<-NULL
  
  for (k in 10:length(EEZ.subs)){
    EEZ.sub.files<-grep(EEZ.subs[k], EEZ.files, value=TRUE, fixed=TRUE) # subregion files
    EEZ.sub.years<-unique(str_split(EEZ.sub.files, '_', simplify=TRUE)[,3]) # subregion years
    
    for (m in 1:length(EEZ.sub.years)){
      # select files for given year
      year.files<-grep(paste0('_',EEZ.sub.years[m],'_'), EEZ.sub.files, value=TRUE)
      
      # read all subregion files in by year & aggregate
      EEZ.in<-do.call(rbind, lapply(c(year.files), function(x) st_read(paste0('WDPA/ISO3_EEZ/', x))))%>%
        group_by(G_UNEP_sub, STATUS_YR)%>%
        summarize()%>%
        st_buffer(0)
      sub.df<-data.frame(subregion = EEZ.in$G_UNEP_sub,
                         type = 'EEZ',
                         year = EEZ.in$STATUS_YR,
                         AREA_HA = set_units(st_area(EEZ.in), ha),
                         AREA_KM2 = set_units(st_area(EEZ.in), km2),
                         AREA_MHA = set_units(st_area(EEZ.in), ha)/1000000)
      
      st_write(EEZ.in, paste0('WDPA/SUB_YEAR/EEZ/', EEZ.sub.years[m],'_', EEZ.subs[k]), driver='ESRI Shapefile', delete_layer=TRUE)
      out.df<-rbind(sub.df, out.df)
    }
  }
  
  write.csv(out.df, 'WDPA_EEZ_area.csv', row.names=FALSE)
  
  ## ABNJ
  ## read in file names by subregion
  ABNJ.subs<-unique(str_split(ABNJ.files, '_', simplify=TRUE)[,4]) # subregion names
  
  out.df<-NULL
  
  for (k in 1:length(ABNJ.subs)){
    ABNJ.sub.files<-grep(ABNJ.subs[k], ABNJ.files, value=TRUE, fixed=TRUE) # subregion files
    ABNJ.sub.years<-unique(str_split(ABNJ.sub.files, '_', simplify=TRUE)[,3]) # subregion years
    
    for (m in 1:length(ABNJ.sub.years)){
      # select files for given year
      year.files<-grep(paste0('_',ABNJ.sub.years[m],'_'), ABNJ.sub.files, value=TRUE)
      
      # read all subregion files in by year & aggregate
      ABNJ.in<-do.call(rbind, lapply(c(year.files), function(x) st_read(paste0('WDPA/ISO3_ABNJ/', x))))%>%
        group_by(G_UNEP_sub, STATUS_YR)%>%
        summarize()%>%
        st_buffer(0)
      sub.df<-data.frame(subregion = ABNJ.in$G_UNEP_sub,
                         type = 'ABNJ',
                         year = ABNJ.in$STATUS_YR,
                         AREA_HA = set_units(st_area(ABNJ.in), ha),
                         AREA_KM2 = set_units(st_area(ABNJ.in), km2),
                         AREA_MHA = set_units(st_area(ABNJ.in), ha)/1000000)
      
      st_write(ABNJ.in, paste0('WDPA/SUB_YEAR/ABNJ/', ABNJ.sub.years[m],'_', ABNJ.subs[k]), driver='ESRI Shapefile', delete_layer=TRUE)
      out.df<-rbind(sub.df, out.df)
    }
  }
  write.csv(out.df, 'WDPA_ABNJ_area.csv', row.names=FALSE)
  
}

## outputs
# shapefiles for each year x subregion under terrestrial, marine and ABNJ delineations
# csv - area calculations for each year x subregion; separate for land, marine and ABNJ

## Processing time: 6 hours

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 6: Calculate protected area coverage  ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Land<-read.csv('WDPA_Land_area.csv')
Land[is.na(Land)]<-0
str(Land)

Land.yr<-Land%>%
  group_by(type, year)%>%
  summarize_at(vars(AREA_HA:AREA_MHA), funs(sum))%>%
  mutate_at(vars(AREA_HA_sum:AREA_MHA_sum), funs(TIME = cumsum))%>%
  mutate(Land_area = 14886.11, Land_percent = 100*(AREA_MHA_sum_TIME/Land_area))

EEZ<-read.csv('WDPA_EEZ_area.csv')
EEZ[is.na(EEZ)]<-0
str(EEZ)

EEZ.yr<-EEZ%>%
  group_by(type, year)%>%
  summarize_at(vars(AREA_HA:AREA_MHA), funs(sum))%>%
  mutate_at(vars(AREA_HA_sum:AREA_MHA_sum), funs(TIME = cumsum))%>%
  mutate(EEZ_area = 14117.87, EEZ_percent = 100*(AREA_MHA_sum_TIME/EEZ_area))


ABNJ<-read.csv('WDPA_ABNJ_area.csv')
ABNJ[is.na(ABNJ)]<-0
str(ABNJ)

ABNJ.yr<-ABNJ%>%
  group_by(type, year)%>%
  summarize_at(vars(AREA_HA:AREA_MHA), funs(sum))%>%
  mutate_at(vars(AREA_HA_sum:AREA_MHA_sum), funs(TIME = cumsum))%>%
  mutate(ABNJ_area = 122115.2, ABNJ_percent = 100*(AREA_MHA_sum_TIME/ABNJ_area))

## combine all
Land.yr%>%select(type, year, area = Land_area, Mha = AREA_MHA_sum_TIME, percent = Land_percent)
EEZ.yr%>%select(type, year, area = EEZ_area, Mha = AREA_MHA_sum_TIME, percent = EEZ_percent)
ABNJ.yr%>%select(type, year, area = ABNJ_area, Mha = AREA_MHA_sum_TIME, percent = ABNJ_percent) 

