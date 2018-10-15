# 
# code: Calculate total area of protected areas within Key Biodiversity Areas
# 
# author: Colleen Nell, collnellphd@gmail.com
# created: October 2018
# 
# 
# ---- inputs ----
#  1) Key Biodiversity Areas
#     - Source: Downloaded under licence from the Integrated Biodiversity Assessment Tool. http://www.ibatforbusiness.org  
#     - Files: ConsDB_Input/KBAsGlobal_2018_01; contains layers "KbaMapGlobal_POL" and "KbaMapGlobal_PNT"  
#  2) Exclusive Economic Zones and terrestrial country boundaries. Hosted at https://datadryad.org/resource/doi:10.5061/dryad.6gb90.2.
#     - Source: UNEP-WCMC (2015). Dataset combining Exclusive Economic Zones (EEZ; VLIZ 2014) and terrestrial country boundaries (World Vector   
#         Shoreline, 3rd edition, National Geospatial-Intelligence Agency). Cambridge (UK): UNEP World Conservation Monitoring Centre.
#     - Dataset: Brooks TM, Akçakaya HR, Burgess ND, Butchart SHM, Hilton-Taylor C, Hoffmann M, Juffe-Bignoli D, Kingston N, MacSharry B, Parr M, 
#         Perianin L, Regan EC, Rodrigues ASL, Rondinini C, Shennan-Farpon Y, Young BE (2016) Data from: Analysing biodiversity and conservation 
#         knowledge products to support regional environmental assessments. Dryad Digital Repository. https://doi.org/10.5061/dryad.6gb90.2
#     - Metadata: https://datadryad.org/resource/doi:10.5061/dryad.6gb90.2/15.2?show=full  
#     - Files: 1_Nov2018/2_FlatDataFiles/ConsDB_Input/EEZ_WVS_layer/
#  3) World database on protected areas (WPDA) shapefiles aggregated by subregion and year, as produced by 'WDPA_calculate_PA'
#     - Source: Accessed in August 2018 from www.protectedplanet.com. 
#     - Manual: https://www.unep-wcmc.org/system/dataset_file_fields/files/000/000/432/original/Manual_FINAL_EN.pdf?1486562872  
#     - Files: 1_Nov2018/2_FlatDataFiles/ConsDB_Input/WDPA/Land_YEAR, EEZ_YEAR, and ABNJ_YEAR   
#  4) WDPA_timeseries.csv  
#     - produced by WDPA_calculate_PA script  
#
# ---- outputs ----
#  1) KBA_WDPA.shp - Polygon shapefiles of protected areas documented in WDPA that overlap with KBAs    
#  2) KBA_timeseries.csv - Data file with timeseries of WDPA coverage of KBAs. Protected area in KBAs given as M ha, and as a percent of total protected areas. 
# 
# ---- code sections ----
#
#  1) Load libraries  
#  2) Prepare KBA data
#  3) Intersect WDPA with KBAs
#  4)  
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
library(reshape2)

# set working directory to input data '/ConsDB_Input/'
setwd("/Users/colleennell/Dropbox/ConsDB_Input")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Prepare KBA data  ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# given the folder containing KBA point and polygon layers
# filters data, buffers point to polygons, agrregates by ISO3 & saves resulting shapefile

clean_KBA<-function(KBA.filepath){
  
  ## KBAs - points and polygon
  kba.pt<-st_read(KBA.filepath, layer='KbaMapGlobal_PNT')%>%
    select(SitRecID, Region, Country,ISO3, IntName, Latitude, Longitude, AreaReport)%>%
    filter(AreaReport > 0)%>%
    st_transform('+proj=moll')
  
  ## buffer points to polygons
  pts.buff<-st_buffer(kba.pt, dist=sqrt(kba.pt$AreaReport/pi)) # use radius to equivalent circular area
  
  ## polygons
  kba.poly<-st_read(KBA.filepath, layer='KbaMapGlobal_POL')%>%
    st_transform('+proj=moll')%>%
    select(SitRecID, Region, Country,ISO3, IntName, Latitude=SitLat, Longitude=SitLong, AreaReport=SitArea)%>%
    filter(AreaReport > 0)
  
  ## combine pts and polys, aggregate
  kba.both<-do.call(rbind, list(kba.pt, kba.poly))%>%
    st_buffer(0)%>%
    group_by(Country, ISO3)%>%
    summarize()%>%
    st_buffer(0)
  
  # save
  st_write(kba.both, 'KBA/KBA_poly_pts', driver='ESRI Shapefile', delete_layer=TRUE)
  
}

KBA.filepath<-'KBA/KBAsGlobal_2018_01/'
clean_KBA(KBA.filepath)

# Processing time: 25 min 

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Intersect WDPA with KBAs  ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## aggregate KBA at subregional level by type (Land, EEZ, ABNJ)
KBA.in<-st_read('KBA/KBA_poly_pts')

intersect_KBA<-function(KBA.in, type = 'UNEP'){
  # repair broken geometries
  KBA.in<-KBA.in%>%st_buffer(0)
  
  # Divide into marine, land and ABNJ boundaries
  EEZ<-st_read(paste0('EEZ_WVS_layer/EEZ_',type))%>%st_buffer(0)
  Land<-st_read(paste0('EEZ_WVS_layer/Land_',type))%>%st_buffer(0)
  ABNJ<-st_read(paste0('EEZ_WVS_layer/ABNJ_',type))%>%st_buffer(0)
  
  # intersect with global boundaries to delineate Land, EEZ, ABNJ, and associate KBAs with global subregions
  KBA.EEZ<-st_intersection(KBA.in, EEZ)%>%st_buffer(0)
  EEZ.sub<-KBA.EEZ%>%group_by(G_UNEP_sub)%>%summarize()%>%st_buffer(0)
  EEZ.sub.list<-unique(EEZ.sub$G_UNEP_sub)
  
  for (i in 1:length(EEZ.sub.list)){
    EEZ.filt<-EEZ.sub%>%filter(G_UNEP_sub == EEZ.sub.list[i])%>%st_buffer(0)
    st_write(KBA.EEZ, paste0('KBA/subregion/KBA_EEZ_',EEZ.sub.list[i]), driver='ESRI Shapefile', delete_layer=TRUE)
  }
  
  KBA.Land<-st_intersection(KBA.in, Land)%>%st_buffer(0)
  Land.sub<-KBA.Land%>%group_by(G_UNEP_sub)%>%summarize()%>%st_buffer(0)
  Land.sub.list<-unique(Land.sub$G_UNEP_sub)
  
  for (i in 1:length(Land.sub.list)){
    Land.filt<-Land.sub%>%filter(G_UNEP_sub == Land.sub.list[i])%>%st_buffer(0)
    st_write(KBA.Land, paste0('KBA/subregion/KBA_Land_',Land.sub.list[i]), driver='ESRI Shapefile', delete_layer=TRUE)
  }
  
  KBA.ABNJ<-st_intersection(KBA.in, ABNJ)%>%st_buffer(0)
  ABNJ.sub<-KBA.ABNJ%>%group_by(G_UNEP_sub)%>%summarize()%>%st_buffer(0)
  ABNJ.sub.list<-unique(ABNJ.sub$G_UNEP_sub)
  
  for (i in 1:length(ABNJ.sub.list)){
    ABNJ.filt<-ABNJ.sub%>%filter(G_UNEP_sub == ABNJ.sub.list[i])%>%st_buffer(0)
    st_write(KBA.ABNJ, paste0('KBA/subregion/KBA_ABNJ_',ABNJ.sub.list[i]), driver='ESRI Shapefile', delete_layer=TRUE)
  }

  
}
intersect_KBA(KBA.in) #started at 3 pm

# which files are missing?
# ABNJ & Land - Northern Africa, Southern Africa, Western Africa, Western Indian Ocaen, South Asia, South East Asia, South Pacific, Western Europe, South America, 

intersecting_KBA<-function(KBA.in, type = 'UNEP'){
  # repair broken geometries
  KBA.in<-KBA.in%>%st_buffer(0)

  Land<-st_read(paste0('EEZ_WVS_layer/Land_',type))%>%st_buffer(0)
  ABNJ<-st_read(paste0('EEZ_WVS_layer/ABNJ_',type))%>%st_buffer(0)

  KBA.Land<-st_intersection(KBA.in, Land)%>%st_buffer(0)
  Land.sub<-KBA.Land%>%group_by(G_UNEP_sub)%>%summarize()%>%st_buffer(0)
  Land.sub.list<-unique(Land.sub$G_UNEP_sub)
  
  for (i in 1:length(Land.sub.list)){
    Land.filt<-Land.sub%>%filter(G_UNEP_sub == Land.sub.list[i])%>%st_buffer(0)
    st_write(KBA.Land, paste0('KBA/subregion/KBA_Land_',Land.sub.list[i]), driver='ESRI Shapefile')
  }
  
  KBA.ABNJ<-st_intersection(KBA.in, ABNJ)%>%st_buffer(0)
  ABNJ.sub<-KBA.ABNJ%>%group_by(G_UNEP_sub)%>%summarize()%>%st_buffer(0)
  ABNJ.sub.list<-unique(ABNJ.sub$G_UNEP_sub)
  
  for (i in 1:length(ABNJ.sub.list)){
    ABNJ.filt<-ABNJ.sub%>%filter(G_UNEP_sub == ABNJ.sub.list[i])%>%st_buffer(0)
    st_write(KBA.ABNJ, paste0('KBA/subregion/KBA_ABNJ_',ABNJ.sub.list[i]), driver='ESRI Shapefile')
  }
  
  
}

intersecting_KBA(KBA.in)

#############
## for a given list of years, intersects protected areas with KBA boundaries
# calculates the total area of intersection & saves output shapefiles and csv of areas

year.list<-as.character(c(2000, 2005, 2006, 2007)) # modify list to desired years
year.list

intersect_KBA<-function(year.list){
  
  # filter KBA data to subregion
  kba.subregion<-st_read('WDPA/KBA_WDPA/KBA_subregion') # read in kba data aggregated at subregional level
  kba.sub.filt<-kba.subregion%>%filter(IPBES__ == 'Central and Western Europe')%>%st_buffer(0) # filter to CW Europe
  
  # find year_subregion WDPA files for intersection
  year.subs.fast<-grep('Central and Western Europe', list.files('WDPA/year_subregion'), value=TRUE) # find list of wdpa files for CW Euro yrs 1995:2018
  year.subs.df<-data.frame(files=year.subs.fast)%>%
    separate(files, '_', into=c('year','subregion'), remove=FALSE)%>%
    filter(subregion == 'Central and Western Europe', year %in% year.list)
  read.in<-year.subs.df$files # filenames for corresponding years to intersect
  
  # for each year_subregion, intersect with KBA and find overlapping area
  for (i in 1:length(year.list)){
    year_sub_in<-st_read(paste0('WDPA/year_subregion/', read.in[i]))%>%st_buffer(0) # read in each file
    subyr<-str_split(read.in[i],'_')[[1]][1]# extract year
    #intersect and fix geometries
    kba_intersect_wdpa<-st_intersection(kba.sub.filt, year_sub_in)%>%
      st_buffer(0)%>%
      mutate(PA_area_km2 = set_units(st_area(.),km^2)) # calculate area
    
    if (dim(kba_intersect_wdpa)[1] == 0){ 
      # if there is no intersecting area, do nothing
      
    }else{ # if there is
      # save intersected area data 
      df.out<-data.frame(subregion = kba_intersect_wdpa$IPBES_sub,
                         type = kba_intersect_wdpa$type,
                         PA_area_km2 = kba_intersect_wdpa$PA_area_km2,
                         year = subyr)
      # and files
      write.csv(df.out, paste0('WDPA/KBA_WDPA/area_time/', read.in[i], '.csv'), row.names=FALSE)
      st_write(kba_intersect_wdpa, paste0('WDPA/KBA_WDPA/area_time/', read.in[i]), delete_layer=TRUE, driver = 'ESRI Shapefile')
    }
  }
}

kba_cweuro(year.list)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: Calculate total protected area in KBA  ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## read each file in and calculate total area by year
kba.csv<-grep('.csv', list.files('WDPA/KBA_WDPA/area_time'), value=TRUE)
all_area<-do.call(rbind, lapply(c(kba.csv), function(x) read.csv(paste0('WDPA/KBA_WDPA/area_time/', x))))

## total for each year
area.yr<-all_area%>%group_by(year, type)%>%summarize(KBA_KM2 = sum(PA_area_km2, na.rm=TRUE))%>%
  mutate(KBA_HA = set_units(set_units(KBA_KM2, 'km^2'), 'ha'), KBA_MHA = as.numeric(KBA_HA)/1000000)
head(area.yr)

area.long<-area.yr%>%melt(id.vars=c('year','type'), variable.name='UNIT', value.name='AREA')
head(area.long)

area.wide<-area.long%>%dcast(year~type+UNIT, value.var='AREA')%>%
  mutate(TOTAL_KBA_MHA = EEZ_KBA_MHA+Land_KBA_MHA)%>%
  mutate_at(vars(EEZ_KBA_MHA, Land_KBA_MHA, TOTAL_KBA_MHA), funs(TIME = cumsum(.))) # cumulative area over time
head(area.wide)

## join with global PA data to calculate proportion of total
wdpa<-read.csv('WDPA_timeseries.csv')

## Indicator: Percent of total protected area that is within Key Biodiversity Areas (M ha PAs within KBAs / M ha PAs)
kba.prop<-area.wide%>%
  left_join(wdpa, by='year')%>%
  mutate(PA_in_KBA_percent = 100*(TOTAL_KBA_MHA_TIME/Total_PA_Mha_time))

#PA_in_KBA_percent
write.csv(kba.prop, 'KBA_timeseries.csv', row.names=FALSE)
