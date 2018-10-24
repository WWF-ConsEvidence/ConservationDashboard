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
#  3) Intersect KBAs with global boundaries
#  4) Intersect KBAs with WDPA 
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
setwd("/Users/collnell/Dropbox/ConsDB_Input")

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
# ---- SECTION 3: Intersect KBAs with global boundaries  ----
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
intersect_KBA(KBA.in) 

# Processing time: 5 hours

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: Intersect KBAs with WDPA   ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## for each year in the WDPA, intersect with KBAs to determine the total area of WDPAs in KBA
# done at the subregional level

## outputs shapefiles

KBA.files<-list.files('KBA/subregion/') # KBA files - intersect each year or same subregion
EEZ.KBA.files<-grep('EEZ',KBA.files, value=TRUE)
subregions<-gsub('KBA_EEZ_', '', EEZ.KBA.files)
subregions
subregions[14]

#df.out<-NULL
for (i in 14:length(subregions)){
  KBA.in<-st_read(paste0('KBA/subregion/',grep(subregions[i], EEZ.KBA.files, value=TRUE, fixed=TRUE)))%>%st_buffer(0) # EEZ KBA file
  KBA.sub<-KBA.in%>%
    filter(G_UNEP_sub == subregions[i])%>%
    st_buffer(0)%>%
    group_by(type, G_UNEP_sub)%>%
    summarize()%>%
    st_buffer(0)
  
  WDPA.sub.yrs<-grep(subregions[i],list.files('WDPA/SUB_YEAR/EEZ'), value=TRUE)
  
  for (j in 1:length(WDPA.sub.yrs)){
    # read in
    WDPA.in<-st_read(paste0('WDPA/SUB_YEAR/EEZ/',WDPA.sub.yrs[j]))%>%
      group_by(G_UNEP_sub, STATUS_YR)%>%
      summarize()%>%
      st_buffer(0)
    
    # intersect
    WDPA.in.KBA<-st_intersection(WDPA.in, KBA.sub)%>%st_buffer(0)
    
    if(dim(WDPA.in.KBA)[1]!= 0){
      temp.df<-data.frame(subregion = subregions[i],
                          type = 'EEZ',
                          year = str_split(WDPA.sub.yrs[j], '_', simplify=TRUE)[,1],
                          AREA_KM2 = set_units(st_area(WDPA.in.KBA), km^2))%>%
        mutate(AREA_HA = set_units(AREA_KM2, ha))
      df.out<-rbind(df.out, temp.df)
      st_write(WDPA.in.KBA, paste0('KBA/intersect/',WDPA.sub.yrs[j]), driver = 'ESRI Shapefile', delete_layer=TRUE)
    }else{}
    
  }
} 
# stopped at 0_North America
subregions[i]
tail(df.out)
str(df.out)
#write.csv(df.out, 'KBA/KBA_EEZ_year.csv', row.names=FALSE) # saved 1:4
#write.csv(df.out, 'KBA/KBA_EEZ_year_1_13.csv', row.names=FALSE)
write.csv(df.out, 'KBA/KBA_EEZ_year_1_1.csv', row.names=FALSE)

# troubleshooting
i<-14
KBA.in<-st_read(paste0('KBA/subregion/',grep(subregions[i], EEZ.KBA.files, value=TRUE, fixed=TRUE))) # EEZ KBA file
glimpse(KBA.in)

KBA.sub<-KBA.in%>%
  filter(G_UNEP_sub == subregions[i])%>%
  st_buffer(0)%>%
  group_by(type, G_UNEP_sub)%>%
  summarize()%>%
  st_buffer(0)
glimpse(KBA.sub)

WDPA.sub.yrs<-grep(subregions[i],list.files('WDPA/SUB_YEAR/EEZ'), value=TRUE)
WDPA.sub.yrs

j<-1
WDPA.sub.yrs[j]
WDPA.in<-st_read(paste0('WDPA/SUB_YEAR/EEZ/',WDPA.sub.yrs[j]))%>%
  group_by(G_UNEP_sub, STATUS_YR)%>%summarize()%>%
  st_buffer(0)
glimpse(WDPA.in)

WDPA.in.KBA<-st_intersection(WDPA.in, KBA.sub)%>%st_buffer(0)
plot(WDPA.in.KBA%>%select(type))

####
temp.df<-data.frame(subregion = subregions[i],
                    type = 'EEZ',
                    year = str_split(WDPA.sub.yrs[j], '_', simplify=TRUE)[,1],
                    AREA_KM2 = set_units(st_area(WDPA.in.KBA), km^2))%>%
  mutate(AREA_HA = set_units(AREA_KM2, ha))
df.out<-rbind(df.out, temp.df)
st_write(WDPA.in.KBA, paste0('KBA/intersect/',WDPA.sub.yrs[j]), driver = 'ESRI Shapefile', delete_layer=TRUE)

##need to do the same for ABNJ and Land
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 5: Calculate total protected area in KBA  ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# combine at subregion level to consolidate overlapping areas
# calcualte area in M ha

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
