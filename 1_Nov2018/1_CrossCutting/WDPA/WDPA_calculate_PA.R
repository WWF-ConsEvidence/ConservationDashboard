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
#  3) 
#  4) 
# 
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
setwd("/Users/colleennell/Dropbox/ConsDB_Input")

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
# ---- SECTION 4: Intersect with global boudnaries  ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Intersect each ISO3.year with subregional coastline for terrestrial, marine, ABNJ
ISO3.yr.list<-list.files('WDPA/ISO3_YEAR/')

## deal with transboundary sites
# all the ISO3s of PAs to intersect, some have multiple ISO3 due to transboundary sites
isos<-read.csv('ISO3_subregions.csv')%>%transform(ISO3=as.character(ISO3))
isos.sub<-isos%>%filter(ISO3 %in% list.files('WDPA/ISO3_YEAR/'))

multi.list<-grep(';',list.files('WDPA/ISO3_YEAR/'), value=TRUE) # trasnboundary ISO3s
iso.multi<-t(str_split(grep(';', multi.list, value=TRUE), ';', simplify=TRUE)) # multiple codes
iso.multi

## add rows to assign sites with multiple iso3 codes to all relevant subregions
# create a data frame with row for each country, preserve multi code
ISO3.out<-NULL
for (d in 1:length(multi.list)){
  pa1<-data.frame(ISO3 = iso.multi[,d], ISO3_multi = multi.list[d])%>%filter(ISO3 != '')
  ISO3.out<-rbind(pa1, ISO3.out)
}

pa.out.sub<-ISO3.out%>%left_join(isos.sub, by='ISO3')# join with subregion data based on ISO3

isos<-isos.sub%>%mutate(ISO3_multi = ISO3)%>%select(colnames(pa.out.sub))
isos.in<-rbind(isos, pa.out.sub)#recomine with single code PA iso codes
#write.csv(isos.in, 'data_out/wdpa_transboundary.csv', row.names=FALSE)

## for each subregion
# read in ISO3_year
# intersect with subregion boundaries
iso.split<-split(isos.in, isos.in$GEO_subregion)

## Break up WDPA by UN subregion
boundaries<-st_read('EEZ_WVS_layer/EEZv8_WVS_DIS_V3_ALL_final_v7disUNEP') # or EEZv8_WVS_DIS_V3_ALL_final_v7disIPBES
sub.list<-unique(boundaries$GEO_subregion)

# for each subregion, find files with matching subregion
file.df<-data.frame(files = list.files('WDPA/ISO3_YEAR'))%>% # all files
  separate(file, '_', into=c('ISO3', 'YEAR'), remove=FALSE)%>%
  left_join(isos.in, by=c('ISO3'= 'ISO3_multi'))# match subregion

files.sub<-grep()

year.subs.fast<-grep('Central and Western Europe', list.files('WDPA/year_subregion'), value=TRUE) # find list of wdpa files

for (i in 1:length(names(iso.split))){
  sub.area<-boundaries%>%filter( == sub.list[i]) # subregion boundaries
  sub.ISO3<-iso.split[[i]]$ISO3_multi # ISO3 list
  # select files containing these ISO3 codes
  
  for (j in 1:length(sub.ISO3)){
    

}


for (i in 1:length())





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
# ---- SECTION 5:  ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#



