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
#  3) Buffer point data with reported area
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

# set working directory to input data '/ConsDB_Input/'
setwd("/Users/colleennell/Dropbox/ConsDB_Input")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Clean WDPA data  ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# polyogn data
wdpa.raw.polys <- st_read(dsn = 'WDPA_Aug2018-shapefile/', layer = 'WDPA_Aug2018-shapefile-polygons')

# point data
wdpa.raw.pts <- st_read(dsn = 'WDPA_Aug2018-shapefile/', layer ='WDPA_Aug2018-shapefile-points')

## Filter data 
# include protected areas with Status = designated, inscribed, and established
# include only points with a reported area
# exclude UNESCO Man and Biosphere Reserves

# Filter polygon data
polys.clean<-wdpa.raw.polys%>%
  filter(STATUS %in% c('Designated','Inscribed','Established'))%>%
  filter(DESIG_ENG != "UNESCO-MAB Biosphere Reserve"))

# Filter point data
pts.clean<-wdpa.raw.pts%>%
  filter(REP_AREA > 0)%>%
  filter(STATUS %in% c('Designated','Inscribed','Established'))%>%
  filter(DESIG_ENG != "UNESCO-MAB Biosphere Reserve"))

## Buffer protected areas reported as points, using the reported area to approximate coverage as circle

#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Buffer point data with reported area ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#



