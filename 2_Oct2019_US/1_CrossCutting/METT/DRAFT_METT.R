
# METT data -- identifying 'effectively managed' PAs that meet a certain threshold
# based on Gill et al (2017) from Nature


# -- load libraries and raw data

pacman::p_load(xlsx,dplyr,reshape2,stringi, fuzzyjoin, tidyr, sf, units)

METT_spatial <- read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/METT/Tbl_Spatial_METT_18072016.xlsx', sheetName='Tbl_Spatial')

METT1_basicinfo <- read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/METT/Tbl_METT1_Basic_info_18072016.xlsx', sheetName='Tbl_METT1_Basic information')
METT1_answers <- read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/METT/Tbl_METT1_Questions_scores_18072016.xlsx', sheetName='Tbl_METT1 Questions and scores')

METT3_basicinfo <- read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/METT/Tbl_METT3_Basic_info_18072016.xlsx', sheetName='Tbl_METT3 Basic information')
METT3_answers <- read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/METT/Tbl_METT3_Questions_scores_18072016.xlsx', sheetName='Tbl_METT3 Questions and scores_')


# -- build out threshold data set for METT1 data

METT1_threshold <- 
  left_join(METT1_answers[,c("Assessment.ID","X15.Current.budget",
                               "X12.Staff.numbers","X7.Management.plan",
                               "X30.Monitoring.and.evaluation","X3.Law.enforcement",
                               "X6.Protected.area.boundary","X2.Protected.area.regulations",
                               "X1.Legal.status")],
            METT1_basicinfo[,c("CatalogueCode","AssessmentID","YearAssessment","PA_ID",
                               "PA_FullName","Area.of.PA..ha._N")],
            by=c("Assessment.ID"="AssessmentID")) %>%
  left_join(METT_spatial, by="Assessment.ID") %>%
  mutate(Budget=ifelse(X15.Current.budget>=2,1,0),
         Staff=ifelse(X12.Staff.numbers==3,1,0),
         MgmtPlan=ifelse(X7.Management.plan>=2,1,0),
         MonEval=ifelse(X30.Monitoring.and.evaluation==3,1,0),
         Enforcement=ifelse(X3.Law.enforcement>=2,1,0),
         Boundaries=ifelse(X6.Protected.area.boundary==3,1,0),
         Regulations=ifelse(X2.Protected.area.regulations>=2,1,0),
         LegalStatus=ifelse(X1.Legal.status==3,1,0)) 

METT1_threshold$Threshold <- rowSums(METT1_threshold[,c("Budget","Staff","MgmtPlan","MonEval","Enforcement",
                                                        "Boundaries","Regulations","LegalStatus")])


# -- build out threshold data set for METT3 data

METT3_threshold <- 
  left_join(METT3_answers[,c("Assessment.ID","X15.Current.budget",
                             "X13.Staff.numbers","X7.Management.plan",
                             "X26.Monitoring.and.evaluation","X3.Law.enforcement",
                             "X6.Protected.area.boundary.demarcation","X2.Protected.area.regulations",
                             "X1.Legal.status")],
            METT3_basicinfo[,c("CatalogueCode","AssessmentID","YearAssessment","PA_ID",
                               "PA_FullName","Area.of.PA..ha._N")],
            by=c("Assessment.ID"="AssessmentID")) %>%
  left_join(METT_spatial, by="Assessment.ID") %>%
  mutate(Budget=ifelse(X15.Current.budget>=2,1,0),
         Staff=ifelse(X13.Staff.numbers==3,1,0),
         MgmtPlan=ifelse(X7.Management.plan>=2,1,0),
         MonEval=ifelse(X26.Monitoring.and.evaluation==3,1,0),
         Enforcement=ifelse(X3.Law.enforcement>=2,1,0),
         Boundaries=ifelse(X6.Protected.area.boundary.demarcation==3,1,0),
         Regulations=ifelse(X2.Protected.area.regulations>=2,1,0),
         LegalStatus=ifelse(X1.Legal.status==3,1,0)) 

METT3_threshold$Threshold <- rowSums(METT3_threshold[,c("Budget","Staff","MgmtPlan","MonEval","Enforcement",
                                                        "Boundaries","Regulations","LegalStatus")])

# -- join METT1 and METT3 threshold data

METT_threshold <-
  rbind.data.frame(METT1_threshold%>%select(.,-c("X15.Current.budget",
                                                 "X12.Staff.numbers","X7.Management.plan",
                                                 "X30.Monitoring.and.evaluation","X3.Law.enforcement",
                                                 "X6.Protected.area.boundary","X2.Protected.area.regulations",
                                                 "X1.Legal.status")),
                   METT3_threshold%>%select(.,-c("X15.Current.budget",
                                                 "X13.Staff.numbers","X7.Management.plan",
                                                 "X26.Monitoring.and.evaluation","X3.Law.enforcement",
                                                 "X6.Protected.area.boundary.demarcation","X2.Protected.area.regulations",
                                                 "X1.Legal.status"))) %>%
  .[!is.na(.$Assessment.ID),] %>%
  mutate(Year=stri_extract_last_regex(CatalogueCode, "\\d{4}"))

# Remove repeat assessment data, only keeping most recent assessments
METT_mostrecentyear <-
  METT_threshold %>%
  group_by(PA_ID,WDPA.ID) %>%
  summarise(MostRecentYear=max(Year))

# Final data sets, with repeat assessments removed, and ready to be paired to WDPA database
# -- NOTE: will want to check PA_FullName against WDPA protected areas, to see if any of the observations 
# --       with 999999999 as their WDPA.ID actually match.  Otherwise, will need to remove those PAs from analysis.
# -- NOTE 2: Threshold for "effective managment" is 8

METT_threshold <-
  left_join(METT_threshold,METT_mostrecentyear,by=c("WDPA.ID","PA_ID")) %>%
  subset(.,Year==MostRecentYear)
  
METT_meetsthreshold <-
  subset(METT_threshold,Threshold==8)

write.csv(METT_threshold, 'METT/METT_threshold.csv', row.names=FALSE)
write.csv(METT_meetsthreshold, 'METT/METT_meetsthreshold.csv', row.names=FALSE)

# --  Pair METT assessments with WDPA 

METT<-read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/METT/METT_threshold.csv')
METT_thresh<-read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/METT/METT_meetsthreshold.csv')

wdpaid.list<-unique(METT$WDPA.ID) # how many WDPA IDs were given?
length(wdpaid.list) #1826 

# match METT with WDPA 
wdpa<-read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/WDPA/WDPA_Aug2018-csv.csv')

# METT assessments with WDPA ids
METT_id<-METT%>%filter(!is.na(WDPA.ID) & WDPA.ID != 999999999) # 2001 sites with ids, 1824 different WDPA IDs (can be duplciates to to different parcels in site)
# match by WDPA id
wdpa.match<-METT_id%>%
  left_join(wdpa, by=c('WDPA.ID'='WDPAID'))%>%
  select(Assessment.ID:Area.of.PA..ha._N, WDPA.ID, Exact.match.:Name.designation.from.WDPA, NAME, ISO3)

# some WDPA ids reported in METT did not match with actual WDPA site (14)
no.wdpa.match<-wdpa.match%>%filter(is.na(NAME))
no.wdpa.match.list<-unique(no.wdpa.match$PA_FullName)
length(unique(no.wdpa.match))

# METTs without WDPA ID
METT_noid<-METT%>%filter(is.na(WDPA.ID) | WDPA.ID == 999999999) # 599 METT assessments without ID 

# add incorrect WDPA ids to METT assessments with no wdpa at all
fuzzy.names<-rbind(METT_noid, METT_id%>%filter(PA_FullName %in% no.wdpa.match.list))
dim(fuzzy.names)# 811 assessments
length(unique(fuzzy.names$PA_FullName))# 723 different site names

# Use METT 'PA_FullName' to match to 'NAME' in WDPA using fuzzy match
fuzzy.list<-fuzzy.names%>%select(METT_NAME = PA_FullName, WDPA.ID, PA_ID, Name.designation.from.WDPA)
fuzz.name<-fuzzy.list%>%
  stringdist_left_join(wdpa%>%select(WDPAID, NAME, ORIG_NAME, DESIG, DESIG_ENG, ISO3), by=c(METT_NAME = 'NAME'), distance_col='dist')
head(fuzz.name)

fuzz.name.good<-fuzz.name%>%filter(!is.na(WDPAID)) 
fuzz.good<-fuzz.name.good%>%
  separate(PA_ID, '_', into=c('ISO3_comp','we'))%>%
  select(ISO3, ISO3_comp, METT_NAME, NAME, WDPAID, WDPA.ID, dist)

## use 'dist' in fuzzy match the determine correct matches between the WDPA and METT
# good match = has same name (dist = 0), 20 sites
matched.dist.0<-fuzz.good%>%filter(dist == 0)
# good match = same country, dist = 1 or 2, 15 sites
matched.iso<-fuzz.good%>%filter(ISO3==ISO3_comp, dist !=0)

## examine remaining unmatched site names manually
# compare NAME and METT_NAME to determine any site matches between databases. 
# ISO3m and ISO3_comp show ssociated country codes between the two, should be the same, or very similar geographically if it is a transboundary site
match.left<-fuzz.good%>%filter(ISO3 != ISO3_comp, dist!=0)
View(match.left) # cant be confident about any others

fuzz.name.orig<-fuzzy.list%>%stringdist_left_join(wdpa, by=c(METT_NAME = 'ORIG_NAME'), distance_col='dist')
fuzz.orig.diff<-fuzz.name.orig%>%
  filter(!is.na(WDPAID))%>%
  separate(PA_ID, '_', into=c('ISO3_comp','we'))%>%
  select(ISO3, ISO3_comp, METT_NAME, NAME,ORIG_NAME, WDPAID, WDPA.ID, dist)

# same name
omatched.dist.0<-fuzz.orig.diff%>%filter(dist == 0)
#compare lists of names
length(unique(omatched.dist.0$WDPAID))
setdiff(omatched.dist.0$WDPAID, matched.dist.0$WDPAID)
setdiff(matched.dist.0$WDPAID,omatched.dist.0$WDPAID)

# same country, dist = 1 or 2
omatched.iso<-fuzz.orig.diff%>%filter(ISO3==ISO3_comp, dist !=0)

# list of WDPAID to add to dataframe
id.in<-c(unique(matched.dist.0$WDPAID), setdiff(omatched.iso$WDPAID, matched.iso$WDPAID),setdiff(matched.iso$WDPAID,omatched.iso$WDPAID))## include these in with threshold wdpa ids
fuzz.name.out<-fuzz.name%>%filter(WDPAID %in% c(unique(omatched.dist.0$WDPAID), setdiff( matched.dist.0$WDPAID,omatched.dist.0$WDPAID)))
fuzz.name.out2<-fuzz.orig.diff%>%filter(WDPAID %in% c(setdiff( matched.dist.0$WDPAID,omatched.dist.0$WDPAID)))
fuzz.o<-rbind(fuzz.name.out%>%select(METT_NAME, WDPAID), fuzz.name.out2%>%select(METT_NAME, WDPAID))

# add fuzz.o to matched names
wdpa.fuzz<-wdpa%>%filter(WDPAID %in% fuzz.o$WDPAID)

# add to matched df
wdpa.match.all<-rbind(wdpa.match%>%select(WDPAID = WDPA.ID, NAME=PA_FullName), wdpa.fuzz%>%select(WDPAID, NAME))
length(unique(wdpa.match.all$WDPAID))#1840/2600 match


# -- Filter WDPA shapefiles to METT sites  
METT_in<-wdpa.match.all

st_layers('WDPA/GLOBAL') # WDPA 'clean' layers are produced in 'WDPA_calculate_PA.R' script step 2

# WDPA polygons
pas.polys<-st_read('WDPA/GLOBAL',layer='wdpa_polygon_clean') #210442 features
polys.mett<-pas.polys%>%
  filter(WDPAID %in% METT_in$WDPAID)%>%
  dplyr::select(WDPAID, NAME, DESIG_ENG, DESIG_TYPE, REP_M_AREA, REP_AREA, STATUS, STATUS_YR, ISO3)
rm(pas.polys)

# WDPA points
pas.pts<-st_read('WDPA/GLOBAL',layer='wdpa_pts_clean')#
pts.mett<-pas.pts%>%
  filter(WDPAID %in% METT_in$WDPAID)%>%
  dplyr::select(WDPAID, NAME, DESIG_ENG, DESIG_TYPE, REP_M_AREA, REP_AREA, STATUS, STATUS_YR, ISO3)
rm(pas.pts)

# combine points and polygons
mett.wdpa<-do.call(rbind, list(polys.mett, pts.mett))

# spatially aggregate METT sites
mett.agg<-mett.wdpa%>%st_buffer(0)%>%mutate(METT = 'METT')%>%group_by(METT)%>%summarize()%>%st_buffer(0)
st_write(mett.agg, 'METT/METT_hasone_agg', driver='ESRI Shapefile')

# same for sites that meet the threshold
mett.thresh<-mett.wdpa%>%filter(WDPAID %in% METT_meetsthreshold$WDPA.ID)
mett.thresh.agg<-mett.thresh%>%mutate(METT = 'meets threshold')%>%group_by(METT)%>%summarize()%>%st_buffer(0)
st_write(mett.thresh.agg, 'METT/METT_meetsthreshold', driver='ESRI Shapefile', delete_layer=TRUE)

# -- Find area of METT assessed sites and that meet threshold in M ha
set_units(st_area(mett.thresh.agg), ha)
set_units(st_area(mett.thresh.agg), ha)/1000000 #  Mha

set_units(st_area(mett.agg), ha)#  ha
set_units(st_area(mett.agg), ha)/1000000# Mha

df.out<-data.frame(METT_Mha = as.numeric(set_units(st_area(mett.thresh.agg), ha)/1000000),
                   METT_meetsthreshold_Mha = as.numeric(set_units(st_area(mett.agg), ha)/1000000),
                   METT_mettsthreshold_percent = 100*((as.numeric(set_units(st_area(mett.thresh.agg), ha)/1000000))/(as.numeric(set_units(st_area(mett.agg), ha)/1000000))))
write.csv(df.out, 'METT_area.csv', row.names=FALSE)

# -- METT assessments as percent of MPA
eez<-st_read('EEZ_WVS_layer/EEZv8_WVS_DIS_V3_ALL_final_v7disIPBES')%>%st_transform('+proj=moll')
mett<-st_read('METT/METT_hasone_agg')%>%st_buffer(0)
mett.thresh<-st_read('METT/METT_meetsthreshold')%>%st_buffer(0)

# intersect mett with global boundaries
mett_eez<-st_intersection(mett%>%st_buffer(0), eez%>%st_buffer(0))%>%st_buffer(0)
mett_eez_only<-mett_eez%>%filter(type=='EEZ')
mett.eez.agg<-mett_eez_only%>%group_by(type)%>%summarize()

# calculate M ha
as.numeric(set_units(st_area(mett.eez.agg),ha)/1000000)  # 39.96398 Mha of MPA with METT

# -- what percent meets the threshold?
mett_eez_thresh<-st_intersection(mett.thresh%>%st_buffer(0), eez%>%st_buffer(0))%>%st_buffer(0)
mett_thresh_agg<-mett_eez_thresh%>%filter(type=='EEZ')%>%group_by(type)%>%summarize()%>%st_buffer(0)
as.numeric(set_units(st_area(mett_thresh_agg),ha)/1000000)  # 0.005121591 Mha

df.out<-data.frame(type = 'EEZ', 
                   MPA_total_Mha = 1490.584, 
                   MPA_METT_Mha = as.numeric(set_units(st_area(mett.eez.agg),ha)/1000000), 
                   MPA_METT_percent = 100*(MPA_METT_Mha/MPA_total_Mha), 
                   MPA_METT_threshold_Mha = as.numeric(set_units(st_area(mett_thresh_agg),ha)/1000000), 
                   MPA_threshold_percent = 100*(MPA_METT_threshold_Mha/MPA_METT_Mha))
write.csv(df.out, 'METT/METT_MPA.csv', row.names=FALSE)

