
# METT data -- identifying 'effectively managed' PAs that meet a certain threshold
# based on Gill et al (2017) from Nature


# -- load libraries and raw data

pacman::p_load(xlsx,dplyr,reshape2,stringi)

METT_spatial <- read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/Tbl_Spatial_METT_18072016.xlsx', sheetName='Tbl_Spatial')

METT1_basicinfo <- read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/Tbl_METT1_Basic_info_18072016.xlsx', sheetName='Tbl_METT1_Basic information')
METT1_answers <- read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/Tbl_METT1_Questions_scores_18072016.xlsx', sheetName='Tbl_METT1 Questions and scores')

METT3_basicinfo <- read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/Tbl_METT3_Basic_info_18072016.xlsx', sheetName='Tbl_METT3 Basic information')
METT3_answers <- read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/Tbl_METT3_Questions_scores_18072016.xlsx', sheetName='Tbl_METT3 Questions and scores_')


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


