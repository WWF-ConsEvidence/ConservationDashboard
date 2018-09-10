# 
# code: Forest Practice Indicator and Initiative Tables
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: August 2018
# modified: 
# 
# ---- inputs ----
#  1) Forest-specific data tables (in 1_Nov2018/2_FlatDataFiles/ConsDB_Input)
# 
# ---- outputs ----
#  1) Forest-specific back-end tables -- ready to be consolidated with other Practices, to go into back-end:
#     - Dim_Context_Indicator_Type
#     - Fact_Global_Context_Indicators
#     - Dim_Global_WWF_2030_Indicator_Type
#     - Fact_Global_2030_Outcomes
#     - Dim_Initiative
#     - Fact_Initiative_Financials
#     - Dim_Initiative_Indicator_Type
#     - Fact_Initiative_Indicators
#     - Milestone_Group_Bridge
#     - Dim_Milestone
# 
# ---- code sections ----
#  1) Load libraries, add reference tables
#  2) Global Context
#  3) Global 2030 Outcomes
#  4) Initiatives
# 
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: Load libraries, add reference tables ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

pacman::p_load(dplyr, xlsx)


practice_key_ref <- read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/cons_dashboard_dim_tables_20180828.xlsx',
                              sheetName='Dim_Practice')

practice_outcome_key_ref <- read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/cons_dashboard_dim_tables_20180828.xlsx',
                                      sheetName='Dim_Practice_Outcome')


#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Global Context ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 2.1 Context - State ----

# -- FOREST EXTENT

Dim_Context_State_Forest_A <- 
  data.frame(Indicator_Type_Key="GCS_FR_A",
             Indicator_Name="Forest area (millions of hectares)",
             Indicator_Label="Total Area of Tree Cover",
             Panel_Label="Forest Extent",
             Panel="State",
             Indicator_Subcategory="A",
             Indicator_Unit="M ha",
             Data_Source="FAO, Global Forest Resources Assessment")

Fact_Context_State_Forest_A <-
  read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/SDG_15.1.1_forest_area_dl_2018_0828.csv') %>%
  transmute(Year_Key=TimePeriod,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Forests"],length(TimePeriod)),
            Indicator_Type_Key=rep(Dim_Context_State_Forest_A$Indicator_Type_Key,length(TimePeriod)),
            Indicator_Value=Value/1000,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 2.2 Context - Threat ----

# -- FOREST LOSS - LOSS

Dim_Context_Threat_Forest_A <- 
  data.frame(Indicator_Type_Key="GCT_FR_A",
             Indicator_Name="Forest cover loss (millions of hectares per year)",
             Indicator_Label="Forest Loss",
             Panel_Label="Forest Loss",
             Panel="Threat",
             Indicator_Subcategory="A",
             Indicator_Unit="M ha per year",
             Data_Source="Global Forest Watch")

Fact_Context_Threat_Forest_A <-
  read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/GFW_ForestLoss_2018_0821.xlsx', sheetName="Sheet1") %>%
  subset(.,Geography=="World") %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Forests"],length(Year)),
            Indicator_Type_Key=rep(Dim_Context_Threat_Forest_A$Indicator_Type_Key,length(Year)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)

# -- FOREST LOSS - FRAGMENTATION

Dim_Context_Threat_Forest_B <- 
  data.frame(Indicator_Type_Key="GCT_FR_B",
             Indicator_Name="Forest fragmentation (non-core:core ratio)",
             Indicator_Label="Forest Fragmentation",
             Panel_Label="Forest Loss",
             Panel="Threat",
             Indicator_Subcategory="B",
             Indicator_Unit="Non-core : Core",
             Data_Source="Aurelie Shapiro, WWF-DE")

Fact_Context_Threat_Forest_B <-
  read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/GFW_ForestLoss_2018_0821.xlsx', sheetName="Sheet1") %>%
  subset(.,Geography=="World") %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Forests"],length(Year)),
            Indicator_Type_Key=rep(Dim_Context_Threat_Forest_B$Indicator_Type_Key,length(Year)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 2.3 Context - Response ----

# -- FOREST PROTECTION - PROTECTED AREAS

Dim_Context_Response_Forest_A <- 
  data.frame(Indicator_Type_Key="GCR_FR_A",
             Indicator_Name="Global terrestrial protected area coverage (M ha)",
             Indicator_Label="Protected Areas",
             Panel_Label="Forest Protection",
             Panel="Response",
             Indicator_Subcategory="A",
             Indicator_Unit="M ha",
             Data_Source="WDPA")

Fact_Context_Response_Forest_A <-
  data.frame(Year_Key=NA,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Forests"],length(1)),
            Indicator_Type_Key=rep(Dim_Context_Response_Forest_A$Indicator_Type_Key,length(1)),
            Indicator_Value=NA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)

# FOREST PROTECTION - FSC Coverage

Dim_Context_Response_Forest_B <- 
  data.frame(Indicator_Type_Key="GCR_FR_B",
             Indicator_Name="FSC certified forest area (M ha)",
             Indicator_Label="FSC Coverage",
             Panel_Label="Forest Protection",
             Panel="Response",
             Indicator_Subcategory="B",
             Indicator_Unit="M ha",
             Data_Source="FSC Annual Reports")

Fact_Context_Response_Forest_B <-
  read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/FSC_area_2017_0915.csv') %>%
  transmute(Year_Key=Year,
             Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Forests"],length(Year_Key)),
             Indicator_Type_Key=rep(Dim_Context_Response_Forest_B$Indicator_Type_Key,length(Year_Key)),
             Indicator_Value=Value,
             Indicator_Upper_Value=NA,
             Indicator_Lower_Value=NA)


# ---- 2.4 Consolidated Forest-specific Global Context tables ----

Dim_Context_Forest <- 
  rbind.data.frame(Dim_Context_State_Forest_A,
                   Dim_Context_Threat_Forest_A,
                   Dim_Context_Threat_Forest_B,
                   Dim_Context_Response_Forest_A,
                   Dim_Context_Response_Forest_B)

Fact_Context_Forest <-
  rbind.data.frame(Fact_Context_State_Forest_A,
                   Fact_Context_Threat_Forest_A,
                   Fact_Context_Threat_Forest_B,
                   Fact_Context_Response_Forest_A,
                   Fact_Context_Response_Forest_B)


#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Global 2030 Outcomes ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 3.1 Forest Outcome 1 - RESILIENT FORESTS  ----

# -- EFFECTIVE PROTECTION (PAME -AS INTERIM)

Dim_Global_2030_Outcome1_Forest_A <- 
  data.frame(Indicator_Type_Key="OUT1_FR_A",
             Indicator_Name="",
             Indicator_Label="Effective Protection",
             Indicator_Subcategory="A",
             Indicator_Unit="",
             Data_source="",
             Indicator_Target=NA,
             Indicator_Type="Outcome",
             Panel_Label="Resilient Forests",
             Display_Order=1)

Fact_Global_2030_Outcome1_Forest_A <-
  data.frame(Year_Key=NA,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Forests"],length(1)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome1_Forest_A$Indicator_Type_Key,length(1)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Forests" &
                                                                   grepl("Manage",practice_outcome_key_ref$practice_outcome)], length(1)),
            Indicator_Value=NA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)

# -- EFFECTIVE MANAGEMENT (FSC)

Dim_Global_2030_Outcome1_Forest_B <- 
  data.frame(Indicator_Type_Key="OUT1_FR_B",
             Indicator_Name="FSC coverage",
             Indicator_Label="Effective Management",
             Indicator_Subcategory="B",
             Indicator_Unit="M ha FSC certified",
             Data_source="FSC",
             Indicator_Target=NA,
             Indicator_Type="Outcome",
             Panel_Label="Resilient Forests",
             Display_Order=1)

Fact_Global_2030_Outcome1_Forest_B <-
  read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/FSC_area_2017_0915.csv') %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Forests"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome1_Forest_B$Indicator_Type_Key,length(Year_Key)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Forests" &
                                                                   grepl("Manage",practice_outcome_key_ref$practice_outcome)], length(Year_Key)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 3.2 Forest Outcome 2 - HALT DEFORESTATION ----

Dim_Global_2030_Outcome2_Forest_A <- 
  data.frame(Indicator_Type_Key="OUT2_FR_A",
             Indicator_Name="Global tree cover loss (M ha per year)",
             Indicator_Label="Forest Cover Loss",
             Indicator_Subcategory="A",
             Indicator_Unit="M ha per year",
             Data_source="Global Forest Watch",
             Indicator_Target=NA,
             Indicator_Type="Outcome",
             Panel_Label="Halt Deforestation",
             Display_Order=2)

Fact_Global_2030_Outcome2_Forest_A <-
  read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/GFW_ForestLoss_2018_0821.xlsx', sheetName="Sheet1") %>%
  subset(.,Geography=="World") %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Forests"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome2_Forest_A$Indicator_Type_Key, length(Year_Key)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Forests" &
                                                                   grepl("Deforestation",practice_outcome_key_ref$practice_outcome)], length(Year_Key)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 3.3 Forest Outcome 3 - FOREST RESTORATION ----

Dim_Global_2030_Outcome3_Forest_A <- 
  data.frame(Indicator_Type_Key="OUT3_FR_A",
             Indicator_Name="Commitments to forest restoration (millions ha by 2030)",
             Indicator_Label="Total Area Committed for Restoration",
             Indicator_Subcategory="A",
             Indicator_Unit="Million ha",
             Data_source="Bonn Challenge, country-level commitments",
             Indicator_Target=350,
             Indicator_Type="Outcome",
             Panel_Label="Forest Restoration",
             Display_Order=3)

Fact_Global_2030_Outcome3_Forest_A <-
  read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/BonnChallenge_commitments_2018_0820.xlsx', 
            sheetName="Sheet1") %>%
  group_by(Commitment_Year) %>%
  summarise(Indicator_Value=sum(Committed_Area)) %>%
  transmute(Year_Key=Commitment_Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Forests"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome3_Forest_A$Indicator_Type_Key, length(Year_Key)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Forests" &
                                                                   grepl("Restoration",practice_outcome_key_ref$practice_outcome)], length(Year_Key)),
            Indicator_Value=cumsum(Indicator_Value),
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 3.4 Consolidated Forest-specific Global 2030 Outcome tables ----

Dim_Global_2030_Outcome_Forest <- 
  rbind.data.frame(Dim_Global_2030_Outcome1_Forest_A,
                   Dim_Global_2030_Outcome1_Forest_B,
                   Dim_Global_2030_Outcome2_Forest_A,
                   Dim_Global_2030_Outcome3_Forest_A)

Fact_Global_2030_Outcome_Forest <-
  rbind.data.frame(Fact_Global_2030_Outcome1_Forest_A,
                   Fact_Global_2030_Outcome1_Forest_B,
                   Fact_Global_2030_Outcome2_Forest_A,
                   Fact_Global_2030_Outcome3_Forest_A)

#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: Initiatives ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 4.1 Load data ----

dim.initiatives.forests <- 
  read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/fy18_initiative_reporting_dim_2018_0908.xlsx',sheetName="Sheet1") %>%
  subset(.,Practice=="Forests") 

dim.initiative.indicators.forests <-
  read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/fy18_initiative_indicators_fact_2018_0908.xlsx',sheetName="Sheet1") %>%
  subset(.,Practice=="Forests")

dim.initiative.milestones.forests <-
  read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/fy18_initiative_milestones_2018_0908.csv') %>%
  subset(.,Practice=="Forests")


# ---- 4.2 Forest-specific Dim_Initiative ----

Dim_Initiative_Forest <-
  dim.initiatives.forests %>%
  transmute(Initiative_Key=Initiative.key,
            Initiative_Name=Initiative,
            Initiative_Status=Overall.status,
            Initiative_Status_Justification=Overall.just,
            Initiative_Goal=Initiative.statement)


# ---- 4.3 Forest-specific Dim_Initiative_Indicator_Type ----

Dim_Initiative_Indicator_Forest <-
  dim.initiative.indicators.forests %>%
  transmute(Indicator_Type_Key=Initiative.indicator.key,
            Indicator_Type=Indicator.type,
            Indicator_Name=ifelse(!is.na(Indicator.name),as.character(Indicator.name),"FORTHCOMING"),
            Indicator_Label=Indicator.label,
            Indicator_Subcategory=Subcategory,
            Indicator_Target=Target.value,
            Indicator_Unit=Units,
            Data_Source=Source)


# ---- 4.4 Forest-specific Fact_Initiative_Indicators ----

Fact_Initiative_Indicator_Forest <-
  left_join(dim.initiative.indicators.forests,dim.initiatives.forests,by="Initiative") %>%
  transmute(Year_Key=NA,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Forests"],length(Year_Key)),
            Initiative_Key=Initiative.key,
            Indicator_Type_Key=Initiative.indicator.key,
            Practice_Outcome_Key=Practice.outcome.key,
            Indicator_Value=NA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 4.5 Forest-specific Fact_Initiative_Financials ----

Fact_Initiative_Financials_Forest <-
  dim.initiatives.forests %>%
  transmute(Date_Key=Date,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Forests"],length(Date_Key)),
            Initiative_Key=Initiative.key,
            Amount_needed=Funds.needed,
            Amount_secured=Funds.secured)


# ---- 4.6 Forest-specific Milestone_Group_Bridge ----

Milestone_Group_Bridge_Forest <-
  left_join(dim.initiative.milestones.forests, dim.initiatives.forests, by=c("Initiative", "Practice")) %>%
  transmute(Milestone_Key=Milestone.key,
            Initiative_Key=Initiative.key)


# ---- 4.7 Forest-specific Dim_Milestone ----

Dim_Milestone_Forest <-
  dim.initiative.milestones.forests %>%
  transmute(Milestone_Key=Milestone.key,
            Milestone_Name=Milestone,
            Milestone_Target=Target,
            Milestone_Status=Status,
            Milestone_Status_Justification=Status.just)




# ---- REMOVE CLUTTER ----

rm(Dim_Context_State_Forest_A,
   Dim_Context_Threat_Forest_A,
   Dim_Context_Threat_Forest_B,
   Dim_Context_Response_Forest_A,
   Dim_Context_Response_Forest_B,
   Fact_Context_State_Forest_A,
   Fact_Context_Threat_Forest_A,
   Fact_Context_Threat_Forest_B,
   Fact_Context_Response_Forest_A,
   Fact_Context_Response_Forest_B,
   Dim_Global_2030_Outcome1_Forest_A,
   Dim_Global_2030_Outcome1_Forest_B,
   Dim_Global_2030_Outcome2_Forest_A,
   Dim_Global_2030_Outcome3_Forest_A,
   Fact_Global_2030_Outcome1_Forest_A,
   Fact_Global_2030_Outcome1_Forest_B,
   Fact_Global_2030_Outcome2_Forest_A,
   Fact_Global_2030_Outcome3_Forest_A,
   dim.initiatives.forests,
   dim.initiative.indicators.forests,
   dim.initiative.milestones.forests)
