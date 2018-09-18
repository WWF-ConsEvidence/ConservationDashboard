# 
# code: Forest Practice Global Context and Global 2030 Outcome Indicator Tables
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
# 
# ---- code sections ----
#  1) Load libraries, add reference tables
#  2) Global Context
#  3) Global 2030 Outcomes
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


#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Global Context ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 2.1 Context - State ----

# -- TREE COVER

Dim_Context_State_Forest <- 
  data.frame(Indicator_Type_Key="GCS_FO_A",
             Indicator_Name="Forest area (millions of hectares)",
             Indicator_Label="Total Area of Tree Cover",
             Panel_Label="Forest Extent",
             Panel="State",
             Indicator_Subcategory=NA,
             Indicator_Unit="M ha",
             Data_Source="FAO, Global Forest Resources Assessment")

Fact_Context_State_Forest <-
  read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/SDG_15.1.1_forest_area_dl_2018_0828.csv') %>%
  transmute(Year_Key=TimePeriod,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Forests"],length(TimePeriod)),
            Indicator_Type_Key=rep(Dim_Context_State_Forest$Indicator_Type_Key,length(TimePeriod)),
            Indicator_Value=Value/1000,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 2.2 Context - Threat ----

# -- TREE LOSS

Dim_Context_Threat_Forest_A <- 
  data.frame(Indicator_Type_Key="GCT_FO_A",
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

# -- FOREST FRAGMENTATION

Dim_Context_Threat_Forest_B <- 
  data.frame(Indicator_Type_Key="GCT_FO_B",
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

# PA coverage
# FSC coverage


# ---- 2.4 Consolidated Forest-specific Global Context tables ----


#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Global 2030 Outcomes ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 3.1 Forest Outcome 1 - RESILIENT FORESTS  ----

# Effective protection (PAME?)
# Effective management (FSC)


# ---- 3.2 Forest Outcome 2 - HALT DEGRADATION ----

# Tree cover loss, trending toward a stable line


# ---- 3.3 Forest Outcome 3 - FOREST RESTORATION ----

Dim_Global_WWF_2030_Outcome3_Forest <- 
  data.frame(Indicator_Type_Key="OUT3_FO_A",
             Indicator_Name="Commitments to forest restoration (millions ha by 2030)",
             Indicator_Label="Total Area Committed for Restoration",
             Indicator_Subcategory=NA,
             Indicator_Unit="Million ha",
             Data_source="Bonn Challenge, country-level commitments",
             Indicator_Target=350,
             Indicator_Type="Outcome",
             Panel_Label="Forest Restoration",
             Display_Order=3)

Fact_Global_2030_Outcome3_Forest <-
  read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/BonnChallenge_commitments_2018_0820.xlsx', 
            sheetName="Sheet1") %>%
  group_by(Commitment_Year) %>%
  summarise(Indicator_Value=sum(Committed_Area)) %>%
  transmute(Year_Key=Commitment_Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Forests"],length(Commitment_Year)),
            Indicator_Type_Key=rep(Dim_Global_WWF_2030_Outcome3_Forest$Indicator_Type_Key, length(Commitment_Year)),
            Indicator_Value=cumsum(Indicator_Value),
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 3.4 Consolidated Forest-specific Global 2030 Outcome tables ----


