# 
# code: Food Practice Global Context and Global 2030 Outcome Indicator Tables
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: August 2018
# modified: 
# 
# ---- inputs ----
#  1) Food-specific data tables (in 1_Nov2018/2_FlatDataFiles/ConsDB_Input)
# 
# ---- outputs ----
#  1) Food-specific back-end tables -- ready to be consolidated with other Practices, to go into back-end:
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

FAOSTAT_FoodBalance <- read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/FAOSTAT_FoodBalanceSheets_dl_2018_0821.csv')

# -- GLOBAL DIETARY ENERGY SUPPLY

Dim_Context_State_Food_A <- 
  data.frame(Indicator_Type_Key="GCS_FD_A",
             Indicator_Name="Global dietary energy supply (kcal per capita per day)",
             Indicator_Label="Dietary Energy",
             Panel_Label="Food Supply",
             Panel="State",
             Indicator_Subcategory="A",
             Indicator_Unit="kcal/capita/day",
             Data_Source="FAOSTAT, FAO Food Balance Sheets")

Fact_Context_State_Food_A <-
  FAOSTAT_FoodBalance %>%
  subset(.,grepl("Food supply",Element,ignore.case=T)) %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Food"],length(Year)),
            Indicator_Type_Key=rep(Dim_Context_State_Food_A$Indicator_Type_Key,length(Year)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)

# -- GLOBAL DIETARY PROTEIN SUPPLY

Dim_Context_State_Food_B <- 
  data.frame(Indicator_Type_Key="GCS_FD_B",
             Indicator_Name="Global dietary protein supply (g per capita per day)",
             Indicator_Label="Protein",
             Panel_Label="Food Supply",
             Panel="State",
             Indicator_Subcategory="B",
             Indicator_Unit="g/capita/day",
             Data_Source="FAOSTAT, FAO Food Balance Sheets")

Fact_Context_State_Food_B <-
  FAOSTAT_FoodBalance %>%
  subset(.,grepl("Protein supply",Element,ignore.case=T)) %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Food"],length(Year)),
            Indicator_Type_Key=rep(Dim_Context_State_Food_B$Indicator_Type_Key,length(Year)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)

# -- GLOBAL DIETARY FAT SUPPLY

Dim_Context_State_Food_C <- 
  data.frame(Indicator_Type_Key="GCS_FD_C",
             Indicator_Name="Global dietary protein supply (g per capita per day)",
             Indicator_Label="Fat",
             Panel_Label="Food Supply",
             Panel="State",
             Indicator_Subcategory="C",
             Indicator_Unit="g/capita/day",
             Data_Source="FAOSTAT, FAO Food Balance Sheets")

Fact_Context_State_Food_C <-
  FAOSTAT_FoodBalance %>%
  subset(.,grepl("Fat supply",Element,ignore.case=T)) %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Food"],length(Year)),
            Indicator_Type_Key=rep(Dim_Context_State_Food_C$Indicator_Type_Key,length(Year)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 2.2 Context - Threat ----

OECDStat_LandUse <- read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/LandUse_OECDStat_dl_2018_0816.csv')

# -- LAND USE - CROPLAND 

Dim_Context_Threat_Food_A <- 
  data.frame(Indicator_Type_Key="GCT_FD_A",
             Indicator_Name="Land use (percentage of total land area)",
             Indicator_Label="Arable and Cropland",
             Panel_Label="Land Use",
             Panel="Threat",
             Indicator_Subcategory="A",
             Indicator_Unit="% of total land",
             Data_Source="OECDStat, from FAO")

Fact_Context_Threat_Food_A <-
  OECDStat_LandUse %>%
  subset(.,Country=="World" & 
           Year>1989 & 
           grepl("arable", Variable, ignore.case=T) &
           Unit=="Percentage") %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Food"],length(Year)),
            Indicator_Type_Key=rep(Dim_Context_Threat_Food_A$Indicator_Type_Key,length(Year)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)

# -- LAND USE - MEADOWS & PASTURE

Dim_Context_Threat_Food_B <- 
  data.frame(Indicator_Type_Key="GCT_FD_B",
             Indicator_Name="Land use (percentage of total land area)",
             Indicator_Label="Meadows and Pastures",
             Panel_Label="Land Use",
             Panel="Threat",
             Indicator_Subcategory="B",
             Indicator_Unit="% of total land",
             Data_Source="OECDStat, from FAO")

Fact_Context_Threat_Food_B <-
  OECDStat_LandUse %>%
  subset(.,Country=="World" & 
           Year>1989 & 
           grepl("meadow", Variable, ignore.case=T) &
           Unit=="Percentage") %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Food"],length(Year)),
            Indicator_Type_Key=rep(Dim_Context_Threat_Food_B$Indicator_Type_Key,length(Year)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)

# -- LAND USE - FOREST

Dim_Context_Threat_Food_C <- 
  data.frame(Indicator_Type_Key="GCT_FD_C",
             Indicator_Name="Land use (percentage of total land area)",
             Indicator_Label="Forest",
             Panel_Label="Land Use",
             Panel="Threat",
             Indicator_Subcategory="C",
             Indicator_Unit="% of total land",
             Data_Source="OECDStat, from FAO")

Fact_Context_Threat_Food_C <-
  OECDStat_LandUse %>%
  subset(.,Country=="World" & 
           Year>1989 & 
           grepl("forest", Variable, ignore.case=T) &
           Unit=="Percentage") %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Food"],length(Year)),
            Indicator_Type_Key=rep(Dim_Context_Threat_Food_C$Indicator_Type_Key,length(Year)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)

# -- LAND USE - OTHER

Dim_Context_Threat_Food_D <- 
  data.frame(Indicator_Type_Key="GCT_FD_D",
             Indicator_Name="Land use (percentage of total land area)",
             Indicator_Label="Meadows and Pastures",
             Panel_Label="Land Use",
             Panel="Threat",
             Indicator_Subcategory="D",
             Indicator_Unit="% of total land",
             Data_Source="OECDStat, from FAO")

Fact_Context_Threat_Food_D <-
  OECDStat_LandUse %>%
  subset(.,Country=="World" & 
           Year>1989 & 
           grepl("other", Variable, ignore.case=T) &
           Unit=="Percentage") %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Food"],length(Year)),
            Indicator_Type_Key=rep(Dim_Context_Threat_Food_D$Indicator_Type_Key,length(Year)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)

# ---- 2.3 Context - Response ----

# -- AGRICULTURAL EMISSIONS



# ---- 2.4 Consolidated Food-specific Global Context tables ----


#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Global 2030 Outcomes ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 3.1 Food Outcome 1 - SUSTAINABLE AGRICULTURE ----



# ---- 3.2 Food Outcome 2 - FOOD WASTE AND LOSS ----



# ---- 3.3 Food Outcome 3 - HEALTHY DIETS ----



# ---- 3.4 Consolidated Food-specific Global 2030 Outcome tables ----


