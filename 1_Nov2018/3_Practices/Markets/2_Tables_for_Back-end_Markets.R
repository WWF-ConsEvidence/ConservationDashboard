# 
# code: Markets Practice Indicator and Initiative Tables
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: August 2018
# modified: 
# 
# ---- inputs ----
#  1) Markets-specific data tables (in 1_Nov2018/2_FlatDataFiles/ConsDB_Input)
# 
# ---- outputs ----
#  1) Markets-specific back-end tables -- ready to be consolidated with other Practices, to go into back-end:
#     - Dim_Context_Indicator_Type
#     - Fact_Global_Context_Indicators
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
#  3) Initiatives
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

sdg.12.2 <- read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/SDG_12.2_material_consumption_world_data_dl_2018_0828.csv')

# -- GLOBAL MATERIAL CONSUMPTION - PER CAPITA

Dim_Context_State_Markets_A <- 
  data.frame(Indicator_Type_Key="GCS_MK_A",
             Indicator_Name="Global Material Consumption, per capita",
             Indicator_Label="Domestic Material Consumption of All Raw Materials",
             Panel_Label="Global Material Consumption",
             Panel="State",
             Indicator_Subcategory="Per Capita",
             Indicator_Unit="Tonnes",
             Data_Source="UN SDG Indicator Bank -- SDG 12.2")

Fact_Context_State_Markets_A <-
  sdg.12.2 %>%
  subset(.,SeriesCode=="EN_MAT_DOMCMPC" & Goal==12 & X.Type.of.product.=="RAW" & GeoAreaName=="World") %>%
  transmute(Year_Key=TimePeriod,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Markets"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_State_Markets_A$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)

# -- GLOBAL MATERIAL CONSUMPTION - PER GDP

Dim_Context_State_Markets_B <- 
  data.frame(Indicator_Type_Key="GCS_MK_B",
             Indicator_Name="Global Material Consumption, per GDP",
             Indicator_Label="Domestic Material Consumption of All Raw Materials",
             Panel_Label="Global Material Consumption",
             Panel="State",
             Indicator_Subcategory="Per GDP",
             Indicator_Unit="Kilograms per constant 2010 USD",
             Data_Source="UN SDG Indicator Bank -- SDG 12.2")

Fact_Context_State_Markets_B <-
  sdg.12.2 %>%
  subset(.,SeriesCode=="EN_MAT_DOMCMPG" & Goal==12 & X.Type.of.product.=="RAW" & GeoAreaName=="World") %>%
  transmute(Year_Key=TimePeriod,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Markets"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_State_Markets_B$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 2.2 Context - Threat ----

# -- DEMAND & TRADE OF ILLEGAL WILDLIFE

Dim_Context_Threat_Markets_A <- 
  data.frame(Indicator_Type_Key="GCT_MK_A",
             Indicator_Name="FORTHCOMING: CITES Illegal Trade & Exploitation Index",
             Indicator_Label="Illegal Trade and Exploitation Index*",
             Panel_Label="Demand & Trade of Illegal Wildlife",
             Panel="State",
             Indicator_Subcategory="(under development by WWF & TRAFFIC)",
             Indicator_Unit="",
             Data_Source="CITES, processed by WWF/TRAFFIC")

Fact_Context_Threat_Markets_A <-
  data.frame(Year_Key=9999,
             Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Markets"],length(1)),
             Indicator_Type_Key=rep(Dim_Context_Threat_Markets_A$Indicator_Type_Key,length(1)),
             Indicator_Value=NA,
             Indicator_Upper_Value=NA,
             Indicator_Lower_Value=NA)


# ---- 2.3 Context - Response ----

# !!!! CONSIDER CHANGING THIS INDICATOR BASED ON forest-trends SUPPLY CHANGE REPORT !!!!
# https://www.forest-trends.org/wp-content/uploads/2018/04/2017SupplyChange_Trackin-Committments.pdf

OECDStat_LandUse <- read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/LandUse_OECDStat_dl_2018_0816.csv')

# -- LAND USE - CROPLAND 

Dim_Context_Response_Markets_A <- 
  data.frame(Indicator_Type_Key="GCR_MK_A",
             Indicator_Name="Land use type (percentage of total land area)",
             Indicator_Label="Type of Use*",
             Panel_Label="Land Use",
             Panel="Threat",
             Indicator_Subcategory="Arable and Cropland",
             Indicator_Unit="% of total land",
             Data_Source="OECDStat, from FAO")

Fact_Context_Response_Markets_A <-
  OECDStat_LandUse %>%
  subset(.,Country=="World" & 
           Year>1989 & 
           grepl("arable", Variable, ignore.case=T) &
           Unit=="Percentage") %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Markets"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_Response_Markets_A$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)

# -- LAND USE - MEADOWS & PASTURE

Dim_Context_Response_Markets_B <- 
  data.frame(Indicator_Type_Key="GCR_MK_B",
             Indicator_Name="Land use (percentage of total land area)",
             Indicator_Label="Type of Use*",
             Panel_Label="Land Use",
             Panel="Threat",
             Indicator_Subcategory="Meadows and Pastures",
             Indicator_Unit="% of total land",
             Data_Source="OECDStat, from FAO")

Fact_Context_Response_Markets_B <-
  OECDStat_LandUse %>%
  subset(.,Country=="World" & 
           Year>1989 & 
           grepl("meadow", Variable, ignore.case=T) &
           Unit=="Percentage") %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Markets"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_Response_Markets_B$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)

# -- LAND USE - FOREST

Dim_Context_Response_Markets_C <- 
  data.frame(Indicator_Type_Key="GCR_MK_C",
             Indicator_Name="Land use (percentage of total land area)",
             Indicator_Label="Type of Use*",
             Panel_Label="Land Use",
             Panel="Threat",
             Indicator_Subcategory="Forest",
             Indicator_Unit="% of total land",
             Data_Source="OECDStat, from FAO")

Fact_Context_Response_Markets_C <-
  OECDStat_LandUse %>%
  subset(.,Country=="World" & 
           Year>1989 & 
           grepl("forest", Variable, ignore.case=T) &
           Unit=="Percentage") %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Markets"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_Response_Markets_C$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)

# -- LAND USE - OTHER

Dim_Context_Response_Markets_D <- 
  data.frame(Indicator_Type_Key="GCR_MK_D",
             Indicator_Name="Land use (percentage of total land area)",
             Indicator_Label="Type of Use*",
             Panel_Label="Land Use",
             Panel="Threat",
             Indicator_Subcategory="Other",
             Indicator_Unit="% of total land",
             Data_Source="OECDStat, from FAO")

Fact_Context_Response_Markets_D <-
  OECDStat_LandUse %>%
  subset(.,Country=="World" & 
           Year>1989 & 
           grepl("other", Variable, ignore.case=T) &
           Unit=="Percentage") %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Markets"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_Response_Markets_D$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 2.4 Consolidated Markets-specific Global Context tables ----

Dim_Context_Markets <- 
  rbind.data.frame(Dim_Context_State_Markets_A,
                   Dim_Context_State_Markets_B,
                   Dim_Context_Threat_Markets_A,
                   Dim_Context_Response_Markets_A,
                   Dim_Context_Response_Markets_B,
                   Dim_Context_Response_Markets_C,
                   Dim_Context_Response_Markets_D)

Fact_Context_Markets <-
  rbind.data.frame(Fact_Context_State_Markets_A,
                   Fact_Context_State_Markets_B,
                   Fact_Context_Threat_Markets_A,
                   Fact_Context_Response_Markets_A,
                   Fact_Context_Response_Markets_B,
                   Fact_Context_Response_Markets_C,
                   Fact_Context_Response_Markets_D)


#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Initiatives ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 3.1 Load data ----

dim.initiatives.markets <- 
  read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/fy18_initiative_reporting_dim_2018_0908.xlsx',sheetName="Sheet1") %>%
  subset(.,Practice=="Markets") 

dim.initiative.indicators.markets <-
  read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/fy18_initiative_indicators_fact_2018_0908.xlsx',sheetName="Sheet1") %>%
  subset(.,Practice=="Markets")


# ---- 3.2 Markets-specific Dim_Initiative ----

Dim_Initiative_Markets <-
  dim.initiatives.markets %>%
  transmute(Initiative_Key=Initiative.key,
            Initiative_Name=Initiative,
            Initiative_Status=Overall.status,
            Initiative_Status_Justification=Overall.just,
            Initiative_Goal=Initiative.statement)


# ---- 3.3 Markets-specific Dim_Initiative_Indicator_Type ----

Dim_Initiative_Indicator_Markets <-
  dim.initiative.indicators.markets %>%
  transmute(Indicator_Type_Key=Initiative.indicator.key,
            Indicator_Type=Indicator.type,
            Indicator_Name=ifelse(!is.na(Indicator.name),as.character(Indicator.name),"FORTHCOMING"),
            Indicator_Label=ifelse(!is.na(Indicator.label),as.character(Indicator.label),"Not Yet Identified"),
            Indicator_Subcategory=Subcategory,
            Indicator_Target=Target,
            Indicator_Unit=Units,
            Data_Source=Source)


# ---- 3.4 Markets-specific Fact_Initiative_Indicators ----

Fact_Initiative_Indicator_Markets <-
  dim.initiative.indicators.markets %>%
  left_join(.,dim.initiatives.markets[,c("Initiative.key","Initiative","Practice.outcome.key")],
            by="Initiative") %>%
  melt(.,measure.vars=c("Baseline.value","Current.value","Target")) %>%
  transmute(Initiative.indicator.key=Initiative.indicator.key,
            Initiative=Initiative,
            Initiative.key=Initiative.key,
            Practice.outcome.key=Practice.outcome.key,
            Year.type=c(rep("Baseline",length(variable[variable=="Baseline.value"])),
                        rep("Current",length(variable[variable=="Current.value"])),
                        rep("Target",length(variable[variable=="Target"]))),
            Year=c(Baseline.year[variable=="Baseline.value"],
                   Current.year[variable=="Current.value"],
                   Target.year[variable=="Target"]),
            Raw.value=c(value[variable=="Baseline.value"],
                        value[variable=="Current.value"],
                        value[variable=="Target"]),
            Raw.baseline.value=rep(value[variable=="Baseline.value"],3),
            Value=ifelse(grepl("% change",Units,ignore.case=T)==T | 
                           grepl("% reduction",Units,ignore.case=T)==T |
                           grepl("% increase",Units,ignore.case=T)==T,
                         ifelse(Year.type=="Baseline" & !is.na(Year),
                                0,
                                ifelse(Year.type=="Current" & Desired.trend=="Down",
                                       (1-(Raw.value/Raw.baseline.value))*100,
                                       ifelse(Year.type=="Current" & Desired.trend=="Up",
                                              ((Raw.value/Raw.baseline.value)-1)*100,
                                              Raw.value))),
                         Raw.value)) %>%
  .[!(is.na(.$Year)==T & .$Year.type=="Current") &
      !(is.na(.$Value)==T & .$Year.type=="Target"),] %>%
  transmute(Year_Key=ifelse(!is.na(Year),Year,9999),
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Markets"],length(Year_Key)),
            Initiative_Key=Initiative.key,
            Indicator_Type_Key=Initiative.indicator.key,
            Practice_Outcome_Key=Practice.outcome.key,
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 3.5 Markets-specific Fact_Initiative_Financials ----

Fact_Initiative_Financials_Markets <-
  dim.initiatives.markets %>%
  transmute(Date_Key=Date,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Markets"],length(Date_Key)),
            Initiative_Key=Initiative.key,
            Amount_needed=Funds.needed,
            Amount_secured=Funds.secured)




# ---- REMOVE CLUTTER ----

rm(sdg.12.2,
   OECDStat_LandUse,
   Dim_Context_State_Markets_A,
   Dim_Context_State_Markets_B,
   Dim_Context_Threat_Markets_A,
   Dim_Context_Response_Markets_A,
   Dim_Context_Response_Markets_B,
   Dim_Context_Response_Markets_C,
   Dim_Context_Response_Markets_D,
   Fact_Context_State_Markets_A,
   Fact_Context_State_Markets_B,
   Fact_Context_Threat_Markets_A,
   Fact_Context_Response_Markets_A,
   Fact_Context_Response_Markets_B,
   Fact_Context_Response_Markets_C,
   Fact_Context_Response_Markets_D,
   dim.initiatives.markets,
   dim.initiative.indicators.markets)
