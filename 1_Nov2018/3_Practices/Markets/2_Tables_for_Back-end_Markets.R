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
             Indicator_Unit="Kg per constant 2010 USD",
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
             Panel="Threat",
             Indicator_Subcategory=NA,
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

# -- LAND CONVERSION - COMMODITY DRIVEN DEFORESTATION

Dim_Context_Response_Markets_A <- 
  data.frame(Indicator_Type_Key="GCR_MK_A",
             Indicator_Name="Commodity Driven Deforestation (M ha per year)",
             Indicator_Label="Commodity Driven Deforestation & Deforestation-Reduction Commitments",
             Panel_Label="Land Conversion",
             Panel="Response",
             Indicator_Subcategory="Deforestation",
             Indicator_Unit="M ha per year",
             Data_Source="Global Forest Watch - Curtis et al (2018) Classifying global drivers of forest loss")

Fact_Context_Response_Markets_A <-
  read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/GFW_treeloss_bydriver_2018_0919.xlsx', sheetName="Sheet1") %>%
  subset(.,Loss_type=="Commodity Driven Deforestation") %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Markets"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_Response_Markets_A$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)

# -- LAND CONVERSION - DEFORESTATION-FREE COMMITMENTS

Dim_Context_Response_Markets_B <- 
  data.frame(Indicator_Type_Key="GCR_MK_B",
             Indicator_Name="Deforestation-Free Commitments (% 'exposed' companies with commitments)",
             Indicator_Label="Commodity Driven Deforestation & Deforestation-Reduction Commitments",
             Panel_Label="Land Conversion",
             Panel="Response",
             Indicator_Subcategory="Commitments",
             Indicator_Unit="% commodity-driven companies", 
             # commodity-driven companies included are those that produce, procure, or use soy, palm, timber & pulp, or cattle products as part of core business
             Data_Source="Supply Change 2017 Report -- forest-trends.org")

Fact_Context_Response_Markets_B <-
  data.frame(Year_Key=c(2016,2017),
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Markets"],2),
            Indicator_Type_Key=rep(Dim_Context_Response_Markets_B$Indicator_Type_Key,2),
            Indicator_Value=c((366/566)*100,(447/718)*100), 
            # proportion has gone down between years, largely because there was a concerted effort to get more cattle-related companies, who have fewer commitments in general
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 2.4 Consolidated Markets-specific Global Context tables ----

Dim_Context_Markets <- 
  rbind.data.frame(Dim_Context_State_Markets_A,
                   Dim_Context_State_Markets_B,
                   Dim_Context_Threat_Markets_A,
                   Dim_Context_Response_Markets_A,
                   Dim_Context_Response_Markets_B)

Fact_Context_Markets <-
  rbind.data.frame(Fact_Context_State_Markets_A,
                   Fact_Context_State_Markets_B,
                   Fact_Context_Threat_Markets_A,
                   Fact_Context_Response_Markets_A,
                   Fact_Context_Response_Markets_B)


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

dim.initiative.milestones.markets <-
  read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/fy18_initiative_milestones_2018_0908.csv') %>%
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
            Data_Source=Source,
            Display_Order=Display.order)


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


# ---- 3.6 Markets-specific Milestone_Group_Bridge ----

Milestone_Group_Bridge_Markets <-
  left_join(dim.initiative.milestones.markets, dim.initiatives.markets, by=c("Initiative", "Practice")) %>%
  transmute(Milestone_Key=Milestone.key,
            Initiative_Key=Initiative.key)


# ---- 3.7 Markets-specific Dim_Milestone ----

Dim_Milestone_Markets <-
  dim.initiative.milestones.markets %>%
  transmute(Milestone_Key=Milestone.key,
            Milestone_Name=Milestone,
            Milestone_Target=Target,
            Milestone_Status=Status,
            Milestone_Status_Justification=Status.just)



# ---- REMOVE CLUTTER ----

rm(sdg.12.2,
   Dim_Context_State_Markets_A,
   Dim_Context_State_Markets_B,
   Dim_Context_Threat_Markets_A,
   Dim_Context_Response_Markets_A,
   Dim_Context_Response_Markets_B,
   Fact_Context_State_Markets_A,
   Fact_Context_State_Markets_B,
   Fact_Context_Threat_Markets_A,
   Fact_Context_Response_Markets_A,
   Fact_Context_Response_Markets_B,
   dim.initiatives.markets,
   dim.initiative.indicators.markets,
   dim.initiative.milestones.markets)
