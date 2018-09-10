# 
# code: Food Practice Indicator and Initiative Tables
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

FAOSTAT_FoodBalance <- read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/FAOSTAT_FoodBalanceSheets_dl_2018_0908.csv')

# -- FOOD SUPPLY - DIETARY ENERGY

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

# -- FOOD SUPPLY - PROTEIN

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

# -- FOOD SUPPLY - FAT

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
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Food"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_Threat_Food_C$Indicator_Type_Key,length(Year_Key)),
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
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Food"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_Threat_Food_D$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)

# ---- 2.3 Context - Response ----

CAIT.emissions <- read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/CAIT Country GHG Emissions.csv')
FAOSTAT_worldpop <- read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/FAOSTAT_world_pop_dl_2018_0908.csv')

# -- EMISSIONS & EFFICIENCY - EMISSIONS

Dim_Context_Response_Food_A <- 
  data.frame(Indicator_Type_Key="GCR_FD_A",
             Indicator_Name="Global GHG emissions from the agricultural sector",
             Indicator_Label="Agricultural GHG Emissions",
             Panel_Label="Emissions & Efficiency",
             Panel="Response",
             Indicator_Subcategory="A",
             Indicator_Unit="Gt CO2e",
             Data_Source="CAIT Climate Data Explorer. 2017. Washington, DC: World Resources Institute")

Fact_Context_Response_Food_A <-
  CAIT.emissions %>%
  subset(.,Country=="World",select=c(1,2,14)) %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Food"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_Response_Food_A$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Agriculture..MtCO2e./1000,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)

# -- EMISSIONS & EFFICIENCY - EFFICIENCY

Dim_Context_Response_Food_B <- 
  data.frame(Indicator_Type_Key="GCR_FD_B",
             Indicator_Name="Millions of kcals produced per ton of GHGs emitted annually",
             Indicator_Label="Efficiency",
             Panel_Label="Emissions & Efficiency",
             Panel="Response",
             Indicator_Subcategory="B",
             Indicator_Unit="Million kcal produced per ton CO2e",
             Data_Source="CAIT Climate Data Explorer (2017); FAO Food Balance Sheets")

Fact_Context_Response_Food_B <-
  left_join(CAIT.emissions[CAIT.emissions$Country=="World" & CAIT.emissions$Year<2014,c(1,2,14)],
            FAOSTAT_FoodBalance[grepl("Food supply",FAOSTAT_FoodBalance$Element,ignore.case=T)==T,c("Element","Year","Unit","Value")],
            by="Year") %>%
  left_join(.,data.frame(Year=FAOSTAT_worldpop[,"Year"],
                         World.Pop=FAOSTAT_worldpop[,"Value"]*1000),
            by="Year") %>%
  mutate(Annual.kcal=(Value*World.Pop*365)/1000000) %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Food"],length(Year)),
            Indicator_Type_Key=rep(Dim_Context_Response_Food_B$Indicator_Type_Key,length(Year)),
            Indicator_Value=Annual.kcal/(Agriculture..MtCO2e.*1000000),
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# plotting.food.response.b <- 
#   Fact_Context_Response_Food_B %>%
#   transmute(Year_Key=Year_Key,
#             Practice_Key=Practice_Key,
#             Indicator_Type_Key=Indicator_Type_Key,
#             Indicator_Value=Indicator_Value*4,
#             Indicator_Upper_Value=Indicator_Upper_Value,
#             Indicator_Lower_Value=Indicator_Lower_Value)
# 
# ggplot(rbind.data.frame(Fact_Context_Response_Food_A,plotting.food.response.b),
#        aes(x=Year_Key,y=Indicator_Value)) +
#   geom_line(aes(group=Indicator_Type_Key,colour=Indicator_Type_Key)) +
#   scale_colour_manual(values=c("brown","dark green"),
#                       labels=c("Emissions","Efficiency")) +
#   scale_x_continuous(breaks=c(1990, 1995, 2000, 2005, 2010, 2014),
#                        expand=c(0,0)) +
#     plot.theme + labs(x="Year",y="Emissions")


# ---- 2.4 Consolidated Food-specific Global Context tables ----

Dim_Context_Food <- 
  rbind.data.frame(Dim_Context_State_Food_A,
                   Dim_Context_State_Food_B,
                   Dim_Context_State_Food_C,
                   Dim_Context_Threat_Food_A,
                   Dim_Context_Threat_Food_B,
                   Dim_Context_Threat_Food_C,
                   Dim_Context_Threat_Food_D,
                   Dim_Context_Response_Food_A,
                   Dim_Context_Response_Food_B)

Fact_Context_Food <-
  rbind.data.frame(Fact_Context_State_Food_A,
                   Fact_Context_State_Food_B,
                   Fact_Context_State_Food_C,
                   Fact_Context_Threat_Food_A,
                   Fact_Context_Threat_Food_B,
                   Fact_Context_Threat_Food_C,
                   Fact_Context_Threat_Food_D,
                   Fact_Context_Response_Food_A,
                   Fact_Context_Response_Food_B)


#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Global 2030 Outcomes ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 3.1 Food Outcome 1 - SUSTAINABLE LAND MANAGEMENT ----

# -- LAND DEGRADATION

Dim_Global_2030_Outcome1_Food_A <- 
  data.frame(Indicator_Type_Key="OUT1_FD_A",
             Indicator_Name="FORTHCOMING: SDG 15.3.1 - Proportion of land that is degraded over total land area",
             Indicator_Label="Land Degradation",
             Indicator_Subcategory="A",
             Indicator_Unit="% of land degraded",
             Data_source="UN SDG Indicator Bank -- UNCCD, FAO, UNSD, UNEP, UNFCCC, CBD",
             Indicator_Target=NA,
             Indicator_Type="Outcome",
             Panel_Label="Sustainable Land Management",
             Display_Order=1)

Fact_Global_2030_Outcome1_Food_A <-
  data.frame(Year_Key=NA,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Food"],length(1)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome1_Food_A$Indicator_Type_Key, length(1)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Food" &
                                                                   grepl("Production",practice_outcome_key_ref$practice_outcome)], length(1)),
            Indicator_Value=NA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)

# -- SUSTAINABLE AGRICULTURE

Dim_Global_2030_Outcome1_Food_B <- 
  data.frame(Indicator_Type_Key="OUT1_FD_B",
             Indicator_Name="FORTHCOMING: SDG 2.4.1 - Proportion of agricultural land under productive and sustainable agriculture",
             Indicator_Label="Sustainable Agricultue",
             Indicator_Subcategory="B",
             Indicator_Unit="% of agricultural land sustainable",
             Data_source="UN SDG Indicator Bank -- FAO",
             Indicator_Target=NA,
             Indicator_Type="Outcome",
             Panel_Label="Sustainable Land Management",
             Display_Order=1)

Fact_Global_2030_Outcome1_Food_B <-
  data.frame(Year_Key=NA,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Food"],length(1)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome1_Food_B$Indicator_Type_Key, length(1)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Food" &
                                                                   grepl("Production",practice_outcome_key_ref$practice_outcome)], length(1)),
            Indicator_Value=NA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 3.2 Food Outcome 2 - FOOD LOSS & WASTE ----

# -- LOSS

Dim_Global_2030_Outcome2_Food_A <- 
  data.frame(Indicator_Type_Key="OUT2_FD_A",
             Indicator_Name="FORTHCOMING: SDG 12.3.1.b -- Global Food Loss Index (GFLI)",
             Indicator_Label="Global Food Loss Index",
             Indicator_Subcategory="A",
             Indicator_Unit="Index",
             Data_source="UN SDG Indicator Bank -- FAO",
             Indicator_Target=NA,
             Indicator_Type="Outcome",
             Panel_Label="Food Loss & Waste",
             Display_Order=2)

Fact_Global_2030_Outcome2_Food_A <-
  data.frame(Year_Key=NA,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Food"],length(1)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome2_Food_A$Indicator_Type_Key, length(1)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Food" &
                                                                   grepl("Waste",practice_outcome_key_ref$practice_outcome)], length(1)),
            Indicator_Value=NA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)

# -- WASTE

Dim_Global_2030_Outcome2_Food_B <- 
  data.frame(Indicator_Type_Key="OUT2_FD_B",
             Indicator_Name="FORTHCOMING: SDG 12.3.1.a -- Per capita food waste (kg/year)",
             Indicator_Label="Per Capita Food Waste",
             Indicator_Subcategory="B",
             Indicator_Unit="kg/year",
             Data_source="UN SDG Indicator Bank -- UNEP",
             Indicator_Target=NA,
             Indicator_Type="Outcome",
             Panel_Label="Food Loss & Waste",
             Display_Order=2)

Fact_Global_2030_Outcome2_Food_B <-
  data.frame(Year_Key=NA,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Food"],length(1)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome2_Food_B$Indicator_Type_Key, length(1)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Food" &
                                                                   grepl("Waste",practice_outcome_key_ref$practice_outcome)], length(1)),
            Indicator_Value=NA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 3.3 Food Outcome 3 - HEALTHY DIETS ----

Dim_Global_2030_Outcome3_Food_A <- 
  data.frame(Indicator_Type_Key="OUT3_FD_A",
             Indicator_Name="FORTHCOMING: Actual global food plate (FAO Food Balance sheets) compared to Harvard diet",
             Indicator_Label="Global Food Plate",
             Indicator_Subcategory="A",
             Indicator_Unit="% matched to Harvard Diet",
             Data_source="Developed and analyzed by WWF's Food Practice -- in collaboation with WWF-UK, Tanya Steele",
             Indicator_Target=NA,
             Indicator_Type="Outcome",
             Panel_Label="Healthy Diets",
             Display_Order=3)

Fact_Global_2030_Outcome3_Food_A <-
  data.frame(Year_Key=NA,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Food"],length(1)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome3_Food_A$Indicator_Type_Key, length(1)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Food" &
                                                                   grepl("Diets",practice_outcome_key_ref$practice_outcome)], length(1)),
            Indicator_Value=NA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 3.4 Consolidated Food-specific Global 2030 Outcome tables ----

Dim_Global_2030_Outcome_Food <-
  rbind.data.frame(Dim_Global_2030_Outcome1_Food_A,
                   Dim_Global_2030_Outcome1_Food_B,
                   Dim_Global_2030_Outcome2_Food_A,
                   Dim_Global_2030_Outcome2_Food_B,
                   Dim_Global_2030_Outcome3_Food_A)

Fact_Global_2030_Outcome_Food <-
  rbind.data.frame(Fact_Global_2030_Outcome1_Food_A,
                   Fact_Global_2030_Outcome1_Food_B,
                   Fact_Global_2030_Outcome2_Food_A,
                   Fact_Global_2030_Outcome2_Food_B,
                   Fact_Global_2030_Outcome3_Food_A)


#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: Initiatives ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 4.1 Load data ----

dim.initiatives.food <- 
  read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/fy18_initiative_reporting_dim_2018_0908.xlsx',sheetName="Sheet1") %>%
  subset(.,Practice=="Food") 

dim.initiative.indicators.food <-
  read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/fy18_initiative_indicators_fact_2018_0908.xlsx',sheetName="Sheet1") %>%
  subset(.,Practice=="Food")


# ---- 4.2 Food-specific Dim_Initiative ----

Dim_Initiative_Food <-
  dim.initiatives.food %>%
  transmute(Initiative_Key=Initiative.key,
            Initiative_Name=Initiative,
            Initiative_Status=Overall.status,
            Initiative_Status_Justification=Overall.just,
            Initiative_Goal=Initiative.statement)
  

# ---- 4.3 Food-specific Dim_Initiative_Indicator_Type ----

Dim_Initiative_Indicator_Food <-
  dim.initiative.indicators.food %>%
  transmute(Indicator_Type_Key=Initiative.indicator.key,
            Indicator_Type=Indicator.type,
            Indicator_Name=ifelse(!is.na(Indicator.name),as.character(Indicator.name),"FORTHCOMING"),
            Indicator_Label=Indicator.label,
            Indicator_Subcategory=Subcategory,
            Indicator_Target=Target.value,
            Indicator_Unit=Units,
            Data_Source=Source)


# ---- 4.4 Food-specific Fact_Initiative_Indicators ----

Fact_Initiative_Indicator_Food <-
  left_join(dim.initiative.indicators.food,dim.initiatives.food,by="Initiative") %>%
  transmute(Year_Key=NA,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Food"],length(Year_Key)),
            Initiative_Key=Initiative.key,
            Indicator_Type_Key=Initiative.indicator.key,
            Practice_Outcome_Key=Practice.outcome.key,
            Indicator_Value=NA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 4.5 Food-specific Fact_Initiative_Financials ----

Fact_Initiative_Financials_Food <-
  dim.initiatives.food %>%
  transmute(Date_Key=Date,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Food"],length(Date_Key)),
            Initiative_Key=Initiative.key,
            Amount_needed=NA,
            Amount_secured=NA)



# ---- REMOVE CLUTTER ----

rm(FAOSTAT_FoodBalance,
   OECDStat_LandUse,
   CAIT.emissions,
   FAOSTAT_worldpop,
   Dim_Context_State_Food_A,
   Dim_Context_State_Food_B,
   Dim_Context_State_Food_C,
   Dim_Context_Threat_Food_A,
   Dim_Context_Threat_Food_B,
   Dim_Context_Threat_Food_C,
   Dim_Context_Threat_Food_D,
   Dim_Context_Response_Food_A,
   Dim_Context_Response_Food_B,
   Fact_Context_State_Food_A,
   Fact_Context_State_Food_B,
   Fact_Context_State_Food_C,
   Fact_Context_Threat_Food_A,
   Fact_Context_Threat_Food_B,
   Fact_Context_Threat_Food_C,
   Fact_Context_Threat_Food_D,
   Fact_Context_Response_Food_A,
   Fact_Context_Response_Food_B,
   Dim_Global_2030_Outcome1_Food_A,
   Dim_Global_2030_Outcome1_Food_B,
   Dim_Global_2030_Outcome2_Food_A,
   Dim_Global_2030_Outcome2_Food_B,
   Dim_Global_2030_Outcome3_Food_A,
   Fact_Global_2030_Outcome1_Food_A,
   Fact_Global_2030_Outcome1_Food_B,
   Fact_Global_2030_Outcome2_Food_A,
   Fact_Global_2030_Outcome2_Food_B,
   Fact_Global_2030_Outcome3_Food_A,
   dim.initiatives.food,
   dim.initiative.indicators.food)