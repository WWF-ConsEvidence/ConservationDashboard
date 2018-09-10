# 
# code: Freshwater Practice Indicator and Initiative Tables
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: August 2018
# modified: 
# 
# ---- inputs ----
#  1) Freshwater-specific data tables (in 1_Nov2018/2_FlatDataFiles/ConsDB_Input)
# 
# ---- outputs ----
#  1) Freshwater-specific back-end tables -- ready to be consolidated with other Practices, to go into back-end:
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

# source('1_Nov2018/1_Practices/')

# -- FRESHWATER BIODIVERSITY

Dim_Context_State_FW_A <- 
  data.frame(Indicator_Type_Key="GCS_FW_A",
             Indicator_Name="Freshwater Living Planet Index",
             Indicator_Label="Freshwater Living Planet Index",
             Panel_Label="Freshwater Biodiversity",
             Panel="State",
             Indicator_Subcategory="A",
             Indicator_Unit="Index",
             Data_Source="Zoological Society of London; 2016 Living Planet Index database")

Fact_Context_State_FW_A <-
  read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/FW_LPI_output_2018_0910.csv') %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Freshwater"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_State_FW_A$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=LPI_final,
            Indicator_Upper_Value=CI_high,
            Indicator_Lower_Value=CI_low)


# ---- 2.2 Context - Threat ----

# -- WATER INFRASTRUCTURE DEVELOPMENT

Dim_Context_Threat_FW_A <- 
  data.frame(Indicator_Type_Key="GCT_FW_A",
             Indicator_Name="Connectivity Status Index (% of total watersheds that reach 95% connectivity threshold)",
             Indicator_Label="Connectivity Status Index",
             Panel_Label="Water Infrastructure Development",
             Panel="Threat",
             Indicator_Subcategory="A",
             Indicator_Unit="% of total watersheds",
             Data_Source="WWF/McGill")

Fact_Context_Threat_FW_A <-
  data.frame(Year_Key=NA,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Freshwater"],length(1)),
            Indicator_Type_Key=rep(Dim_Context_Threat_FW_A$Indicator_Type_Key,length(1)),
            Indicator_Value=NA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 2.3 Context - Response ----

# -- WATER STEWARDSHIP & GOVERNANCE

Dim_Context_Response_FW_A <- 
  data.frame(Indicator_Type_Key="GCR_FW_A",
             Indicator_Name="Degree of IWRM implementation (scores range from 1-100)",
             Indicator_Label="Integrated Water Resource Management Implementation",
             Panel_Label="Water Stewardship & Governance",
             Panel="Response",
             Indicator_Subcategory="A",
             Indicator_Unit="% of countries with high degree of implementation",
             Data_Source="UN SDG Indicator bank - SDG 6.5.1 - UNEP")

Fact_Context_Response_FW_A <-
  read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/SDG_6.5.1_iwrm_implementation_dl_2018_0905.csv') %>%
  group_by(TimePeriod) %>%
  summarise(Value=length(GeoAreaName[Value>=80])/length(GeoAreaName[!is.na(Value)])) %>%
  transmute(Year_Key=TimePeriod,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Freshwater"],length(TimePeriod)),
            Indicator_Type_Key=rep(Dim_Context_Response_FW_A$Indicator_Type_Key,length(TimePeriod)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 2.4 Consolidated Freshwater-specific Global Context tables ----

Dim_Context_FW <- 
  rbind.data.frame(Dim_Context_State_FW_A,
                   Dim_Context_Threat_FW_A,
                   Dim_Context_Response_FW_A)

Fact_Context_FW <-
  rbind.data.frame(Fact_Context_State_FW_A,
                   Fact_Context_Threat_FW_A,
                   Fact_Context_Response_FW_A)


#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Global 2030 Outcomes ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 3.1 Freshwater Outcome 1 - HEALTHY HABITATS & SPECIES ----

# -- STABLE OR INCREASING POPULATIONS

Dim_Global_2030_Outcome1_FW_A <- 
  data.frame(Indicator_Type_Key="OUT1_FW_A",
             Indicator_Name="Proportion of freshwater species with stablized or increasing populations (of those species represented in LPI database)",
             Indicator_Label="Stable or Increasing Populations",
             Indicator_Subcategory="A",
             Indicator_Unit="% freshwater species",
             Data_source="Zoological Society of London, Living Planet Index Database",
             Indicator_Target=NA,
             Indicator_Type="Outcome",
             Panel_Label="Healthy Habitats & Species",
             Display_Order=1)

Fact_Global_2030_Outcome1_FW_A <-
  data.frame(Year_Key=NA,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Freshwater"],length(1)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome1_FW_A$Indicator_Type_Key, length(1)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Freshwater" &
                                                                   grepl("Habitats",practice_outcome_key_ref$practice_outcome)], length(1)),
            Indicator_Value=NA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)

# -- RAMSAR-PROTECTED HABITAT

Dim_Global_2030_Outcome1_FW_B <- 
  data.frame(Indicator_Type_Key="OUT1_FW_B",
             Indicator_Name="Area under Ramsar designation (M ha)",
             Indicator_Label="Ramsar-Protected Habitat",
             Indicator_Subcategory="B",
             Indicator_Unit="M ha",
             Data_source="Zoological Society of London, Living Planet Index Database",
             Indicator_Target=NA,
             Indicator_Type="Outcome",
             Panel_Label="Healthy Habitats & Species",
             Display_Order=1)

Fact_Global_2030_Outcome1_FW_B <-
  data.frame(Year_Key=NA,
             Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Freshwater"],length(1)),
             Indicator_Type_Key=rep(Dim_Global_2030_Outcome1_FW_B$Indicator_Type_Key, length(1)),
             Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Freshwater" &
                                                                    grepl("Habitats",practice_outcome_key_ref$practice_outcome)], length(1)),
             Indicator_Value=NA,
             Indicator_Upper_Value=NA,
             Indicator_Lower_Value=NA)


# ---- 3.2 Freshwater Outcome 2 - CLEAN FLOWING RIVERS ----

Dim_Global_2030_Outcome2_FW_A <- 
  data.frame(Indicator_Type_Key="OUT2_FW_A",
             Indicator_Name="Lengths of river secured through the removal / avoidance of instrastructure (using Connectivity Status Index data)",
             Indicator_Label="Protected & Restored Rivers",
             Indicator_Subcategory="A",
             Indicator_Unit="Km",
             Data_source="WWF/McGill",
             Indicator_Target=NA,
             Indicator_Type="Outcome",
             Panel_Label="Clean Flowing Rivers",
             Display_Order=2)

Fact_Global_2030_Outcome2_FW_A <-
  data.frame(Year_Key=NA,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Freshwater"],length(1)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome2_FW_A$Indicator_Type_Key, length(1)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Freshwater" &
                                                                   grepl("Rivers",practice_outcome_key_ref$practice_outcome)], length(1)),
            Indicator_Value=NA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 3.3 Consolidated Freshwater-specific Global 2030 Outcome tables ----

Dim_Global_2030_Outcome_FW <-
  rbind.data.frame(Dim_Global_2030_Outcome1_FW_A,
                   Dim_Global_2030_Outcome1_FW_B,
                   Dim_Global_2030_Outcome2_FW_A)

Fact_Global_2030_Outcome_FW <-
  rbind.data.frame(Fact_Global_2030_Outcome1_FW_A,
                   Fact_Global_2030_Outcome1_FW_B,
                   Fact_Global_2030_Outcome2_FW_A)


#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: Initiatives ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 4.1 Load data ----

dim.initiatives.fw <- 
  read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/fy18_initiative_reporting_dim_2018_0908.xlsx',sheetName="Sheet1") %>%
  subset(.,Practice=="Freshwater") 

dim.initiative.indicators.fw <-
  read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/fy18_initiative_indicators_fact_2018_0908.xlsx',sheetName="Sheet1") %>%
  subset(.,Practice=="Freshwater")


# ---- 4.2 Freshwater-specific Dim_Initiative ----

Dim_Initiative_FW <-
  dim.initiatives.fw %>%
  transmute(Initiative_Key=Initiative.key,
            Initiative_Name=Initiative,
            Initiative_Status=Overall.status,
            Initiative_Status_Justification=Overall.just,
            Initiative_Goal=Initiative.statement)


# ---- 4.3 Freshwater-specific Dim_Initiative_Indicator_Type ----

Dim_Initiative_Indicator_FW <-
  dim.initiative.indicators.fw %>%
  transmute(Indicator_Type_Key=Initiative.indicator.key,
            Indicator_Type=Indicator.type,
            Indicator_Name=ifelse(!is.na(Indicator.name),as.character(Indicator.name),"FORTHCOMING"),
            Indicator_Label=Indicator.label,
            Indicator_Subcategory=Subcategory,
            Indicator_Target=Target.value,
            Indicator_Unit=Units,
            Data_Source=Source)


# ---- 4.4 Freshwater-specific Fact_Initiative_Indicators ----

Fact_Initiative_Indicator_FW <-
  left_join(dim.initiative.indicators.fw,dim.initiatives.fw,by="Initiative") %>%
  transmute(Year_Key=NA,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Freshwater"],length(Year_Key)),
            Initiative_Key=Initiative.key,
            Indicator_Type_Key=Initiative.indicator.key,
            Practice_Outcome_Key=Practice.outcome.key,
            Indicator_Value=NA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 4.5 Freshwater-specific Fact_Initiative_Financials ----

Fact_Initiative_Financials_FW <-
  dim.initiatives.fw %>%
  transmute(Date_Key=Date,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Freshwater"],length(Date_Key)),
            Initiative_Key=Initiative.key,
            Amount_needed=Funds.needed,
            Amount_secured=Funds.secured)



# ---- REMOVE CLUTTER ----

rm(Dim_Context_State_FW_A,
   Dim_Context_Threat_FW_A,
   Dim_Context_Response_FW_A,
   Fact_Context_State_FW_A,
   Fact_Context_Threat_FW_A,
   Fact_Context_Response_FW_A,
   Dim_Global_2030_Outcome1_FW_A,
   Dim_Global_2030_Outcome1_FW_B,
   Dim_Global_2030_Outcome2_FW_A,
   Fact_Global_2030_Outcome1_FW_A,
   Fact_Global_2030_Outcome1_FW_B,
   Fact_Global_2030_Outcome2_FW_A,
   dim.initiatives.fw,
   dim.initiative.indicators.fw)