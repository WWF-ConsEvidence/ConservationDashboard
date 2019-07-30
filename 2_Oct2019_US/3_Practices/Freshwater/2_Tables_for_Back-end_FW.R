# 
# code: Freshwater Practice Indicator and Initiative Tables FOR 2019 US DASHBOARD
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: August 2018
# modified: July 2019
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

pacman::p_load(dplyr, xlsx, reshape2)


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
             Indicator_Label="Freshwater Living Planet Index (LPI)",
             Panel_Label="Freshwater Biodiversity",
             Panel="State",
             Indicator_Subcategory=NA,
             Indicator_Unit="Index",
             Data_Source="Zoological Society of London; 2016 Living Planet Index database",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_State_FW_A <-
  read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/FW_LPI_output_2018_0910.csv') %>%
  subset(.,Year>1994 & Year<2016) %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Freshwater"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_State_FW_A$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=LPI_final,
            Indicator_Upper_Value=CI_high,
            Indicator_Lower_Value=CI_low)


# ---- 2.2 Context - Threat ----

# -- WATER INFRASTRUCTURE DEVELOPMENT - LENGTH FREE FLOWING

Dim_Context_Threat_FW_A <- 
  data.frame(Indicator_Type_Key="GCT_FW_A",
             Indicator_Name="Connectivity Status Index (% of total watersheds that reach 95% connectivity threshold)",
             Indicator_Label="Free-Flowing Rivers (Connectivity Status Index)",
             Panel_Label="Water Infrastructure Development",
             Panel="Threat",
             Indicator_Subcategory=NA,
             Indicator_Unit="% of global river length",
             Data_Source="WWF/McGill -- value provided by Michele Thieme, WWF-US",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_Threat_FW_A <-
  data.frame(Year_Key=2018,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Freshwater"],length(1)),
            Indicator_Type_Key=rep(Dim_Context_Threat_FW_A$Indicator_Type_Key,length(1)),
            Indicator_Value=(9971600/11720000)*100,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)

# # -- WATER INFRASTRUCTURE DEVELOPMENT - TOTAL LENGTH
# 
# Dim_Context_Threat_FW_B <- 
#   data.frame(Indicator_Type_Key="GCT_FW_B",
#              Indicator_Name="Connectivity Status Index (% of total watersheds that reach 95% connectivity threshold)",
#              Indicator_Label="Length of Free-Flowing Rivers (Connectivity Status Index)",
#              Panel_Label="Water Infrastructure Development",
#              Panel="Threat",
#              Indicator_Subcategory="Total",
#              Indicator_Unit="Kilometers",
#              Data_Source="WWF/McGill -- value provided by Michele Thieme, WWF-US")
# 
# Fact_Context_Threat_FW_B <-
#   data.frame(Year_Key=2018,
#              Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Freshwater"],length(1)),
#              Indicator_Type_Key=rep(Dim_Context_Threat_FW_B$Indicator_Type_Key,length(1)),
#              Indicator_Value=11720000,
#              Indicator_Upper_Value=NA,
#              Indicator_Lower_Value=NA)

# ---- 2.3 Context - Response ----

# -- WATER STEWARDSHIP & GOVERNANCE

Dim_Context_Response_FW_A <- 
  data.frame(Indicator_Type_Key="GCR_FW_A",
             Indicator_Name="Degree of IWRM implementation (scores range from 1-100)",
             Indicator_Label="Integrated Water Resource Management Implementation",
             Panel_Label="Water Stewardship & Governance",
             Panel="Response",
             Indicator_Subcategory=NA,
             Indicator_Unit="% of countries with high implementation",
             Data_Source="UN SDG Indicator bank - SDG 6.5.1 - UNEP",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_Response_FW_A <-
  read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/SDG_6.5.1_iwrm_implementation_dl_2018_0905.csv') %>%
  group_by(TimePeriod) %>%
  summarise(Value=length(GeoAreaName[Value>=80])/length(GeoAreaName[!is.na(Value)])) %>%
  transmute(Year_Key=TimePeriod,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Freshwater"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_Response_FW_A$Indicator_Type_Key,length(Year_Key)),
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
             Indicator_Name="Trend of freshwater LPI since baseline of 2015",
             Indicator_Label="Species Recovery & Habitat Protection",
             Indicator_Subcategory="Stable or Increasing Populations",
             Indicator_Unit="Change in FW LPI since 2015",
             Data_source="Zoological Society of London, Living Planet Index Database",
             Indicator_Target=NA,
             Indicator_Type="Outcome",
             Panel_Label="Healthy Habitats & Species",
             Display_Order=1,
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Global_2030_Outcome1_FW_A <-
  data.frame(Year_Key=2015,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Freshwater"],length(1)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome1_FW_A$Indicator_Type_Key, length(1)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Freshwater" &
                                                                   grepl("Habitats",practice_outcome_key_ref$practice_outcome)], length(1)),
            Indicator_Value=0,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)

# - RIVER PROTECTION

Dim_Global_2030_Outcome1_FW_B <- 
  data.frame(Indicator_Type_Key="OUT1_FW_B",
             Indicator_Name="Length of river brought under protection (i.e., all river length within PAs)",
             Indicator_Label="Species Recovery & Habitat Protection",
             Indicator_Subcategory="River Protection",
             Indicator_Unit="% of global river length",
             Data_source="WWF/McGill - Abell et al (2017) Looking Beyond the Fenceline - Assessing Protection Gaps for the World's Rivers",
             Indicator_Target=32,
             Indicator_Type="Outcome",
             Panel_Label="Healthy Habitats & Species",
             Display_Order=1,
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Global_2030_Outcome1_FW_B <-
  data.frame(Year_Key=c(2017,2030),
             Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Freshwater"],2),
             Indicator_Type_Key=rep(Dim_Global_2030_Outcome1_FW_B$Indicator_Type_Key, 2),
             Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Freshwater" &
                                                                    grepl("Habitats",practice_outcome_key_ref$practice_outcome)], 2),
             Indicator_Value=c(16.0,Dim_Global_2030_Outcome1_FW_B$Indicator_Target),
             Indicator_Upper_Value=NA,
             Indicator_Lower_Value=NA)

# -- RAMSAR-PROTECTED HABITAT

Dim_Global_2030_Outcome1_FW_C <- 
  data.frame(Indicator_Type_Key="OUT1_FW_C",
             Indicator_Name="Area under Ramsar designation (M ha)",
             Indicator_Label="Species Recovery & Habitat Protection",
             Indicator_Subcategory="Ramsar Site Area",
             Indicator_Unit="M ha",
             Data_source="Zoological Society of London, Living Planet Index Database",
             Indicator_Target=NA,
             Indicator_Type="Outcome",
             Panel_Label="Healthy Habitats & Species",
             Display_Order=1,
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Global_2030_Outcome1_FW_C <-
  read.csv('2_Oct2019_US/2_FlatDataFiles/ConsDB_Input_2019/Ramsar_sitedata_dl_2019_0710.csv') %>%
  select(.,c("Designation.date","Area..ha.")) %>%
  mutate(Date=as.character(Designation.date),
         Year=as.numeric(substr(Date,(nchar(Date)-4)+1,nchar(Date)))) %>%
  group_by(Year) %>%
  summarise(Area=sum(Area..ha.)) %>%
  transmute(Year_Key=Year,
             Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Freshwater"],length(Year_Key)),
             Indicator_Type_Key=rep(Dim_Global_2030_Outcome1_FW_C$Indicator_Type_Key, length(Year_Key)),
             Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Freshwater" &
                                                                    grepl("Habitats",practice_outcome_key_ref$practice_outcome)], length(Year_Key)),
             Indicator_Value=cumsum(Area)/1000000,
             Indicator_Upper_Value=NA,
             Indicator_Lower_Value=NA)

# Target value for this indicator is "double protected habitat from 2017 baseline", so we will calculate the target and 
# append the Dim table, using the Ramsar data source imported for the Fact table

Dim_Global_2030_Outcome1_FW_C$Indicator_Target <- 
  2*Fact_Global_2030_Outcome1_FW_C$Indicator_Value[Fact_Global_2030_Outcome1_FW_C$Year_Key==2017 &
                                                     !is.na(Fact_Global_2030_Outcome1_FW_C$Year_Key)]

# Add target value to Fact table

Fact_Global_2030_Outcome1_FW_C <-
  rbind.data.frame(Fact_Global_2030_Outcome1_FW_C,
                   data.frame(Year_Key=2030,
                              Practice_Key=practice_key_ref$id[practice_key_ref$practice_name=="Freshwater"],
                              Indicator_Type_Key=Dim_Global_2030_Outcome1_FW_C$Indicator_Type_Key,
                              Practice_Outcome_Key=practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Freshwater" &
                                                                                 grepl("Habitats",practice_outcome_key_ref$practice_outcome)],
                              Indicator_Value=Dim_Global_2030_Outcome1_FW_C$Indicator_Target,
                              Indicator_Upper_Value=NA,
                              Indicator_Lower_Value=NA))


# ---- 3.2 Freshwater Outcome 2 - CLEAN FLOWING RIVERS ----

# -- INTEGRITY

Dim_Global_2030_Outcome2_FW_A <- 
  data.frame(Indicator_Type_Key="OUT2_FW_A",
             Indicator_Name="Connectivity Status Index (% of total watersheds that reach 95% connectivity threshold)",
             Indicator_Label="Improved Integrity & Quality",
             Indicator_Subcategory="Hydrological Integrity",
             Indicator_Unit="Kilometers free-flowing river",
             Data_source="WWF/McGill -- value provided by Michele Thieme, WWF-US",
             Indicator_Target=NA,
             Indicator_Type="Outcome",
             Panel_Label="Clean Flowing Rivers",
             Display_Order=2,
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Global_2030_Outcome2_FW_A <-
  data.frame(Year_Key=2018,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Freshwater"],length(1)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome2_FW_A$Indicator_Type_Key, length(1)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Freshwater" &
                                                                   grepl("Rivers",practice_outcome_key_ref$practice_outcome)], length(1)),
            Indicator_Value=9971600,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# -- QUALITY

Dim_Global_2030_Outcome2_FW_B <- 
  data.frame(Indicator_Type_Key="OUT2_FW_B",
             Indicator_Name="Ambient water quality of rivers",
             Indicator_Label="Improved Integrity & Quality",
             Indicator_Subcategory="Ambient Water Quality*",
             Indicator_Unit="% river water bodies with good quality",
             Data_source="UN - SDG Indicator Bank (SDG 6.3.2)",
             Indicator_Target=NA,
             Indicator_Type="Outcome",
             Panel_Label="Clean Flowing Rivers",
             Display_Order=2,
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Global_2030_Outcome2_FW_B <-
  data.frame(Year_Key=9999,
             Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Freshwater"],length(1)),
             Indicator_Type_Key=rep(Dim_Global_2030_Outcome2_FW_B$Indicator_Type_Key, length(1)),
             Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Freshwater" &
                                                                    grepl("Rivers",practice_outcome_key_ref$practice_outcome)], length(1)),
             Indicator_Value=NA,
             Indicator_Upper_Value=NA,
             Indicator_Lower_Value=NA)


# ---- 3.3 Consolidated Freshwater-specific Global 2030 Outcome tables ----

Dim_Global_2030_Outcome_FW <-
  rbind.data.frame(Dim_Global_2030_Outcome1_FW_A,
                   Dim_Global_2030_Outcome1_FW_B,
                   Dim_Global_2030_Outcome1_FW_C,
                   Dim_Global_2030_Outcome2_FW_A,
                   Dim_Global_2030_Outcome2_FW_B)

Fact_Global_2030_Outcome_FW <-
  rbind.data.frame(Fact_Global_2030_Outcome1_FW_A,
                   Fact_Global_2030_Outcome1_FW_B,
                   Fact_Global_2030_Outcome1_FW_C,
                   Fact_Global_2030_Outcome2_FW_A,
                   Fact_Global_2030_Outcome2_FW_B)


#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: Initiatives ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 4.1 Load data ----

dim.initiatives.fw <- 
  read.xlsx('2_Oct2019_US/2_FlatDataFiles/ConsDB_Input_2019/fy19_initiative_reporting_dim_2019_0715.xlsx',sheetName="Sheet1") %>%
  subset(.,Practice=="Freshwater") 

dim.initiative.indicators.fw <-
  read.xlsx('2_Oct2019_US/2_FlatDataFiles/ConsDB_Input_2019/fy19_initiative_indicators_dim_2019_0715.xlsx',sheetName="Sheet1") %>%
  subset(.,Practice=="Freshwater")

fact.initiative.indicators.fw <-
  read.xlsx('2_Oct2019_US/2_FlatDataFiles/ConsDB_Input_2019/fy19_initiative_indicators_fact_2019_0715.xlsx',sheetName="Sheet1") %>%
  left_join(.,dim.initiatives.fw[,c("Initiative.key","Practice")], by="Initiative.key") %>%
  subset(.,Practice=="Freshwater")

dim.initiative.milestones.fw <-
  read.xlsx('2_Oct2019_US/2_FlatDataFiles/ConsDB_Input_2019/fy19_initiative_milestones_2019_0715.xlsx',sheetName="Sheet1") %>%
  subset(.,Practice=="Freshwater")


# ---- 4.2 Freshwater-specific Dim_Initiative ----

Dim_Initiative_FW <-
  dim.initiatives.fw %>%
  transmute(Initiative_Key=Initiative.key,
            Initiative_Name=Initiative,
            Initiative_Status=Overall.status,
            Initiative_Status_Justification=Overall.just,
            Initiative_Goal=Initiative.statement,
            Global_Initiative=Global.initiative,
            US_Initiative=US.initiative,
            Display_Order=Display.order)


# ---- 4.3 Freshwater-specific Dim_Initiative_Indicator_Type ----

Dim_Initiative_Indicator_FW <-
  dim.initiative.indicators.fw %>%
  transmute(Indicator_Type_Key=Initiative.indicator.key,
            Indicator_Type=Indicator.type,
            Indicator_Name=ifelse(!is.na(Indicator.name),as.character(Indicator.name),"FORTHCOMING"),
            Indicator_Label=ifelse(!is.na(Indicator.label),as.character(Indicator.label),"Not Yet Identified"),
            Indicator_Subcategory=Subcategory,
            Indicator_Unit=Units,
            Data_Source=Source,
            Indicator_Target=Target,
            Display_Order=Display.order,
            Indicator_Statement=Statement)


# ---- 4.4 Freshwater-specific Fact_Initiative_Indicators ----

Fact_Initiative_Indicator_FW <-
  fact.initiative.indicators.fw %>%
  transmute(Year_Key=ifelse(!is.na(Year),Year,9999),
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Freshwater"],length(Year_Key)),
            Initiative_Key=Initiative.key,
            Indicator_Type_Key=Initiative.indicator.key,
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 4.5 Freshwater-specific Fact_Initiative_Financials ----

Fact_Initiative_Financials_FW <-
  dim.initiatives.fw %>%
  transmute(Date_Key=Date,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Freshwater"],length(Date_Key)),
            Initiative_Key=Initiative.key,
            Amount_Needed=Funds.needed,
            Amount_Secured=Funds.secured,
            Amount_Anticipated=Funds.anticipated)


# ---- 4.6 Freshwater-specific Milestone_Group_Bridge ----

Milestone_Group_Bridge_FW <-
  left_join(dim.initiative.milestones.fw, dim.initiatives.fw, by=c("Initiative", "Practice")) %>%
  transmute(Milestone_Key=Milestone.key,
            Initiative_Key=Initiative.key)


# ---- 4.7 Freshwater-specific Dim_Milestone ----

Dim_Milestone_FW <-
  dim.initiative.milestones.fw %>%
  transmute(Milestone_Surrogate_Key="",
            Milestone_Key=Milestone.key,
            Milestone_Name=Milestone,
            Milestone_Target=Target,
            Milestone_Status=Status,
            Milestone_Status_Justification=Status.just,
            Creation_Date=Creation.date,
            Effective_Start_Date=Effective.start.date,
            Effective_End_Date=Effective.end.date,
            Is_Active=Is.active)




# ---- REMOVE CLUTTER ----

rm(Dim_Context_State_FW_A,
   Dim_Context_Threat_FW_A,
   Dim_Context_Response_FW_A,
   Fact_Context_State_FW_A,
   Fact_Context_Threat_FW_A,
   Fact_Context_Response_FW_A,
   Dim_Global_2030_Outcome1_FW_A,
   Dim_Global_2030_Outcome1_FW_B,
   Dim_Global_2030_Outcome1_FW_C,
   Dim_Global_2030_Outcome2_FW_A,
   Dim_Global_2030_Outcome2_FW_B,
   Fact_Global_2030_Outcome1_FW_A,
   Fact_Global_2030_Outcome1_FW_B,
   Fact_Global_2030_Outcome1_FW_C,
   Fact_Global_2030_Outcome2_FW_A,
   Fact_Global_2030_Outcome2_FW_B,
   dim.initiatives.fw,
   dim.initiative.indicators.fw,
   fact.initiative.indicators.fw,
   dim.initiative.milestones.fw)
