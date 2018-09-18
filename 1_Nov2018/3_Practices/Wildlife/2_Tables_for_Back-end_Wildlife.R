# 
# code: Wildlife Practice Indicator and Initiative Tables
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: August 2018
# modified: 
# 
# ---- inputs ----
#  1) Wildlife-specific data tables (in 1_Nov2018/2_FlatDataFiles/ConsDB_Input)
# 
# ---- outputs ----
#  1) Wildlife-specific back-end tables -- ready to be consolidated with other Practices, to go into back-end:
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

# -- GLOBAL BIODIVERSITY - LIVING PLANET INDEX

Dim_Context_State_Wildlife_A <- 
  data.frame(Indicator_Type_Key="GCS_WL_A",
             Indicator_Name="Global Living Planet Index",
             Indicator_Label="Population Trends & Extinction Risk",
             Panel_Label="Global Biodiversity",
             Panel="State",
             Indicator_Subcategory="Living Planet Index",
             Indicator_Unit="Index",
             Data_Source="Zoological Society of London; 2016 Living Planet Index database")

Fact_Context_State_Wildlife_A <-
  read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/Global_LPI_fromflatfile_2017_0912.xlsx', sheetName="Sheet1") %>%
  subset(.,Year>1994) %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_State_Wildlife_A$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Value,
            Indicator_Upper_Value=Upper,
            Indicator_Lower_Value=Lower)


# -- GLOBAL BIODIVERSITY - RED LIST INDEX

Dim_Context_State_Wildlife_B <- 
  data.frame(Indicator_Type_Key="GCS_WL_B",
             Indicator_Name="Red List Index",
             Indicator_Label="Population Trends & Extinction Risk",
             Panel_Label="Global Biodiversity",
             Panel="State",
             Indicator_Subcategory="Red List Index",
             Indicator_Unit="Index",
             Data_Source="SDG Indicator Bank -- IUCN")

Fact_Context_State_Wildlife_B <-
  read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/Global_RLI_fromflatfile_2017_0912.xlsx', sheetName="Sheet1") %>%
  subset(.,Year>1994 & Year<2013) %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_State_Wildlife_B$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Value,
            Indicator_Upper_Value=Upper,
            Indicator_Lower_Value=Lower)


# ---- 2.2 Context - Threat ----

# -- HABITAT LOSS

Dim_Context_Threat_Wildlife_A <- 
  data.frame(Indicator_Type_Key="GCT_WL_A",
             Indicator_Name="Global tree cover loss (M ha per year)",
             Indicator_Label="Terrestrial & Marine* Habitat Loss",
             Panel_Label="Habitat Loss",
             Panel="Threat",
             Indicator_Subcategory="Forest Loss",
             Indicator_Unit="M ha per year",
             Data_Source="Global Forest Watch")

Fact_Context_Threat_Wildlife_A <-
  read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/GFW_ForestLoss_2018_0821.xlsx', sheetName="Sheet1") %>%
  subset(.,Geography=="World") %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_Threat_Wildlife_A$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 2.3 Context - Response ----

# -- HABITAT PROTECTION - PROTECETD

Dim_Context_Response_Wildlife_A <- 
  data.frame(Indicator_Type_Key="GCR_WL_A",
             Indicator_Name="Global PA Coverage (terrestrial & marine; M ha)",
             Indicator_Label="Protected & Community Conserved Areas",
             Panel_Label="Habitat Protection",
             Panel="Response",
             Indicator_Subcategory="Protected",
             Indicator_Unit="M ha",
             Data_Source="WDPA")

Fact_Context_Response_Wildlife_A <-
  data.frame(Year_Key=9999,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],length(1)),
            Indicator_Type_Key=rep(Dim_Context_Response_Wildlife_A$Indicator_Type_Key,length(1)),
            Indicator_Value=NA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)

# -- HABITAT PROTECTION - COMMUNITY CONSERVED

Dim_Context_Response_Wildlife_B <- 
  data.frame(Indicator_Type_Key="GCR_WL_B",
             Indicator_Name="ICCA Coverage (M ha)",
             Indicator_Label="Protected & Community Conserved Areas",
             Panel_Label="Habitat Protection",
             Panel="Response",
             Indicator_Subcategory="Community Conserved*",
             Indicator_Unit="M ha",
             Data_Source="WDPA")

Fact_Context_Response_Wildlife_B <-
  data.frame(Year_Key=9999,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],length(1)),
            Indicator_Type_Key=rep(Dim_Context_Response_Wildlife_B$Indicator_Type_Key,length(1)),
            Indicator_Value=NA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 2.4 Consolidated Wildlife-specific Global Context tables ----

Dim_Context_Wildlife <- 
  rbind.data.frame(Dim_Context_State_Wildlife_A,
                   Dim_Context_State_Wildlife_B,
                   Dim_Context_Threat_Wildlife_A,
                   Dim_Context_Response_Wildlife_A,
                   Dim_Context_Response_Wildlife_B)

Fact_Context_Wildlife <-
  rbind.data.frame(Fact_Context_State_Wildlife_A,
                   Fact_Context_State_Wildlife_B,
                   Fact_Context_Threat_Wildlife_A,
                   Fact_Context_Response_Wildlife_A,
                   Fact_Context_Response_Wildlife_B)


#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Global 2030 Outcomes ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 3.1 Wildlife Outcome 1 - VITAL HABITATS CONSERVED ----

# -- EXTENSIVE

Dim_Global_2030_Outcome1_Wildlife_A <- 
  data.frame(Indicator_Type_Key="OUT1_WL_A",
             Indicator_Name="Global protected area coverage, as percent of total land area",
             Indicator_Label="Extensive, Effective, Connected, & Biologically Important Habitat Protection",
             Indicator_Subcategory="Extensive",
             Indicator_Unit="% global area",
             Data_source="WDPA",
             Indicator_Target=30,
             Indicator_Type="Outcome",
             Panel_Label="Vital Habitats Conserved",
             Display_Order=1)

Fact_Global_2030_Outcome1_Wildlife_A <-
  data.frame(Year_Key=c(1995,2030),
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],2),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome1_Wildlife_A$Indicator_Type_Key, 2),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Wildlife" &
                                                                   grepl("Habitats",practice_outcome_key_ref$practice_outcome)], 2),
            Indicator_Value=c(NA,30),
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)

# -- EFFECTIVE - ASSESSED

Dim_Global_2030_Outcome1_Wildlife_B <- 
  data.frame(Indicator_Type_Key="OUT1_WL_B",
             Indicator_Name="Percent of total protected area that has been assessed using METT",
             Indicator_Label="Extensive, Effective, Connected, & Biologically Important Habitat Protection",
             Indicator_Subcategory="Effective (METT Assessed)*",
             Indicator_Unit="% protected area",
             Data_source="METT",
             Indicator_Target=100,
             Indicator_Type="Outcome",
             Panel_Label="Vital Habitats Conserved",
             Display_Order=1)

Fact_Global_2030_Outcome1_Wildlife_B <-
  data.frame(Year_Key=9999,
             Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],length(1)),
             Indicator_Type_Key=rep(Dim_Global_2030_Outcome1_Wildlife_B$Indicator_Type_Key, length(1)),
             Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Wildlife" &
                                                                    grepl("Habitats",practice_outcome_key_ref$practice_outcome)], length(1)),
             Indicator_Value=NA,
             Indicator_Upper_Value=NA,
             Indicator_Lower_Value=NA)

# -- EFFECTIVE - MEETS THRESHOLD

Dim_Global_2030_Outcome1_Wildlife_C <- 
  data.frame(Indicator_Type_Key="OUT1_WL_C",
             Indicator_Name="Percent of total assessed area that meets a METT threshold of effectiveness, based on Nature's Gill et al (2017)",
             Indicator_Label="Extensive, Effective, Connected, & Biologically Important Habitat Protection",
             Indicator_Subcategory="Effective (Meets Score Threshold)*",
             Indicator_Unit="% assessed area",
             Data_source="METT",
             Indicator_Target=100,
             Indicator_Type="Outcome",
             Panel_Label="Vital Habitats Conserved",
             Display_Order=1)

Fact_Global_2030_Outcome1_Wildlife_C <-
  data.frame(Year_Key=9999,
             Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],length(1)),
             Indicator_Type_Key=rep(Dim_Global_2030_Outcome1_Wildlife_C$Indicator_Type_Key, length(1)),
             Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Wildlife" &
                                                                    grepl("Habitats",practice_outcome_key_ref$practice_outcome)], length(1)),
             Indicator_Value=NA,
             Indicator_Upper_Value=NA,
             Indicator_Lower_Value=NA)

# -- CONNECTED

Dim_Global_2030_Outcome1_Wildlife_D <- 
  data.frame(Indicator_Type_Key="OUT1_WL_D",
             Indicator_Name="FORTHCOMING: Percent of total protected area that meets a connectedness threshold (M ha PAs meeting threshold / M ha PAs)",
             Indicator_Label="Extensive, Effective, Connected, & Biologically Important Habitat Protection",
             Indicator_Subcategory="Connected*",
             Indicator_Unit="% protected area",
             Data_source="",
             Indicator_Target=100,
             Indicator_Type="Outcome",
             Panel_Label="Vital Habitats Conserved",
             Display_Order=1)

Fact_Global_2030_Outcome1_Wildlife_D <-
  data.frame(Year_Key=9999,
             Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],length(1)),
             Indicator_Type_Key=rep(Dim_Global_2030_Outcome1_Wildlife_D$Indicator_Type_Key, length(1)),
             Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Wildlife" &
                                                                    grepl("Habitats",practice_outcome_key_ref$practice_outcome)], length(1)),
             Indicator_Value=NA,
             Indicator_Upper_Value=NA,
             Indicator_Lower_Value=NA)

# -- BIOLOGICALLY IMPORTANT

Dim_Global_2030_Outcome1_Wildlife_E <- 
  data.frame(Indicator_Type_Key="OUT1_WL_E",
             Indicator_Name="Percent of total protected area that is within Key Biodiversity Areas (M ha PAs within KBAs / M ha PAs)",
             Indicator_Label="Extensive, Effective, Connected, & Biologically Important Habitat Protection",
             Indicator_Subcategory="Biologically Important",
             Indicator_Unit="% protected area",
             Data_source="WDPA & KBA database",
             Indicator_Target=100,
             Indicator_Type="Outcome",
             Panel_Label="Vital Habitats Conserved",
             Display_Order=1)

Fact_Global_2030_Outcome1_Wildlife_E <-
  data.frame(Year_Key=9999,
             Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],length(1)),
             Indicator_Type_Key=rep(Dim_Global_2030_Outcome1_Wildlife_E$Indicator_Type_Key, length(1)),
             Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Wildlife" &
                                                                    grepl("Habitats",practice_outcome_key_ref$practice_outcome)], length(1)),
             Indicator_Value=NA,
             Indicator_Upper_Value=NA,
             Indicator_Lower_Value=NA)

# ---- 3.2 Wildlife Outcome 2 - OVEREXPLOITATION PREVENTED ----

Dim_Global_2030_Outcome2_Wildlife_A <- 
  data.frame(Indicator_Type_Key="OUT2_WL_A",
             Indicator_Name="FORTHCOMING: CITES Illegal Trade & Exploitation Index",
             Indicator_Label="Illegal Trade and Exploitation Index*",
             Indicator_Subcategory="(under development by WWF & TRAFFIC)",
             Indicator_Unit="",
             Data_source="CITES, processed by WWF/TRAFFIC",
             Indicator_Target=NA,
             Indicator_Type="Outcome",
             Panel_Label="Overexploitation Prevented",
             Display_Order=2)

Fact_Global_2030_Outcome2_Wildlife_A <-
  data.frame(Year_Key=9999,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],length(1)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome2_Wildlife_A$Indicator_Type_Key, length(1)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Wildlife" &
                                                                   grepl("Overexploitation",practice_outcome_key_ref$practice_outcome)], length(1)),
            Indicator_Value=NA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 3.3 Consolidated Wildlife-specific Global 2030 Outcome tables ----

Dim_Global_2030_Outcome_Wildlife <-
  rbind.data.frame(Dim_Global_2030_Outcome1_Wildlife_A,
                   Dim_Global_2030_Outcome1_Wildlife_B,
                   Dim_Global_2030_Outcome1_Wildlife_C,
                   Dim_Global_2030_Outcome1_Wildlife_D,
                   Dim_Global_2030_Outcome1_Wildlife_E,
                   Dim_Global_2030_Outcome2_Wildlife_A)

Fact_Global_2030_Outcome_Wildlife <-
  rbind.data.frame(Fact_Global_2030_Outcome1_Wildlife_A,
                   Fact_Global_2030_Outcome1_Wildlife_B,
                   Fact_Global_2030_Outcome1_Wildlife_C,
                   Fact_Global_2030_Outcome1_Wildlife_D,
                   Fact_Global_2030_Outcome1_Wildlife_E,
                   Fact_Global_2030_Outcome2_Wildlife_A)


#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: Initiatives ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 4.1 Load data ----

dim.initiatives.wildlife <- 
  read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/fy18_initiative_reporting_dim_2018_0908.xlsx',sheetName="Sheet1") %>%
  subset(.,Practice=="Wildlife") 

dim.initiative.indicators.wildlife <-
  read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/fy18_initiative_indicators_fact_2018_0908.xlsx',sheetName="Sheet1") %>%
  subset(.,Practice=="Wildlife")

dim.initiative.milestones.wildlife <-
  read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/fy18_initiative_milestones_2018_0908.csv') %>%
  subset(.,Practice=="Wildlife")

# ---- 4.2 Wildlife-specific Dim_Initiative ----

Dim_Initiative_Wildlife <-
  dim.initiatives.wildlife %>%
  transmute(Initiative_Key=Initiative.key,
            Initiative_Name=Initiative,
            Initiative_Status=Overall.status,
            Initiative_Status_Justification=Overall.just,
            Initiative_Goal=Initiative.statement)


# ---- 4.3 Wildlife-specific Dim_Initiative_Indicator_Type ----

Dim_Initiative_Indicator_Wildlife <-
  dim.initiative.indicators.wildlife %>%
  transmute(Indicator_Type_Key=Initiative.indicator.key,
            Indicator_Type=Indicator.type,
            Indicator_Name=ifelse(!is.na(Indicator.name),as.character(Indicator.name),"FORTHCOMING"),
            Indicator_Label=ifelse(!is.na(Indicator.label),as.character(Indicator.label),"Not Yet Identified"),
            Indicator_Subcategory=Subcategory,
            Indicator_Target=Target,
            Indicator_Unit=Units,
            Data_Source=Source)


# ---- 4.4 Wildlife-specific Fact_Initiative_Indicators ----

Fact_Initiative_Indicator_Wildlife <-
  dim.initiative.indicators.wildlife %>%
  left_join(.,dim.initiatives.wildlife[,c("Initiative.key","Initiative","Practice.outcome.key")],
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
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],length(Year_Key)),
            Initiative_Key=Initiative.key,
            Indicator_Type_Key=Initiative.indicator.key,
            Practice_Outcome_Key=Practice.outcome.key,
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)



# ---- 4.5 Wildlife-specific Fact_Initiative_Financials ----

Fact_Initiative_Financials_Wildlife <-
  dim.initiatives.wildlife %>%
  transmute(Date_Key=Date,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],length(Date_Key)),
            Initiative_Key=Initiative.key,
            Amount_needed=Funds.needed,
            Amount_secured=Funds.secured)


# ---- 4.6 Wildlife-specific Milestone_Group_Bridge ----

Milestone_Group_Bridge_Wildlife <-
  left_join(dim.initiative.milestones.wildlife, dim.initiatives.wildlife, by=c("Initiative", "Practice")) %>%
  transmute(Milestone_Key=Milestone.key,
            Initiative_Key=Initiative.key)


# ---- 4.7 Wildlife-specific Dim_Milestone ----

Dim_Milestone_Wildlife <-
  dim.initiative.milestones.wildlife %>%
  transmute(Milestone_Key=Milestone.key,
            Milestone_Name=Milestone,
            Milestone_Target=Target,
            Milestone_Status=Status,
            Milestone_Status_Justification=Status.just)



# ---- REMOVE CLUTTER ----

rm(Dim_Context_State_Wildlife_A,
   Dim_Context_State_Wildlife_B,
   Dim_Context_Threat_Wildlife_A,
   Dim_Context_Response_Wildlife_A,
   Dim_Context_Response_Wildlife_B,
   Fact_Context_State_Wildlife_A,
   Fact_Context_State_Wildlife_B,
   Fact_Context_Threat_Wildlife_A,
   Fact_Context_Response_Wildlife_A,
   Fact_Context_Response_Wildlife_B,
   Dim_Global_2030_Outcome1_Wildlife_A,
   Dim_Global_2030_Outcome1_Wildlife_B,
   Dim_Global_2030_Outcome1_Wildlife_C,
   Dim_Global_2030_Outcome1_Wildlife_D,
   Dim_Global_2030_Outcome2_Wildlife_A,
   Fact_Global_2030_Outcome1_Wildlife_A,
   Fact_Global_2030_Outcome1_Wildlife_B,
   Fact_Global_2030_Outcome1_Wildlife_C,
   Fact_Global_2030_Outcome1_Wildlife_D,
   Fact_Global_2030_Outcome2_Wildlife_A,
   dim.initiatives.wildlife,
   dim.initiative.indicators.wildlife,
   dim.initiative.milestones.wildlife)