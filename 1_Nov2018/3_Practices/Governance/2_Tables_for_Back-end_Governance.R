# 
# code: Governance Practice Indicator and Initiative Tables
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: August 2018
# modified: 
# 
# ---- inputs ----
#  1) Governance-specific data tables (in 1_Nov2018/2_FlatDataFiles/ConsDB_Input)
# 
# ---- outputs ----
#  1) Governance-specific back-end tables -- ready to be consolidated with other Practices, to go into back-end:
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

# -- INDIGENOUS AND COMMUNITY LAND RIGHTS

Dim_Context_State_Governance_A <- 
  data.frame(Indicator_Type_Key="GCS_GV_A",
             Indicator_Name="Gap in formal recognition of ICCAs, compared to all community conserved lands",
             Indicator_Label="Formally Recognized Indigenous and Community Conserved Areas (ICCAs)*",
             Panel_Label="Indigenous and Community Land Rights",
             Panel="State",
             Indicator_Subcategory=NA,
             Indicator_Unit="% of total estimated community conserved lands",
             Data_Source="WDPA; IUCN (for estimate of total community conserved lands)")

Fact_Context_State_Governance_A <-
  read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/ICCA_timeseries.csv') %>%
  subset(.,STATUS_YR>1994) %>%
  transmute(Year_Key=STATUS_YR,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Governance"],length(STATUS_YR)),
            Indicator_Type_Key=rep(Dim_Context_State_Governance_A$Indicator_Type_Key,length(STATUS_YR)),
            Indicator_Value=AREA_PERCENT_EST*100,
            Indicator_Upper_Value=AREA_PERCENT_HI*100,
            Indicator_Lower_Value=AREA_PERCENT_LOW*100)


# ---- 2.2 Context - Threat ----

# -- UNSUSTAINABLE DEVELOPMENT - TOTAL LOSS

Dim_Context_Threat_Governance_A <- 
  data.frame(Indicator_Type_Key="GCT_GV_A",
             Indicator_Name="Intact ecosystems lost to unsustainable development",
             Indicator_Label="Global Tree Cover Loss & Deforestation",
             Panel_Label="Unsustainable Development",
             Panel="Threat",
             Indicator_Subcategory="Total Loss",
             Indicator_Unit="M ha per year",
             Data_Source="Global Forest Watch")

Fact_Context_Threat_Governance_A <-
  read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/GFW_treeloss_bydriver_2018_0919.xlsx', sheetName="Sheet1") %>%
  subset(.,Geography=="World" & Loss_type=="Total Loss") %>%
  transmute(Year_Key=Year,
             Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Governance"],length(Year_Key)),
             Indicator_Type_Key=rep(Dim_Context_Threat_Governance_A$Indicator_Type_Key,length(Year_Key)),
             Indicator_Value=Value,
             Indicator_Upper_Value=NA,
             Indicator_Lower_Value=NA)

# -- UNSUSTAINABLE DEVELOPMENT - COMMODITY DRIVEN DEFORESTATION

Dim_Context_Threat_Governance_B <- 
  data.frame(Indicator_Type_Key="GCT_GV_B",
             Indicator_Name="Intact ecosystems lost to unsustainable development",
             Indicator_Label="Global Tree Cover Loss & Deforestation",
             Panel_Label="Unsustainable Development",
             Panel="Threat",
             Indicator_Subcategory="Commodity Driven Deforestation",
             Indicator_Unit="M ha per year",
             Data_Source="Global Forest Watch; Curtis et al (2018) Global drivers of forest loss")

Fact_Context_Threat_Governance_B <-
  read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/GFW_treeloss_bydriver_2018_0919.xlsx', sheetName="Sheet1") %>%
  subset(.,Geography=="World" & Loss_type=="Commodity Driven Deforestation") %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Governance"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_Threat_Governance_B$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 2.3 Context - Response ----

# -- COMMUNITY CONSERVED LAND - ICCA COVERAGE

Dim_Context_Response_Governance_A <- 
  data.frame(Indicator_Type_Key="GCR_GV_A",
             Indicator_Name="ICCA coverage (of formally recognized Indigenous and Community Conserved Areas)",
             Indicator_Label="Indigenous and Community Conserved Areas (ICCAs)*",
             Panel_Label="Community Conserved Land",
             Panel="Response",
             Indicator_Subcategory="Coverage",
             Indicator_Unit="M ha",
             Data_Source="WDPA")

Fact_Context_Response_Governance_A <-
  read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/ICCA_timeseries.csv') %>%
  subset(.,STATUS_YR>1994) %>%
  transmute(Year_Key=STATUS_YR,
             Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Governance"],length(STATUS_YR)),
             Indicator_Type_Key=rep(Dim_Context_Response_Governance_A$Indicator_Type_Key,length(STATUS_YR)),
             Indicator_Value=AREA_MHA_TIME,
             Indicator_Upper_Value=NA,
             Indicator_Lower_Value=NA)

# -- COMMUNITY CONSERVED LAND - EFFECTIVE GUARDIANSHIP

Dim_Context_Response_Governance_B <- 
  data.frame(Indicator_Type_Key="GCR_GV_B",
             Indicator_Name="Effective guardianship of community conserved lands",
             Indicator_Label="Indigenous and Community Conserved Areas (ICCAs)*",
             Panel_Label="Community Conserved Land",
             Panel="Response",
             Indicator_Subcategory="Effective Guardianship*",
             Indicator_Unit="",
             Data_Source="")

Fact_Context_Response_Governance_B <-
  data.frame(Year_Key=9999,
             Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Governance"],length(1)),
             Indicator_Type_Key=rep(Dim_Context_Response_Governance_B$Indicator_Type_Key,length(1)),
             Indicator_Value=NA,
             Indicator_Upper_Value=NA,
             Indicator_Lower_Value=NA)


# ---- 2.4 Consolidated Governance-specific Global Context tables ----

Dim_Context_Governance <- 
  rbind.data.frame(Dim_Context_State_Governance_A,
                   Dim_Context_Threat_Governance_A,
                   Dim_Context_Threat_Governance_B,
                   Dim_Context_Response_Governance_A,
                   Dim_Context_Response_Governance_B)

Fact_Context_Governance <-
  rbind.data.frame(Fact_Context_State_Governance_A,
                   Fact_Context_Threat_Governance_A,
                   Fact_Context_Threat_Governance_B,
                   Fact_Context_Response_Governance_A,
                   Fact_Context_Response_Governance_B)

#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Initiatives ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 3.1 Load data ----

dim.initiatives.governance <- 
  read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/fy18_initiative_reporting_dim_2018_0908.xlsx',sheetName="Sheet1") %>%
  subset(.,Practice=="Governance") 

dim.initiative.indicators.governance <-
  read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/fy18_initiative_indicators_fact_2018_0908.xlsx',sheetName="Sheet1") %>%
  subset(.,Practice=="Governance")


# ---- 3.2 Governance-specific Dim_Initiative ----

Dim_Initiative_Governance <-
  dim.initiatives.governance %>%
  transmute(Initiative_Key=Initiative.key,
            Initiative_Name=Initiative,
            Initiative_Status=Overall.status,
            Initiative_Status_Justification=Overall.just,
            Initiative_Goal=Initiative.statement)


# ---- 3.3 Governance-specific Dim_Initiative_Indicator_Type ----

Dim_Initiative_Indicator_Governance <-
  dim.initiative.indicators.governance %>%
  transmute(Indicator_Type_Key=Initiative.indicator.key,
            Indicator_Type=Indicator.type,
            Indicator_Name=ifelse(!is.na(Indicator.name),as.character(Indicator.name),"FORTHCOMING"),
            Indicator_Label=ifelse(!is.na(Indicator.label),as.character(Indicator.label),"Not Yet Identified"),
            Indicator_Subcategory=Subcategory,
            Indicator_Target=Target,
            Indicator_Unit=Units,
            Data_Source=Source,
            Display_Order=Display.order)


# ---- 3.4 Governance-specific Fact_Initiative_Indicators ----

Fact_Initiative_Indicator_Governance <-
  dim.initiative.indicators.governance %>%
  left_join(.,dim.initiatives.governance[,c("Initiative.key","Initiative","Practice.outcome.key")],
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
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Governance"],length(Year_Key)),
            Initiative_Key=Initiative.key,
            Indicator_Type_Key=Initiative.indicator.key,
            Practice_Outcome_Key=Practice.outcome.key,
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 3.5 Governance-specific Fact_Initiative_Financials ----

Fact_Initiative_Financials_Governance <-
  dim.initiatives.governance %>%
  transmute(Date_Key=Date,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Governance"],length(Date_Key)),
            Initiative_Key=Initiative.key,
            Amount_needed=Funds.needed,
            Amount_secured=Funds.secured)




# ---- REMOVE CLUTTER ----

rm(Dim_Context_State_Governance_A,
   Dim_Context_Threat_Governance_A,
   Dim_Context_Threat_Governance_B,
   Dim_Context_Response_Governance_A,
   Dim_Context_Response_Governance_B,
   Fact_Context_State_Governance_A,
   Fact_Context_Threat_Governance_A,
   Fact_Context_Threat_Governance_B,
   Fact_Context_Response_Governance_A,
   Fact_Context_Response_Governance_B,
   dim.initiatives.governance,
   dim.initiative.indicators.governance)
