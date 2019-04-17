# 
# code: Finance Practice Indicator and Initiative Tables
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: August 2018
# modified: 
# 
# ---- inputs ----
#  1) Finance-specific data tables (in 1_Nov2018/2_FlatDataFiles/ConsDB_Input)
# 
# ---- outputs ----
#  1) Finance-specific back-end tables -- ready to be consolidated with other Practices, to go into back-end:
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

# -- SUSTAINABLE INVESTMENT - GLOBAL SUSTAINABLE ASSETS

Dim_Context_State_Finance_A <- 
  data.frame(Indicator_Type_Key="GCS_FI_A",
             Indicator_Name="Global sustainable investment assets (SRI) - global ",
             Indicator_Label="Global Sustainable Investment Assets",
             Panel_Label="Sustainable Investment",
             Panel="State",
             Indicator_Subcategory="Billion USD",
             Indicator_Unit="Billion USD",
             Data_Source="Global Sustainable Investment Alliance (GSIA), biannual GSIA review report")

Fact_Context_State_Finance_A <-
  read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/GSIA_sustainable_assets_2018_0820.xlsx', sheetName="Sheet1") %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Finance"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_State_Finance_A$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Global_SRI_Assets,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)

# -- SUSTAINABLE INVESTMENT - PERCENT ASSETS SUSTAINABLE

Dim_Context_State_Finance_B <- 
  data.frame(Indicator_Type_Key="GCS_FI_B",
             Indicator_Name="Global sustainable investment assets (SRI) - percent of total assets ",
             Indicator_Label="Global Sustainable Investment Assets",
             Panel_Label="Sustainable Investment",
             Panel="State",
             Indicator_Subcategory="% Total Assets",
             Indicator_Unit="% total assets",
             Data_Source="Global Sustainable Investment Alliance (GSIA), biannual GSIA review report")

Fact_Context_State_Finance_B <-
  read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/GSIA_sustainable_assets_2018_0820.xlsx', sheetName="Sheet1") %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Finance"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_State_Finance_B$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Percent_Total_Assets,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 2.2 Consolidated Finance-specific Global Context tables ----

Dim_Context_Finance <- 
  rbind.data.frame(Dim_Context_State_Finance_A,
                   Dim_Context_State_Finance_B)

Fact_Context_Finance <-
  rbind.data.frame(Fact_Context_State_Finance_A,
                   Fact_Context_State_Finance_B)

#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Initiatives ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 3.1 Load data ----

dim.initiatives.finance <- 
  read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/fy18_initiative_reporting_dim_2018_1121.xlsx',sheetName="Sheet1") %>%
  subset(.,Practice=="Finance") 

dim.initiative.indicators.finance <-
  read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/fy18_initiative_indicators_fact_2018_1121.xlsx',sheetName="Sheet1") %>%
  subset(.,Practice=="Finance")


# ---- 3.2 Finance-specific Dim_Initiative ----

Dim_Initiative_Finance <-
  dim.initiatives.finance %>%
  transmute(Initiative_Key=Initiative.key,
            Initiative_Name=Initiative,
            Initiative_Status=Overall.status,
            Initiative_Status_Justification=Overall.just,
            Initiative_Goal=Initiative.statement)


# ---- 3.3 Finance-specific Dim_Initiative_Indicator_Type ----

Dim_Initiative_Indicator_Finance <-
  dim.initiative.indicators.finance %>%
  transmute(Indicator_Type_Key=Initiative.indicator.key,
            Indicator_Type=Indicator.type,
            Indicator_Name=ifelse(!is.na(Indicator.name),as.character(Indicator.name),"FORTHCOMING"),
            Indicator_Label=ifelse(!is.na(Indicator.label),as.character(Indicator.label),"Not Yet Identified"),
            Indicator_Subcategory=Subcategory,
            Indicator_Target=Target,
            Indicator_Unit=Units,
            Data_Source=Source,
            Display_Order=Display.order)


# ---- 3.4 Finance-specific Fact_Initiative_Indicators ----

Fact_Initiative_Indicator_Finance <-
  dim.initiative.indicators.finance %>%
  left_join(.,dim.initiatives.finance[,c("Initiative.key","Initiative","Practice.outcome.key")],
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
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Finance"],length(Year_Key)),
            Initiative_Key=Initiative.key,
            Indicator_Type_Key=Initiative.indicator.key,
            Practice_Outcome_Key=Practice.outcome.key,
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 3.5 Finance-specific Fact_Initiative_Financials ----

Fact_Initiative_Financials_Finance <-
  dim.initiatives.finance %>%
  transmute(Date_Key=Date,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Finance"],length(Date_Key)),
            Initiative_Key=Initiative.key,
            Amount_needed=Funds.needed,
            Amount_secured=Funds.secured)




# ---- REMOVE CLUTTER ----

rm(Dim_Context_State_Finance_A,
   Dim_Context_State_Finance_B,
   Fact_Context_State_Finance_A,
   Fact_Context_State_Finance_B,
   dim.initiatives.finance,
   dim.initiative.indicators.finance)