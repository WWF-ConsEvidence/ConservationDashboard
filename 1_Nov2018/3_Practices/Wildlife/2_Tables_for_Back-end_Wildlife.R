# 
# code: Wildlife Practice Global Context and Global 2030 Outcome Indicator Tables
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


# -- INDICATOR NAME

Dim_Context_State_Wildlife <- 
  data.frame(Indicator_Type_Key="GCS_WL_A",
             Indicator_Name="",
             Indicator_Label="",
             Panel_Label="",
             Panel="State",
             Indicator_Subcategory=NA,
             Indicator_Unit="",
             Data_Source="")

Fact_Context_State_Wildlife <-
  read.csv() %>%
  transmute(Year_Key=NA,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],length(NA)),
            Indicator_Type_Key=rep(Dim_Context_State_Wildlife$Indicator_Type_Key,length(NA)),
            Indicator_Value=NA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 2.2 Context - Threat ----

# -- INDICATOR NAME

Dim_Context_Threat_Wildlife <- 
  data.frame(Indicator_Type_Key="GCT_WL_A",
             Indicator_Name="",
             Indicator_Label="",
             Panel_Label="",
             Panel="Threat",
             Indicator_Subcategory=NA,
             Indicator_Unit="",
             Data_Source="")

Fact_Context_Threat_Wildlife <-
  read.csv() %>%
  transmute(Year_Key=NA,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],length(NA)),
            Indicator_Type_Key=rep(Dim_Context_Threat_Wildlife$Indicator_Type_Key,length(NA)),
            Indicator_Value=NA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 2.3 Context - Response ----

# -- INDICATOR NAME

Dim_Context_Response_Wildlife <- 
  data.frame(Indicator_Type_Key="GCR_WL_A",
             Indicator_Name="",
             Indicator_Label="",
             Panel_Label="",
             Panel="Response",
             Indicator_Subcategory=NA,
             Indicator_Unit="",
             Data_Source="")

Fact_Context_Response_Wildlife <-
  read.csv() %>%
  transmute(Year_Key=NA,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],length(NA)),
            Indicator_Type_Key=rep(Dim_Context_Response_Wildlife$Indicator_Type_Key,length(NA)),
            Indicator_Value=NA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 2.4 Consolidated Oceans-specific Global Context tables ----


#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Global 2030 Outcomes ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 3.1 Oceans Outcome 1 - VITAL HABITATS CONSERVED ----

Dim_Global_WWF_2030_Outcome1_Wildlife <- 
  data.frame(Indicator_Type_Key="OUT1_WL_A",
             Indicator_Name="",
             Indicator_Label="",
             Indicator_Subcategory=NA,
             Indicator_Unit="",
             Data_source="",
             Indicator_Target=NA,
             Indicator_Type="",
             Panel_Label="",
             Display_Order=1)

Fact_Global_2030_Outcome1_Wildlife <-
  read.csv() %>%
  transmute(Year_Key=NA,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],length(NA)),
            Indicator_Type_Key=rep(Dim_Global_WWF_2030_Outcome1_Wildlife$Indicator_Type_Key, length(NA)),
            Indicator_Value=NA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 3.2 Wildlife Outcome 2 - OVEREXPLOITATION PREVENTED ----

Dim_Global_WWF_2030_Outcome2_Wildlife <- 
  data.frame(Indicator_Type_Key="OUT2_WL_A",
             Indicator_Name="",
             Indicator_Label="",
             Indicator_Subcategory=NA,
             Indicator_Unit="",
             Data_source="",
             Indicator_Target=NA,
             Indicator_Type="",
             Panel_Label="",
             Display_Order=2)

Fact_Global_2030_Outcome2_Wildlife <-
  read.csv() %>%
  transmute(Year_Key=NA,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],length(NA)),
            Indicator_Type_Key=rep(Dim_Global_WWF_2030_Outcome2_Wildlife$Indicator_Type_Key, length(NA)),
            Indicator_Value=NA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 3.3 Consolidated Wildlife-specific Global 2030 Outcome tables ----

