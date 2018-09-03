# 
# code: Oceans Practice Global Context and Global 2030 Outcome Indicator Tables
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: August 2018
# modified: 
# 
# ---- inputs ----
#  1) Oceans-specific data tables (in 1_Nov2018/2_FlatDataFiles/ConsDB_Input)
# 
# ---- outputs ----
#  1) Oceans-specific back-end tables -- ready to be consolidated with other Practices, to go into back-end:
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

Dim_Context_State_Oceans <- 
  data.frame(Indicator_Type_Key="GCS_OC_A",
             Indicator_Name="",
             Indicator_Label="",
             Panel_Label="",
             Panel="State",
             Indicator_Subcategory=NA,
             Indicator_Unit="",
             Data_Source="")

Fact_Context_State_Oceans <-
  read.csv() %>%
  transmute(Year_Key=NA,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Oceans"],length(NA)),
            Indicator_Type_Key=rep(Dim_Context_State_Oceans$Indicator_Type_Key,length(NA)),
            Indicator_Value=NA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 2.2 Context - Threat ----

# -- INDICATOR NAME

Dim_Context_Threat_Oceans <- 
  data.frame(Indicator_Type_Key="GCT_OC_A",
             Indicator_Name="",
             Indicator_Label="",
             Panel_Label="",
             Panel="Threat",
             Indicator_Subcategory=NA,
             Indicator_Unit="",
             Data_Source="")

Fact_Context_Threat_Oceans <-
  read.csv() %>%
  transmute(Year_Key=NA,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Oceans"],length(NA)),
            Indicator_Type_Key=rep(Dim_Context_Threat_Oceans$Indicator_Type_Key,length(NA)),
            Indicator_Value=NA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 2.3 Context - Response ----

# -- INDICATOR NAME

Dim_Context_Response_Oceans <- 
  data.frame(Indicator_Type_Key="GCR_OC_A",
             Indicator_Name="",
             Indicator_Label="",
             Panel_Label="",
             Panel="Response",
             Indicator_Subcategory=NA,
             Indicator_Unit="",
             Data_Source="")

Fact_Context_Response_Oceans <-
  read.csv() %>%
  transmute(Year_Key=NA,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Oceans"],length(NA)),
            Indicator_Type_Key=rep(Dim_Context_Response_Oceans$Indicator_Type_Key,length(NA)),
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


# ---- 3.1 Oceans Outcome 1 - RESILIENT AND PRODUCTIVE ECOSYSTEMS ----

Dim_Global_WWF_2030_Outcome1_Oceans <- 
  data.frame(Indicator_Type_Key="OUT1_OC_A",
             Indicator_Name="",
             Indicator_Label="",
             Indicator_Subcategory=NA,
             Indicator_Unit="",
             Data_source="",
             Indicator_Target=NA,
             Indicator_Type="",
             Panel_Label="",
             Display_Order=1)

Fact_Global_2030_Outcome1_Oceans <-
  read.csv() %>%
  transmute(Year_Key=NA,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Oceans"],length(NA)),
            Indicator_Type_Key=rep(Dim_Global_WWF_2030_Outcome1_Oceans$Indicator_Type_Key, length(NA)),
            Indicator_Value=NA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 3.2 Oceans Outcome 2 - SUSTAINABLE FISHERIES ----

Dim_Global_WWF_2030_Outcome2_Oceans <- 
  data.frame(Indicator_Type_Key="OUT2_OC_A",
             Indicator_Name="",
             Indicator_Label="",
             Indicator_Subcategory=NA,
             Indicator_Unit="",
             Data_source="",
             Indicator_Target=NA,
             Indicator_Type="",
             Panel_Label="",
             Display_Order=2)

Fact_Global_2030_Outcome2_Oceans <-
  read.csv() %>%
  transmute(Year_Key=NA,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Oceans"],length(NA)),
            Indicator_Type_Key=rep(Dim_Global_WWF_2030_Outcome2_Oceans$Indicator_Type_Key, length(NA)),
            Indicator_Value=NA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 3.3 Consolidated Oceans-specific Global 2030 Outcome tables ----

