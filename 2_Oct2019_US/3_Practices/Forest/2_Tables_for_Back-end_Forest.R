# 
# code: Forest Practice Indicator and Initiative Tables FOR 2019 US DASHBOARD
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: August 2018
# modified: July 2019
# 
# ---- inputs ----
#  1) Forest-specific data tables (in 1_Nov2018/2_FlatDataFiles/ConsDB_Input)
# 
# ---- outputs ----
#  1) Forest-specific back-end tables -- ready to be consolidated with other Practices, to go into back-end:
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


#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Global Context ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 2.1 Context - State ----

# -- FOREST EXTENT - BOREAL

Dim_Context_State_Forest_A <- 
  data.frame(Indicator_Type_Key="GCS_FR_A",
             Indicator_Name="Forest area (millions of hectares)",
             Indicator_Label="Total Area of Tree Cover",
             Panel_Label="Forest Extent",
             Panel="State",
             Indicator_Subcategory="Boreal",
             Indicator_Unit="M ha",
             Data_Source="FAO, Global Forest Resources Assessment",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_State_Forest_A <-
  read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/FRA2015_data.csv') %>%
  subset(.,Geography=="Bor") %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Forests"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_State_Forest_A$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Value/1000,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))

# -- FOREST EXTENT - TEMPERATE

Dim_Context_State_Forest_B <- 
  data.frame(Indicator_Type_Key="GCS_FR_B",
             Indicator_Name="Forest area (millions of hectares)",
             Indicator_Label="Total Area of Tree Cover",
             Panel_Label="Forest Extent",
             Panel="State",
             Indicator_Subcategory="Temperate",
             Indicator_Unit="M ha",
             Data_Source="FAO, Global Forest Resources Assessment",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_State_Forest_B <-
  read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/FRA2015_data.csv') %>%
  subset(.,Geography=="Temp") %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Forests"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_State_Forest_B$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Value/1000,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))

# -- FOREST EXTENT - TROPICAL/SUB-TROPICAL

Dim_Context_State_Forest_C <- 
  data.frame(Indicator_Type_Key="GCS_FR_C",
             Indicator_Name="Forest area (millions of hectares)",
             Indicator_Label="Total Area of Tree Cover",
             Panel_Label="Forest Extent",
             Panel="State",
             Indicator_Subcategory="Tropical & Sub-Tropical",
             Indicator_Unit="M ha",
             Data_Source="FAO, Global Forest Resources Assessment",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_State_Forest_C <-
  read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/FRA2015_data.csv') %>%
  subset(.,Geography=="Trp" | Geography=="SubTrp") %>%
  group_by(Year) %>%
  summarise(Value=sum(Value)) %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Forests"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_State_Forest_C$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Value/1000,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))


# Add Panel-specific measures

Dim_Context_State_Forest_A <-
  Dim_Context_State_Forest_A %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(unique(rbind(Fact_Context_State_Forest_A$Year_Key,Fact_Context_State_Forest_B$Year_Key,Fact_Context_State_Forest_C$Year_Key))),
         Panel_Max_Year=max(unique(rbind(Fact_Context_State_Forest_A$Year_Key,Fact_Context_State_Forest_B$Year_Key,Fact_Context_State_Forest_C$Year_Key))))

Dim_Context_State_Forest_B <-
  Dim_Context_State_Forest_B %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(unique(rbind(Fact_Context_State_Forest_A$Year_Key,Fact_Context_State_Forest_B$Year_Key,Fact_Context_State_Forest_C$Year_Key))),
         Panel_Max_Year=max(unique(rbind(Fact_Context_State_Forest_A$Year_Key,Fact_Context_State_Forest_B$Year_Key,Fact_Context_State_Forest_C$Year_Key))))

Dim_Context_State_Forest_C <-
  Dim_Context_State_Forest_C %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(unique(rbind(Fact_Context_State_Forest_A$Year_Key,Fact_Context_State_Forest_B$Year_Key,Fact_Context_State_Forest_C$Year_Key))),
         Panel_Max_Year=max(unique(rbind(Fact_Context_State_Forest_A$Year_Key,Fact_Context_State_Forest_B$Year_Key,Fact_Context_State_Forest_C$Year_Key))))


# ---- 2.2 Context - Threat ----

# -- FOREST LOSS - LOSS

Dim_Context_Threat_Forest_A <- 
  data.frame(Indicator_Type_Key="GCT_FR_A",
             Indicator_Name="Tree cover loss (millions of hectares per year)",
             Indicator_Label="Tree Cover Loss & Forest Fragmentation",
             Panel_Label="Forest Loss",
             Panel="Threat",
             Indicator_Subcategory="Loss",
             Indicator_Unit="M ha per year",
             Data_Source="Global Forest Watch",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_Threat_Forest_A <-
  read.xlsx('2_Oct2019_US/2_FlatDataFiles/ConsDB_Input_2019/GFW_treeloss_bydriver_2019_0723.xlsx', sheetName="Sheet1") %>%
  subset(.,Geography=="World" & Loss_type=="Total Loss") %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Forests"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_Threat_Forest_A$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))

# -- FOREST LOSS - FRAGMENTATION

Dim_Context_Threat_Forest_B <- 
  data.frame(Indicator_Type_Key="GCT_FR_B",
             Indicator_Name="Forest fragmentation (non-core:core ratio)",
             Indicator_Label="Tree Cover Loss & Forest Fragmentation",
             Panel_Label="Forest Loss",
             Panel="Threat",
             Indicator_Subcategory="Fragmentation",
             Indicator_Unit="Non-core : Core",
             Data_Source="Aurelie Shapiro, WWF-DE",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_Threat_Forest_B <-
  read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/Shapiro_fragmentation_data_2018_0901.csv') %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Forests"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_Threat_Forest_B$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))

# Add Panel-specific measures

Dim_Context_Threat_Forest_A <-
  Dim_Context_Threat_Forest_A %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(unique(rbind(Fact_Context_Threat_Forest_A$Year_Key,Fact_Context_Threat_Forest_B$Year_Key))),
         Panel_Max_Year=max(unique(rbind(Fact_Context_Threat_Forest_A$Year_Key,Fact_Context_Threat_Forest_B$Year_Key))))

Dim_Context_Threat_Forest_B <-
  Dim_Context_Threat_Forest_B %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(unique(rbind(Fact_Context_Threat_Forest_A$Year_Key,Fact_Context_Threat_Forest_B$Year_Key))),
         Panel_Max_Year=max(unique(rbind(Fact_Context_Threat_Forest_A$Year_Key,Fact_Context_Threat_Forest_B$Year_Key))))


# ---- 2.3 Context - Response ----

# -- FOREST PROTECTION - PROTECTED AREAS

Dim_Context_Response_Forest_A <- 
  data.frame(Indicator_Type_Key="GCR_FR_A",
             Indicator_Name="Global terrestrial protected area coverage (M ha)",
             Indicator_Label="Protected & FSC Certified Areas",
             Panel_Label="Forest Protection",
             Panel="Response",
             Indicator_Subcategory="Protected",
             Indicator_Unit="M ha",
             Data_Source="WDPA",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_Response_Forest_A <-
  read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/WDPA_time.csv') %>%
  transmute(Year_Key=YEAR,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Forests"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_Response_Forest_A$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Land_AREA_MHA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))

# FOREST PROTECTION - FSC Coverage

Dim_Context_Response_Forest_B <- 
  data.frame(Indicator_Type_Key="GCR_FR_B",
             Indicator_Name="FSC certified forest area (M ha)",
             Indicator_Label="Protected & FSC Certified Areas",
             Panel_Label="Forest Protection",
             Panel="Response",
             Indicator_Subcategory="FSC Certified",
             Indicator_Unit="M ha",
             Data_Source="FSC Annual Reports",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_Response_Forest_B <-
  read.xlsx('2_Oct2019_US/2_FlatDataFiles/ConsDB_Input_2019/FSC_area_2019_0711.xlsx',sheetName="FSC_area_2017_0915") %>%
  filter(!is.na(Year)) %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Forests"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_Response_Forest_B$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))

# Add Panel-specific measures

Dim_Context_Response_Forest_A <-
  Dim_Context_Response_Forest_A %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(unique(rbind(Fact_Context_Response_Forest_A$Year_Key,Fact_Context_Response_Forest_B$Year_Key))),
         Panel_Max_Year=max(unique(rbind(Fact_Context_Response_Forest_A$Year_Key,Fact_Context_Response_Forest_B$Year_Key))))

Dim_Context_Response_Forest_B <-
  Dim_Context_Response_Forest_B %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(unique(rbind(Fact_Context_Response_Forest_A$Year_Key,Fact_Context_Response_Forest_B$Year_Key))),
         Panel_Max_Year=max(unique(rbind(Fact_Context_Response_Forest_A$Year_Key,Fact_Context_Response_Forest_B$Year_Key))))


# ---- 2.4 Consolidated Forest-specific Global Context tables ----

Dim_Context_Forest <- 
  rbind.data.frame(Dim_Context_State_Forest_A,
                   Dim_Context_State_Forest_B,
                   Dim_Context_State_Forest_C,
                   Dim_Context_Threat_Forest_A,
                   Dim_Context_Threat_Forest_B,
                   Dim_Context_Response_Forest_A,
                   Dim_Context_Response_Forest_B)

Fact_Context_Forest <-
  rbind.data.frame(Fact_Context_State_Forest_A,
                   Fact_Context_State_Forest_B,
                   Fact_Context_State_Forest_C,
                   Fact_Context_Threat_Forest_A,
                   Fact_Context_Threat_Forest_B,
                   Fact_Context_Response_Forest_A,
                   Fact_Context_Response_Forest_B)


#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Global 2030 Outcomes ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 3.1 Forest Outcome 1 - RESILIENT FORESTS  ----

# -- EFFECTIVE PROTECTION

Dim_Global_2030_Outcome1_Forest_A <- 
  data.frame(Indicator_Type_Key="OUT1_FR_A",
             Indicator_Name="Effectively managed and protected forests",
             Indicator_Label="Effective Protection & Management",
             Indicator_Subcategory="Reduced Degradation* & FSC Certification",
             Indicator_Unit="% of total forest area",
             Data_source="FSC for Effective Management; FORTHCOMING for Effective Protection (either fragmentation data (WWF-DE) or global drivers of loss & WDPA data)",
             Indicator_Target=50,
             Indicator_Type="Outcome",
             Panel_Label="Resilient Forests",
             Display_Order=1,
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Global_2030_Outcome1_Forest_A <-
  data.frame(Year_Key=9999,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Forests"],length(1)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome1_Forest_A$Indicator_Type_Key,length(1)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Forests" &
                                                                   grepl("Manage",practice_outcome_key_ref$practice_outcome)], length(1)),
            Indicator_Value=NA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Indicator_Target=NA)

# Add Indicator_Latest_Year and Indicator_Latest_Value based on Fact table

Dim_Global_2030_Outcome1_Forest_A <- 
  Dim_Global_2030_Outcome1_Forest_A %>%
  mutate(Indicator_Latest_Year=max(Fact_Global_2030_Outcome1_Forest_A$Year_Key,na.rm=T),
         Indicator_Latest_Value=Fact_Global_2030_Outcome1_Forest_A$Indicator_Value[Fact_Global_2030_Outcome1_Forest_A$Year_Key==Indicator_Latest_Year])


# ---- 3.2 Forest Outcome 2 - HALT DEFORESTATION ----

Dim_Global_2030_Outcome2_Forest_A <- 
  data.frame(Indicator_Type_Key="OUT2_FR_A",
             Indicator_Name="Commodity Driven Deforestation (M ha per year)",
             Indicator_Label="Commodity Driven Forest Cover Loss",
             Indicator_Subcategory=NA,
             Indicator_Unit="M ha per year",
             Data_source="Global Forest Watch - Curtis et al (2018) Global drivers of forest loss",
             Indicator_Target=0,
             Indicator_Type="Outcome",
             Panel_Label="Halt Deforestation",
             Display_Order=2,
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Global_2030_Outcome2_Forest_A <-
  read.xlsx('2_Oct2019_US/2_FlatDataFiles/ConsDB_Input_2019/GFW_treeloss_bydriver_2019_0723.xlsx', sheetName="Sheet1") %>%
  subset(.,Loss_type=="Commodity Driven Deforestation") %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Forests"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome2_Forest_A$Indicator_Type_Key, length(Year_Key)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Forests" &
                                                                   grepl("Deforestation",practice_outcome_key_ref$practice_outcome)], length(Year_Key)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Indicator_Target=NA)

# Add Indicator_Latest_Year and Indicator_Latest_Value based on Fact table

Dim_Global_2030_Outcome2_Forest_A <- 
  Dim_Global_2030_Outcome2_Forest_A %>%
  mutate(Indicator_Latest_Year=max(Fact_Global_2030_Outcome2_Forest_A$Year_Key,na.rm=T),
         Indicator_Latest_Value=Fact_Global_2030_Outcome2_Forest_A$Indicator_Value[Fact_Global_2030_Outcome2_Forest_A$Year_Key==Indicator_Latest_Year])

# Add target value to Fact table

Fact_Global_2030_Outcome2_Forest_A <-
  rbind.data.frame(Fact_Global_2030_Outcome2_Forest_A,
                   data.frame(Year_Key=2030,
                              Practice_Key=practice_key_ref$id[practice_key_ref$practice_name=="Forests"],
                              Indicator_Type_Key=Dim_Global_2030_Outcome2_Forest_A$Indicator_Type_Key,
                              Practice_Outcome_Key=practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Forests" &
                                                                                 grepl("Deforestation",practice_outcome_key_ref$practice_outcome)],
                              Indicator_Value=NA,
                              Indicator_Upper_Value=NA,
                              Indicator_Lower_Value=NA,
                              Indicator_Target=Dim_Global_2030_Outcome2_Forest_A$Indicator_Target))


# ---- 3.3 Forest Outcome 3 - FOREST RESTORATION ----

# -- COMMITTED

Dim_Global_2030_Outcome3_Forest_A <- 
  data.frame(Indicator_Type_Key="OUT3_FR_A",
             Indicator_Name="Commitments to forest restoration (millions ha by 2030)",
             Indicator_Label="Total Area Committed, Funded, & Restored",
             Indicator_Subcategory="Committed",
             Indicator_Unit="M ha",
             Data_source="Bonn Challenge, country-level commitments",
             Indicator_Target=350,
             Indicator_Type="Outcome",
             Panel_Label="Forest Restoration",
             Display_Order=3,
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Global_2030_Outcome3_Forest_A <-
  read.xlsx('2_Oct2019_US/2_FlatDataFiles/ConsDB_Input_2019/May_2020_updates/BonnChallenge_commitments_2020_0409.xlsx', 
            sheetName="Sheet1") %>%
  filter(!is.na(Commitment_Year)) %>%
  group_by(Commitment_Year) %>%
  summarise(Indicator_Value=sum(Committed_Area)) %>%
  transmute(Year_Key=Commitment_Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Forests"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome3_Forest_A$Indicator_Type_Key, length(Year_Key)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Forests" &
                                                                   grepl("Restoration",practice_outcome_key_ref$practice_outcome)], length(Year_Key)),
            Indicator_Value=cumsum(Indicator_Value),
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Indicator_Target=NA)

# Add Indicator_Latest_Year and Indicator_Latest_Value based on Fact table

Dim_Global_2030_Outcome3_Forest_A <- 
  Dim_Global_2030_Outcome3_Forest_A %>%
  mutate(Indicator_Latest_Year=max(Fact_Global_2030_Outcome3_Forest_A$Year_Key,na.rm=T),
         Indicator_Latest_Value=Fact_Global_2030_Outcome3_Forest_A$Indicator_Value[Fact_Global_2030_Outcome3_Forest_A$Year_Key==Indicator_Latest_Year])

# Add target value to Fact table

Fact_Global_2030_Outcome3_Forest_A <-
  rbind.data.frame(Fact_Global_2030_Outcome3_Forest_A,
                   data.frame(Year_Key=2030,
                              Practice_Key=practice_key_ref$id[practice_key_ref$practice_name=="Forests"],
                              Indicator_Type_Key=Dim_Global_2030_Outcome3_Forest_A$Indicator_Type_Key,
                              Practice_Outcome_Key=practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Forests" &
                                                                                 grepl("Restoration",practice_outcome_key_ref$practice_outcome)],
                              Indicator_Value=NA,
                              Indicator_Upper_Value=NA,
                              Indicator_Lower_Value=NA,
                              Indicator_Target=Dim_Global_2030_Outcome3_Forest_A$Indicator_Target))


# -- FUNDED

Dim_Global_2030_Outcome3_Forest_B <- 
  data.frame(Indicator_Type_Key="OUT3_FR_B",
             Indicator_Name="Area of commitments that are funded (millions ha)",
             Indicator_Label="Total Area Committed, Funded, & Restored",
             Indicator_Subcategory="Funded*",
             Indicator_Unit="M ha",
             Data_source="Bonn Challenge, country-level commitments",
             Indicator_Target=350,
             Indicator_Type="Outcome",
             Panel_Label="Forest Restoration",
             Display_Order=3,
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Global_2030_Outcome3_Forest_B <-
  data.frame(Year_Key=2017,
            Practice_Key=practice_key_ref$id[practice_key_ref$practice_name=="Forests"],
            Indicator_Type_Key=Dim_Global_2030_Outcome3_Forest_B$Indicator_Type_Key,
            Practice_Outcome_Key=practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Forests" &
                                                                   grepl("Restoration",practice_outcome_key_ref$practice_outcome)],
            Indicator_Value=NA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Indicator_Target=NA)

# Add Indicator_Latest_Year and Indicator_Latest_Value based on Fact table

Dim_Global_2030_Outcome3_Forest_B <- 
  Dim_Global_2030_Outcome3_Forest_B %>%
  mutate(Indicator_Latest_Year=max(Fact_Global_2030_Outcome3_Forest_B$Year_Key,na.rm=T),
         Indicator_Latest_Value=Fact_Global_2030_Outcome3_Forest_B$Indicator_Value[Fact_Global_2030_Outcome3_Forest_B$Year_Key==Indicator_Latest_Year])

# Add target value to Fact table

Fact_Global_2030_Outcome3_Forest_B <-
  rbind.data.frame(Fact_Global_2030_Outcome3_Forest_B,
                   data.frame(Year_Key=2030,
                              Practice_Key=practice_key_ref$id[practice_key_ref$practice_name=="Forests"],
                              Indicator_Type_Key=Dim_Global_2030_Outcome3_Forest_B$Indicator_Type_Key,
                              Practice_Outcome_Key=practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Forests" &
                                                                                 grepl("Restoration",practice_outcome_key_ref$practice_outcome)],
                              Indicator_Value=NA,
                              Indicator_Upper_Value=NA,
                              Indicator_Lower_Value=NA,
                              Indicator_Target=Dim_Global_2030_Outcome3_Forest_B$Indicator_Target))


# -- RESTORED

Dim_Global_2030_Outcome3_Forest_C <- 
  data.frame(Indicator_Type_Key="OUT3_FR_C",
             Indicator_Name="Area of forest restored (millions ha)",
             Indicator_Label="Total Area Committed, Funded, & Restored",
             Indicator_Subcategory="Restored*",
             Indicator_Unit="M ha",
             Data_source="Bonn Challenge, country-level commitments",
             Indicator_Target=350,
             Indicator_Type="Outcome",
             Panel_Label="Forest Restoration",
             Display_Order=3,
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Global_2030_Outcome3_Forest_C <-
  data.frame(Year_Key=2017,
            Practice_Key=practice_key_ref$id[practice_key_ref$practice_name=="Forests"],
            Indicator_Type_Key=Dim_Global_2030_Outcome3_Forest_C$Indicator_Type_Key,
            Practice_Outcome_Key=practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Forests" &
                                                                   grepl("Restoration",practice_outcome_key_ref$practice_outcome)],
            Indicator_Value=NA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Indicator_Target=NA)

# Add Indicator_Latest_Year and Indicator_Latest_Value based on Fact table

Dim_Global_2030_Outcome3_Forest_C <- 
  Dim_Global_2030_Outcome3_Forest_C %>%
  mutate(Indicator_Latest_Year=max(Fact_Global_2030_Outcome3_Forest_C$Year_Key,na.rm=T),
         Indicator_Latest_Value=Fact_Global_2030_Outcome3_Forest_C$Indicator_Value[Fact_Global_2030_Outcome3_Forest_C$Year_Key==Indicator_Latest_Year])

# Add target value to Fact table

Fact_Global_2030_Outcome3_Forest_C <-
  rbind.data.frame(Fact_Global_2030_Outcome3_Forest_C,
                   data.frame(Year_Key=2030,
                              Practice_Key=practice_key_ref$id[practice_key_ref$practice_name=="Forests"],
                              Indicator_Type_Key=Dim_Global_2030_Outcome3_Forest_C$Indicator_Type_Key,
                              Practice_Outcome_Key=practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Forests" &
                                                                                 grepl("Restoration",practice_outcome_key_ref$practice_outcome)],
                              Indicator_Value=NA,
                              Indicator_Upper_Value=NA,
                              Indicator_Lower_Value=NA,
                              Indicator_Target=Dim_Global_2030_Outcome3_Forest_C$Indicator_Target))


# ---- 3.4 Consolidated Forest-specific Global 2030 Outcome tables ----

Dim_Global_2030_Outcome_Forest <- 
  rbind.data.frame(Dim_Global_2030_Outcome1_Forest_A,
                   Dim_Global_2030_Outcome2_Forest_A,
                   Dim_Global_2030_Outcome3_Forest_A,
                   Dim_Global_2030_Outcome3_Forest_B,
                   Dim_Global_2030_Outcome3_Forest_C)

Fact_Global_2030_Outcome_Forest <-
  rbind.data.frame(Fact_Global_2030_Outcome1_Forest_A,
                   Fact_Global_2030_Outcome2_Forest_A,
                   Fact_Global_2030_Outcome3_Forest_A,
                   Fact_Global_2030_Outcome3_Forest_B,
                   Fact_Global_2030_Outcome3_Forest_C)

#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 4: Initiatives ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 4.1 Load data ----

dim.initiatives.forests <- 
  dim.initiatives %>% subset(Practice=="Forests") 

dim.initiative.indicators.forests <-
  dim.initiative.indicators %>% subset(Practice=="Forests")

fact.initiative.indicators.forests <-
  fact.initiative.indicators %>% subset(Practice=="Forests")

dim.initiative.milestones.forests <-
  dim.initiative.milestones %>% subset(Practice=="Forests")

pie.type.forests <-
  pie.type %>% subset(Practice=="Forests")


# ---- 4.2 Forest-specific Dim_Initiative ----

Dim_Initiative_Forest <-
  dim.initiatives.forests %>%
  transmute(Initiative_Key=Initiative.key,
            Initiative_Name=Initiative,
            Initiative_Status=Overall.status,
            Initiative_Status_Justification=Overall.just,
            Initiative_Goal=Initiative.statement,
            Global_Initiative=Global.initiative,
            US_Initiative=US.initiative,
            Display_Order=Display.order)


# ---- 4.3 Forest-specific Dim_Initiative_Indicator_Type ----

Dim_Initiative_Indicator_Forest <-
  left_join(dim.initiative.indicators.forests,
            pie.type.forests[,c("Initiative.indicator.key","pie.type","amount.achieved","amount.remaining","max.year.value")],
            by="Initiative.indicator.key") %>%
  transmute(Indicator_Type_Key=new.key,
            Indicator_Type=Indicator.type,
            Indicator_Name=ifelse(!is.na(Indicator.name),as.character(Indicator.name),"FORTHCOMING"),
            Indicator_Label=ifelse(!is.na(Indicator.label),as.character(Indicator.label),"Not Yet Identified"),
            Indicator_Subcategory=Subcategory,
            Indicator_Unit=Units,
            Data_Source=Source,
            Indicator_Target=as.numeric(Target),
            Display_Order=Display.order,
            Indicator_Statement=Statement,
            Indicator_Label_Abbr=toupper(Indicator.label.abbr),
            Subcategory_Abbr=Subcategory.abbr,
            Amount_Achieved=amount.achieved,
            Amount_Remaining=amount.remaining,
            Pie_Type=pie.type,
            Indicator_Label_Caps=toupper(Indicator_Label),
            Indicator_Latest=max.year.value)


# ---- 4.4 Forest-specific Fact_Initiative_Indicators ----

Fact_Initiative_Indicator_Forest <-
  left_join(fact.initiative.indicators.forests,pie.type.forests[,c("Initiative.indicator.key","target.year","Target")],by="Initiative.indicator.key") %>%
  left_join(dim.initiative.indicators.forests[,c("Initiative.indicator.key","Units","new.key")],by="Initiative.indicator.key") %>%
  transmute(Year_Key=ifelse(!is.na(Year),Year,9999),
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Forests"],length(Year_Key)),
            Initiative_Key=Initiative.key,
            Indicator_Type_Key=new.key,
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Value_Trend=ifelse(grepl("reduction",Units,ignore.case=T)==T,-(Value),Value),
            Indicator_Target=ifelse(!is.na(Target) & Year==target.year,Target,NA),
            Target_Trend=ifelse(grepl("reduction",Units,ignore.case=T)==T,-(Indicator_Target),Indicator_Target))


# ---- 4.5 Forest-specific Fact_Initiative_Financials ----

Fact_Initiative_Financials_Forest <-
  dim.initiatives.forests %>%
  transmute(Date_Key=Date,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Forests"],length(Date_Key)),
            Initiative_Key=Initiative.key,
            Amount_Needed=Funds.needed,
            Amount_Secured=Funds.secured,
            Amount_Anticipated=Funds.anticipated,
            Amount_Remaining=Funds.needed-Funds.secured-Funds.anticipated)


# ---- 4.6 Forest-specific Milestone_Group_Bridge ----

Milestone_Group_Bridge_Forest <-
  left_join(dim.initiative.milestones.forests, dim.initiatives.forests, by=c("Initiative", "Practice")) %>%
  transmute(Milestone_Key=Milestone.key,
            Initiative_Key=Initiative.key)


# ---- 4.7 Forest-specific Dim_Milestone ----

Dim_Milestone_Forest <-
  dim.initiative.milestones.forests %>%
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

rm(Dim_Context_State_Forest_A,
   Dim_Context_State_Forest_B,
   Dim_Context_State_Forest_C,
   Dim_Context_Threat_Forest_A,
   Dim_Context_Threat_Forest_B,
   Dim_Context_Response_Forest_A,
   Dim_Context_Response_Forest_B,
   Fact_Context_State_Forest_A,
   Fact_Context_State_Forest_B,
   Fact_Context_State_Forest_C,
   Fact_Context_Threat_Forest_A,
   Fact_Context_Threat_Forest_B,
   Fact_Context_Response_Forest_A,
   Fact_Context_Response_Forest_B,
   Dim_Global_2030_Outcome1_Forest_A,
   Dim_Global_2030_Outcome2_Forest_A,
   Dim_Global_2030_Outcome3_Forest_A,
   Dim_Global_2030_Outcome3_Forest_B,
   Dim_Global_2030_Outcome3_Forest_C,
   Fact_Global_2030_Outcome1_Forest_A,
   Fact_Global_2030_Outcome2_Forest_A,
   Fact_Global_2030_Outcome3_Forest_A,
   Fact_Global_2030_Outcome3_Forest_B,
   Fact_Global_2030_Outcome3_Forest_C,
   dim.initiatives.forests,
   dim.initiative.indicators.forests,
   fact.initiative.indicators.forests,
   dim.initiative.milestones.forests,
   pie.type.forests)
