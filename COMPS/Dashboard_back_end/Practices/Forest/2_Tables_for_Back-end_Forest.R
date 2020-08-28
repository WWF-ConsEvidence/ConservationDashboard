# 
# code: Forest Practice Indicator and Initiative Tables FOR COMPS
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: August 2018
# modified: June 2020
# 
# ---- inputs ----
#  1) Forest-specific data tables
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
#  1) Global Context
#  2) Global 2030 Outcomes
#  2) Initiatives
# 
#
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: Global Context ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 1.1 Context - State ----

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
  import(last.file(dir.nam = dir.nam.Forest, nam = 'FRA_data')) %>%
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
  import(last.file(dir.nam = dir.nam.Forest, nam = 'FRA_data')) %>%
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
  import(last.file(dir.nam = dir.nam.Forest, nam = 'FRA_data')) %>%
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


# ---- 1.2 Context - Threat ----

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
  import(last.file(dir.nam = dir.nam.Forest, nam = 'GFW_treeloss_bydriver')) %>%
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
  import(last.file(dir.nam = dir.nam.Forest, nam = 'Shapiro_fragmentation_data')) %>%
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


# ---- 1.3 Context - Response ----

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
  import(last.file(dir.nam = dir.nam.Forest, nam = 'WDPA_time')) %>%
  filter(YEAR>1989) %>%
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
  import(last.file(dir.nam = dir.nam.Forest, nam = 'FSC_area')) %>%
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


# ---- 1.4 Consolidated Forest-specific Global Context tables ----

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
# ---- SECTION 2: Global 2030 Outcomes ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 2.1 Forest Outcome 1 - RESILIENT FORESTS  ----

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


# ---- 2.2 Forest Outcome 2 - HALT DEFORESTATION ----

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
  import(last.file(dir.nam = dir.nam.Forest, nam = 'GFW_treeloss_bydriver')) %>%
  subset(.,Loss_type=="Commodity driven deforestation") %>%
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


# ---- 2.3 Forest Outcome 3 - FOREST RESTORATION ----

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
  import(last.file(dir.nam = dir.nam.Forest, nam = 'BonnChallenge_commitments')) %>%
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


# ---- 2.4 Consolidated Forest-specific Global 2030 Outcome tables ----

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
# ---- SECTION 3: Initiatives ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 3.1 Load data ----

dim_initiatives_Forest <- 
  dim_initiatives %>% subset(goal=="Forests") 

dim_initiative_indicators_Forest <-
  dim_initiative_indicators %>% subset(goal=="Forests")

fact_initiative_indicators_Forest <-
  fact_initiative_indicators %>% subset(goal=="Forests")

dim_initiative_milestones_Forest <-
  dim_initiative_milestones %>% subset(goal=="Forests")

pie_type_Forest <-
  pie_type %>% subset(goal=="Forests")


# ---- 3.2 Forest-specific Dim_Initiative ----

Dim_Initiative_Forest <-
  dim_initiatives_Forest %>%
  transmute(Initiative_Key=initiativekey,
            Initiative_Name=initiative,
            Initiative_Status=initiativestatus,
            Initiative_Status_Justification=initiativejust,
            Initiative_Goal=initiativestatement,
            Global_Initiative=globalinitiative,
            US_Initiative=usinitiative,
            Display_Order=displayorder)


# ---- 3.3 Forest-specific Dim_Initiative_Indicator_Type ----

Dim_Initiative_Indicator_Forest <-
  left_join(dim_initiative_indicators_Forest,
            pie_type_Forest[,c("indicatorkey","pie.type","amount.achieved","amount.remaining","max.year.value")],
            by="indicatorkey") %>%
  transmute(Indicator_Type_Key=indicatorkey,
            Indicator_Type=indicatortype,
            Indicator_Name=ifelse(!is.na(indicatorlabel),as.character(indicatorlabel),"FORTHCOMING"), # we no longer ask for different indicator names and labels. Therefore, this data field is no longer functional
            Indicator_Label=ifelse(!is.na(indicatorlabel),as.character(indicatorlabel),"Not Yet Identified"),
            Indicator_Subcategory=subcat,
            Indicator_Unit=indicatorunits,
            Data_Source=indicatorsource,
            Indicator_Target=as.numeric(subcattarget),
            Display_Order=displayorder,
            Indicator_Statement=statement,
            Indicator_Label_Abbr=toupper(indicatorlabelabbr), # MUST MANUALLY CALCULATE!!
            Subcategory_Abbr=subcatlabelabbr, # MUST MANUALLY CALCULATE!!
            Amount_Achieved=amount.achieved,
            Amount_Remaining=amount.remaining,
            Pie_Type=pie.type,
            Indicator_Label_Caps=toupper(indicatorlabel),
            Indicator_Latest=max.year.value)


# ---- 3.4 Forest-specific Fact_Initiative_Indicators ----

Fact_Initiative_Indicator_Forest <-
  left_join(fact_initiative_indicators_Forest,pie_type_Forest[,c("indicatorkey","target.year","subcattarget")],by="indicatorkey") %>%
  left_join(dim_initiative_indicators_Forest[,c("indicatorkey","indicatorunits")],by="indicatorkey") %>%
  transmute(Year_Key=ifelse(!is.na(Year),Year,9999),
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Forests"],length(Year_Key)),
            Initiative_Key=initiativekey,
            Indicator_Type_Key=indicatorkey,
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Value_Trend=ifelse(grepl("reduction",indicatorunits,ignore.case=T)==T,-(Value),Value),
            Indicator_Target=ifelse(!is.na(subcattarget) & Year==target.year,subcattarget,NA),
            Target_Trend=ifelse(grepl("reduction",indicatorunits,ignore.case=T)==T,-(Indicator_Target),Indicator_Target))


# ---- 3.5 Forest-specific Fact_Initiative_Financials ----

Fact_Initiative_Financials_Forest <-
  dim_initiatives_Forest %>%
  transmute(Date_Key=date,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Forests"],length(Date_Key)),
            Initiative_Key=initiativekey,
            Amount_Needed=fundsneeded,
            Amount_Secured=fundssecured,
            Amount_Anticipated=fundsanticipated,
            Amount_Remaining=fundsneeded-fundssecured-fundsanticipated)


# ---- 3.6 Forest-specific Milestone_Group_Bridge ----

Milestone_Group_Bridge_Forest <-
  left_join(dim_initiative_milestones_Forest, dim_initiatives_Forest, by=c("initiative", "goal")) %>%
  transmute(Milestone_Key=milestonekey,
            Initiative_Key=initiativekey)


# ---- 3.7 Forest-specific Dim_Milestone ----

Dim_Milestone_Forest <-
  dim_initiative_milestones_Forest %>%
  transmute(Milestone_Surrogate_Key="",
            Milestone_Key=milestonekey,
            Milestone_Name=milestone,
            Milestone_Target=target,
            Milestone_Status=milestonestatus,
            Milestone_Status_Justification=milestonejust,
            Creation_Date=milestonecreation,
            Effective_Start_Date=milestonestart,
            Effective_End_Date=milestoneend,
            Is_Active=milestoneactive)



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
   dim_initiatives_Forest,
   dim_initiative_indicators_Forest,
   fact_initiative_indicators_Forest,
   dim_initiative_milestones_Forest,
   pie_type_Forest)
