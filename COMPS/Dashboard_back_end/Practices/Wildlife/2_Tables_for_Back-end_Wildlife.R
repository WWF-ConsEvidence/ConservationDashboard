# 
# code: Wildlife Practice Indicator and Initiative Tables FOR COMPS
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: August 2018
# modified: June 2020
# 
# ---- inputs ----
#  1) Wildlife-specific data tables
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
#  1) Global Context
#  2) Global 2030 Outcomes
#  3) Initiatives
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

# -- GLOBAL BIODIVERSITY - LIVING PLANET INDEX

Dim_Context_State_Wildlife_A <- 
  data.frame(Indicator_Type_Key="GCS_WL_A",
             Indicator_Name="Global Living Planet Index",
             Indicator_Label="Population Trends & Extinction Risk",
             Panel_Label="Global Biodiversity",
             Panel="State",
             Indicator_Subcategory="Living Planet Index",
             Indicator_Unit="Index",
             Data_Source="Zoological Society of London; August 2018 Living Planet Index database",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_State_Wildlife_A <-
  import(last.file(dir.nam = dir.nam.Wildlife, nam = 'Global_LPI_fromflatfile')) %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_State_Wildlife_A$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Value,
            Indicator_Upper_Value=Upper,
            Indicator_Lower_Value=Lower,
            Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))


# -- GLOBAL BIODIVERSITY - RED LIST INDEX

Dim_Context_State_Wildlife_B <- 
  data.frame(Indicator_Type_Key="GCS_WL_B",
             Indicator_Name="Red List Index",
             Indicator_Label="Population Trends & Extinction Risk",
             Panel_Label="Global Biodiversity",
             Panel="State",
             Indicator_Subcategory="Red List Index",
             Indicator_Unit="Index",
             Data_Source="SDG Indicator Bank -- IUCN",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_State_Wildlife_B <-
  import(last.file(dir.nam = dir.nam.Wildlife, nam = 'SDG_15.5.1_RLI_dl')) %>%
  transmute(Year_Key=unique(TimePeriod),
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_State_Wildlife_B$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Value,
            Indicator_Upper_Value=UpperBound,
            Indicator_Lower_Value=LowerBound,
            Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))

# Add Panel-specific measures

Dim_Context_State_Wildlife_A <-
  Dim_Context_State_Wildlife_A %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(unique(rbind(Fact_Context_State_Wildlife_A$Year_Key,Fact_Context_State_Wildlife_B$Year_Key))),
         Panel_Max_Year=max(unique(rbind(Fact_Context_State_Wildlife_A$Year_Key,Fact_Context_State_Wildlife_B$Year_Key))))

Dim_Context_State_Wildlife_B <-
  Dim_Context_State_Wildlife_B %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(unique(rbind(Fact_Context_State_Wildlife_A$Year_Key,Fact_Context_State_Wildlife_B$Year_Key))),
         Panel_Max_Year=max(unique(rbind(Fact_Context_State_Wildlife_A$Year_Key,Fact_Context_State_Wildlife_B$Year_Key))))


# ---- 1.2 Context - Threat ----

# -- HABITAT LOSS

Dim_Context_Threat_Wildlife_A <- 
  data.frame(Indicator_Type_Key="GCT_WL_A",
             Indicator_Name="Global tree cover loss (M ha per year)",
             Indicator_Label="Terrestrial & Marine* Habitat Loss",
             Panel_Label="Habitat Loss",
             Panel="Threat",
             Indicator_Subcategory="Tree Cover Loss",
             Indicator_Unit="M ha per year",
             Data_Source="Global Forest Watch",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_Threat_Wildlife_A <-
  import(last.file(dir.nam = dir.nam.Forest, nam = 'GFW_treeloss_bydriver')) %>%
  subset(.,Geography=="World" & Loss_type=="Total Loss") %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_Threat_Wildlife_A$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))

# Add Panel-specific measures

Dim_Context_Threat_Wildlife_A <-
  Dim_Context_Threat_Wildlife_A %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(Fact_Context_Threat_Wildlife_A$Year_Key,na.rm=T),
         Panel_Max_Year=max(Fact_Context_Threat_Wildlife_A$Year_Key,na.rm=T))


# ---- 1.3 Context - Response ----

# -- HABITAT PROTECTION - PROTECTED

Dim_Context_Response_Wildlife_A <- 
  data.frame(Indicator_Type_Key="GCR_WL_A",
             Indicator_Name="Global PA Coverage (terrestrial & marine; M ha)",
             Indicator_Label="Protected & Community Conserved* Areas",
             Panel_Label="Habitat Protection",
             Panel="Response",
             Indicator_Subcategory="Protected",
             Indicator_Unit="M ha",
             Data_Source="WDPA",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_Response_Wildlife_A <-
  import(last.file(dir.nam = dir.nam.Forest, nam = 'WDPA_time')) %>%
  transmute(Year_Key=YEAR,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_Response_Wildlife_A$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=TOTAL_MHA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))

# -- HABITAT PROTECTION - COMMUNITY CONSERVED

Dim_Context_Response_Wildlife_B <- 
  data.frame(Indicator_Type_Key="GCR_WL_B",
             Indicator_Name="ICCA Coverage (M ha)",
             Indicator_Label="Protected & Community Conserved* Areas",
             Panel_Label="Habitat Protection",
             Panel="Response",
             Indicator_Subcategory="Community Conserved",
             Indicator_Unit="M ha",
             Data_Source="WDPA",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_Response_Wildlife_B <- 
  import(last.file(dir.nam = dir.nam.Wildlife, nam = 'ICCA_timeseries')) %>%
  subset(.,STATUS_YR>1994) %>%
  transmute(Year_Key=STATUS_YR,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],length(STATUS_YR)),
            Indicator_Type_Key=rep(Dim_Context_Response_Wildlife_B$Indicator_Type_Key,length(STATUS_YR)),
            Indicator_Value=AREA_MHA_TIME,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))

# Add Panel-specific measures

Dim_Context_Response_Wildlife_A <-
  Dim_Context_Response_Wildlife_A %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(unique(rbind(Fact_Context_Response_Wildlife_A$Year_Key,Fact_Context_Response_Wildlife_B$Year_Key))),
         Panel_Max_Year=max(unique(rbind(Fact_Context_Response_Wildlife_A$Year_Key,Fact_Context_Response_Wildlife_B$Year_Key))))

Dim_Context_Response_Wildlife_B <-
  Dim_Context_Response_Wildlife_B %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(unique(rbind(Fact_Context_Response_Wildlife_A$Year_Key,Fact_Context_Response_Wildlife_B$Year_Key))),
         Panel_Max_Year=max(unique(rbind(Fact_Context_Response_Wildlife_A$Year_Key,Fact_Context_Response_Wildlife_B$Year_Key))))


# ---- 1.4 Consolidated Wildlife-specific Global Context tables ----

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
# ---- SECTION 2: Global 2030 Outcomes ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 2.1 Wildlife Outcome 1 - VITAL HABITATS CONSERVED ----

# -- EXTENT

Dim_Global_2030_Outcome1_Wildlife_A <- 
  data.frame(Indicator_Type_Key="OUT1_WL_A",
             Indicator_Name="Global protected area coverage, as percent of total land area",
             Indicator_Label="Extensive, Effective, Connected, & Biologically Important Habitat Protection",
             Indicator_Subcategory="Extent",
             Indicator_Unit="% global area",
             Data_source="WDPA",
             Indicator_Target=30,
             Indicator_Type="Outcome",
             Panel_Label="Vital Habitats Conserved",
             Display_Order=1,
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Global_2030_Outcome1_Wildlife_A <- 
  import(last.file(dir.nam = dir.nam.Forest, nam = 'WDPA_time')) %>%
  transmute(Year_Key=YEAR,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome1_Wildlife_A$Indicator_Type_Key, length(Year_Key)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Wildlife" &
                                                                   grepl("Habitats",practice_outcome_key_ref$practice_outcome)], length(Year_Key)),
            Indicator_Value=TOTAL_PERCENT,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Indicator_Target=NA)

# Add Indicator_Latest_Year and Indicator_Latest_Value based on Fact table

Dim_Global_2030_Outcome1_Wildlife_A <- 
  Dim_Global_2030_Outcome1_Wildlife_A %>%
  mutate(Indicator_Latest_Year=max(Fact_Global_2030_Outcome1_Wildlife_A$Year_Key,na.rm=T),
         Indicator_Latest_Value=Fact_Global_2030_Outcome1_Wildlife_A$Indicator_Value[Fact_Global_2030_Outcome1_Wildlife_A$Year_Key==Indicator_Latest_Year])

# Add target value to Fact table

Fact_Global_2030_Outcome1_Wildlife_A <-
  rbind.data.frame(Fact_Global_2030_Outcome1_Wildlife_A,
                   data.frame(Year_Key=2030,
                              Practice_Key=practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],
                              Indicator_Type_Key=Dim_Global_2030_Outcome1_Wildlife_A$Indicator_Type_Key,
                              Practice_Outcome_Key=practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Wildlife" &
                                                                                  grepl("Habitats",practice_outcome_key_ref$practice_outcome)],
                              Indicator_Value=NA,
                              Indicator_Upper_Value=NA,
                              Indicator_Lower_Value=NA,
                              Indicator_Target=Dim_Global_2030_Outcome1_Wildlife_A$Indicator_Target))


# -- BIOLOGICAL IMPORTANCE

Dim_Global_2030_Outcome1_Wildlife_B <- 
  data.frame(Indicator_Type_Key="OUT1_WL_B",
             Indicator_Name="Percent of total protected area that is within Key Biodiversity Areas (M ha PAs within KBAs / M ha PAs)",
             Indicator_Label="Extensive, Effective, Connected, & Biologically Important Habitat Protection",
             Indicator_Subcategory="Biological Importance",
             Indicator_Unit="% protected area",
             Data_source="WDPA & KBA database",
             Indicator_Target=100,
             Indicator_Type="Outcome",
             Panel_Label="Vital Habitats Conserved",
             Display_Order=1,
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Global_2030_Outcome1_Wildlife_B <-
  import(last.file(dir.nam = dir.nam.Wildlife, nam = 'KBA_timeseries')) %>%
  transmute(Year_Key=year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome1_Wildlife_B$Indicator_Type_Key, length(Year_Key)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Wildlife" &
                                                                   grepl("Habitats",practice_outcome_key_ref$practice_outcome)], length(Year_Key)),
            Indicator_Value=PA_in_KBA_percent,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Indicator_Target=NA)

# Add Indicator_Latest_Year and Indicator_Latest_Value based on Fact table

Dim_Global_2030_Outcome1_Wildlife_B <- 
  Dim_Global_2030_Outcome1_Wildlife_B %>%
  mutate(Indicator_Latest_Year=max(Fact_Global_2030_Outcome1_Wildlife_B$Year_Key,na.rm=T),
         Indicator_Latest_Value=Fact_Global_2030_Outcome1_Wildlife_B$Indicator_Value[Fact_Global_2030_Outcome1_Wildlife_B$Year_Key==Indicator_Latest_Year])

# Add target value to Fact table

Fact_Global_2030_Outcome1_Wildlife_B <-
  rbind.data.frame(Fact_Global_2030_Outcome1_Wildlife_B,
                   data.frame(Year_Key=2030,
                              Practice_Key=practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],
                              Indicator_Type_Key=Dim_Global_2030_Outcome1_Wildlife_B$Indicator_Type_Key,
                              Practice_Outcome_Key=practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Wildlife" &
                                                                                 grepl("Habitats",practice_outcome_key_ref$practice_outcome)],
                              Indicator_Value=NA,
                              Indicator_Upper_Value=NA,
                              Indicator_Lower_Value=NA,
                              Indicator_Target=Dim_Global_2030_Outcome1_Wildlife_B$Indicator_Target))

# -- CONNECTEDNESS

Dim_Global_2030_Outcome1_Wildlife_C <- 
  data.frame(Indicator_Type_Key="OUT1_WL_C",
             Indicator_Name="Percent of global protected area coverage that is well-connected (accounting for boundaries that limit ability to connect)",
             Indicator_Label="Extensive, Effective, Connected, & Biologically Important Habitat Protection",
             Indicator_Subcategory="Connectedness*",
             Indicator_Unit="% protected area",
             Data_source="Saura et al (2018) Protected area connectivity: Shortfalls in global targets and country-level priorities",
             Indicator_Target=100,
             Indicator_Type="Outcome",
             Panel_Label="Vital Habitats Conserved",
             Display_Order=1,
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Global_2030_Outcome1_Wildlife_C <-
  data.frame(Year_Key=2016,
             Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],length(1)),
             Indicator_Type_Key=rep(Dim_Global_2030_Outcome1_Wildlife_C$Indicator_Type_Key, length(1)),
             Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Wildlife" &
                                                                    grepl("Habitats",practice_outcome_key_ref$practice_outcome)], length(1)),
             Indicator_Value=(9.9/14.7)*100,
             Indicator_Upper_Value=NA,
             Indicator_Lower_Value=NA,
             Indicator_Target=NA)

# Add Indicator_Latest_Year and Indicator_Latest_Value based on Fact table

Dim_Global_2030_Outcome1_Wildlife_C <- 
  Dim_Global_2030_Outcome1_Wildlife_C %>%
  mutate(Indicator_Latest_Year=max(Fact_Global_2030_Outcome1_Wildlife_C$Year_Key,na.rm=T),
         Indicator_Latest_Value=Fact_Global_2030_Outcome1_Wildlife_C$Indicator_Value[Fact_Global_2030_Outcome1_Wildlife_C$Year_Key==Indicator_Latest_Year])

# Add target value to Fact table

Fact_Global_2030_Outcome1_Wildlife_C <-
  rbind.data.frame(Fact_Global_2030_Outcome1_Wildlife_C,
                   data.frame(Year_Key=2030,
                              Practice_Key=practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],
                              Indicator_Type_Key=Dim_Global_2030_Outcome1_Wildlife_C$Indicator_Type_Key,
                              Practice_Outcome_Key=practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Wildlife" &
                                                                                 grepl("Habitats",practice_outcome_key_ref$practice_outcome)],
                              Indicator_Value=NA,
                              Indicator_Upper_Value=NA,
                              Indicator_Lower_Value=NA,
                              Indicator_Target=Dim_Global_2030_Outcome1_Wildlife_C$Indicator_Target))


# -- EFFECTIVENESS - ASSESSED

Dim_Global_2030_Outcome1_Wildlife_D <- 
  data.frame(Indicator_Type_Key="OUT1_WL_D",
             Indicator_Name="Percent of total protected area that has been assessed using METT",
             Indicator_Label="Extensive, Effective, Connected, & Biologically Important Habitat Protection",
             Indicator_Subcategory="Effectiveness (METT Assessed)*",
             Indicator_Unit="% protected area",
             Data_source="METT - UNEP-WCMC; WDPA",
             Indicator_Target=100,
             Indicator_Type="Outcome",
             Panel_Label="Vital Habitats Conserved",
             Display_Order=1,
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Global_2030_Outcome1_Wildlife_D <-
  import(last.file(dir.nam = dir.nam.Wildlife, nam = 'METT_area')) %>%
  transmute(Year_Key=2016,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome1_Wildlife_D$Indicator_Type_Key, length(Year_Key)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Wildlife" &
                                                                   grepl("Habitats",practice_outcome_key_ref$practice_outcome)], length(Year_Key)),
            Indicator_Value=100*(METT_meetsthreshold_Mha/4327.028912), # 2016 total cumulative PA coverage, since METT database was last updated 2016
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Indicator_Target=NA)

# Add Indicator_Latest_Year and Indicator_Latest_Value based on Fact table

Dim_Global_2030_Outcome1_Wildlife_D <- 
  Dim_Global_2030_Outcome1_Wildlife_D %>%
  mutate(Indicator_Latest_Year=max(Fact_Global_2030_Outcome1_Wildlife_D$Year_Key,na.rm=T),
         Indicator_Latest_Value=Fact_Global_2030_Outcome1_Wildlife_D$Indicator_Value[Fact_Global_2030_Outcome1_Wildlife_D$Year_Key==Indicator_Latest_Year])

# Add target value to Fact table

Fact_Global_2030_Outcome1_Wildlife_D <-
  rbind.data.frame(Fact_Global_2030_Outcome1_Wildlife_D,
                   data.frame(Year_Key=2030,
                              Practice_Key=practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],
                              Indicator_Type_Key=Dim_Global_2030_Outcome1_Wildlife_D$Indicator_Type_Key,
                              Practice_Outcome_Key=practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Wildlife" &
                                                                                 grepl("Habitats",practice_outcome_key_ref$practice_outcome)],
                              Indicator_Value=NA,
                              Indicator_Upper_Value=NA,
                              Indicator_Lower_Value=NA,
                              Indicator_Target=Dim_Global_2030_Outcome1_Wildlife_D$Indicator_Target))


# -- EFFECTIVENESS - MEETS THRESHOLD

Dim_Global_2030_Outcome1_Wildlife_E <- 
  data.frame(Indicator_Type_Key="OUT1_WL_E",
             Indicator_Name="Percent of total assessed area that meets a METT threshold of effectiveness, based on Nature's Gill et al (2017)",
             Indicator_Label="Extensive, Effective, Connected, & Biologically Important Habitat Protection",
             Indicator_Subcategory="Effectiveness (Meets Score Threshold)*",
             Indicator_Unit="% assessed area",
             Data_source="METT - UNEP-WCMC; threshold methodology from Gill et al (2017); WDPA",
             Indicator_Target=100,
             Indicator_Type="Outcome",
             Panel_Label="Vital Habitats Conserved",
             Display_Order=1,
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Global_2030_Outcome1_Wildlife_E <-
  import(last.file(dir.nam = dir.nam.Wildlife, nam = 'METT_area')) %>%
  transmute(Year_Key=2016,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome1_Wildlife_E$Indicator_Type_Key, length(Year_Key)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Wildlife" &
                                                                   grepl("Habitats",practice_outcome_key_ref$practice_outcome)], length(Year_Key)),
            Indicator_Value=METT_mettsthreshold_percent,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Indicator_Target=NA)

# Add Indicator_Latest_Year and Indicator_Latest_Value based on Fact table

Dim_Global_2030_Outcome1_Wildlife_E <- 
  Dim_Global_2030_Outcome1_Wildlife_E %>%
  mutate(Indicator_Latest_Year=max(Fact_Global_2030_Outcome1_Wildlife_E$Year_Key,na.rm=T),
         Indicator_Latest_Value=Fact_Global_2030_Outcome1_Wildlife_E$Indicator_Value[Fact_Global_2030_Outcome1_Wildlife_E$Year_Key==Indicator_Latest_Year])

# Add target value to Fact table

Fact_Global_2030_Outcome1_Wildlife_E <-
  rbind.data.frame(Fact_Global_2030_Outcome1_Wildlife_E,
                   data.frame(Year_Key=2030,
                              Practice_Key=practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],
                              Indicator_Type_Key=Dim_Global_2030_Outcome1_Wildlife_E$Indicator_Type_Key,
                              Practice_Outcome_Key=practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Wildlife" &
                                                                                 grepl("Habitats",practice_outcome_key_ref$practice_outcome)],
                              Indicator_Value=NA,
                              Indicator_Upper_Value=NA,
                              Indicator_Lower_Value=NA,
                              Indicator_Target=Dim_Global_2030_Outcome1_Wildlife_E$Indicator_Target))


# ---- 2.2 Wildlife Outcome 2 - OVEREXPLOITATION PREVENTED ----

# -- GLOBAL-SPECIFIC

Dim_Global_2030_Outcome2_Wildlife_A <- 
  data.frame(Indicator_Type_Key="OUT2_WL_A",
             Indicator_Name="FORTHCOMING: CITES Illegal Trade & Exploitation Index",
             Indicator_Label="Illegal Trade and Exploitation Index*",
             Indicator_Subcategory=NA,
             Indicator_Unit="",
             Data_source="CITES, processed by WWF/TRAFFIC",
             Indicator_Target=NA,
             Indicator_Type="Outcome",
             Panel_Label="Overexploitation Prevented",
             Display_Order=2,
             Global_Indicator="Yes",
             US_Indicator="No")

Fact_Global_2030_Outcome2_Wildlife_A <-
  data.frame(Year_Key=9999,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],length(1)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome2_Wildlife_A$Indicator_Type_Key, length(1)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Wildlife" &
                                                                   grepl("Overexploitation",practice_outcome_key_ref$practice_outcome)], length(1)),
            Indicator_Value=NA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Indicator_Target=NA)

# Add Indicator_Latest_Year and Indicator_Latest_Value based on Fact table

Dim_Global_2030_Outcome2_Wildlife_A <- 
  Dim_Global_2030_Outcome2_Wildlife_A %>%
  mutate(Indicator_Latest_Year=max(Fact_Global_2030_Outcome2_Wildlife_A$Year_Key,na.rm=T),
         Indicator_Latest_Value=Fact_Global_2030_Outcome2_Wildlife_A$Indicator_Value[Fact_Global_2030_Outcome2_Wildlife_A$Year_Key==Indicator_Latest_Year])


# -- US-SPECIFIC PROXY 

Dim_Global_2030_Outcome2_Wildlife_B <- 
  data.frame(Indicator_Type_Key="OUT2_WL_B",
             Indicator_Name="CITES Monitoring the Illegal Killing of Elephants Programme",
             Indicator_Label="Illegal Killing of African Elephants*",
             Indicator_Subcategory=NA,
             Indicator_Unit="% PIKE",
             Data_source="CITES / PIKE 'Trends in Africa' dataset",
             Indicator_Target=0,
             Indicator_Type="Outcome",
             Panel_Label="Overexploitation Prevented",
             Display_Order=2,
             Global_Indicator="No",
             US_Indicator="Yes")

Fact_Global_2030_Outcome2_Wildlife_B <-
  import(last.file(dir.nam = dir.nam.Wildlife, nam = 'PIKE_Africa')) %>%
  transmute(Year_Key=yr,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],length(yr)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome2_Wildlife_B$Indicator_Type_Key, length(yr)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Wildlife" &
                                                                   grepl("Overexploitation",practice_outcome_key_ref$practice_outcome)], length(yr)),
            Indicator_Value=`PIKE estimate`*100,
            Indicator_Upper_Value=(`PIKE estimate`*100) + (se*100),
            Indicator_Lower_Value=(`PIKE estimate`*100) - (se*100),
            Indicator_Target=NA)

# Add Indicator_Latest_Year and Indicator_Latest_Value based on Fact table

Dim_Global_2030_Outcome2_Wildlife_B <- 
  Dim_Global_2030_Outcome2_Wildlife_B %>%
  mutate(Indicator_Latest_Year=max(Fact_Global_2030_Outcome2_Wildlife_B$Year_Key,na.rm=T),
         Indicator_Latest_Value=Fact_Global_2030_Outcome2_Wildlife_B$Indicator_Value[Fact_Global_2030_Outcome2_Wildlife_B$Year_Key==Indicator_Latest_Year])

# Add target value to Fact table

Fact_Global_2030_Outcome2_Wildlife_B <-
  rbind.data.frame(Fact_Global_2030_Outcome2_Wildlife_B,
                   data.frame(Year_Key=2030,
                              Practice_Key=practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],
                              Indicator_Type_Key=Dim_Global_2030_Outcome2_Wildlife_B$Indicator_Type_Key,
                              Practice_Outcome_Key=practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Wildlife" &
                                                                                 grepl("Overexploitation",practice_outcome_key_ref$practice_outcome)],
                              Indicator_Value=NA,
                              Indicator_Upper_Value=NA,
                              Indicator_Lower_Value=NA,
                              Indicator_Target=Dim_Global_2030_Outcome2_Wildlife_B$Indicator_Target))


# ---- 2.3 Consolidated Wildlife-specific Global 2030 Outcome tables ----

Dim_Global_2030_Outcome_Wildlife <-
  rbind.data.frame(Dim_Global_2030_Outcome1_Wildlife_A,
                   Dim_Global_2030_Outcome1_Wildlife_B,
                   Dim_Global_2030_Outcome1_Wildlife_C,
                   Dim_Global_2030_Outcome1_Wildlife_D,
                   Dim_Global_2030_Outcome1_Wildlife_E,
                   Dim_Global_2030_Outcome2_Wildlife_A,
                   Dim_Global_2030_Outcome2_Wildlife_B)

Fact_Global_2030_Outcome_Wildlife <-
  rbind.data.frame(Fact_Global_2030_Outcome1_Wildlife_A,
                   Fact_Global_2030_Outcome1_Wildlife_B,
                   Fact_Global_2030_Outcome1_Wildlife_C,
                   Fact_Global_2030_Outcome1_Wildlife_D,
                   Fact_Global_2030_Outcome1_Wildlife_E,
                   Fact_Global_2030_Outcome2_Wildlife_A,
                   Fact_Global_2030_Outcome2_Wildlife_B)


#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Initiatives ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 3.1 Load data ----

dim.initiatives.wildlife <- 
  dim.initiatives %>% subset(Practice=="Wildlife") 

dim.initiative.indicators.wildlife <-
  dim.initiative.indicators %>% subset(Practice=="Wildlife")

fact.initiative.indicators.wildlife <-
  fact.initiative.indicators %>% subset(Practice=="Wildlife")

dim.initiative.milestones.wildlife <-
  dim.initiative.milestones %>% subset(Practice=="Wildlife")

pie.type.wildlife <-
  pie.type %>% subset(Practice=="Wildlife")


# ---- 3.2 Wildlife-specific Dim_Initiative ----

Dim_Initiative_Wildlife <-
  dim.initiatives.wildlife %>%
  transmute(Initiative_Key=Initiative.key,
            Initiative_Name=Initiative,
            Initiative_Status=Overall.status,
            Initiative_Status_Justification=Overall.just,
            Initiative_Goal=Initiative.statement,
            Global_Initiative=Global.initiative,
            US_Initiative=US.initiative,
            Display_Order=Display.order)


# ---- 3.3 Wildlife-specific Dim_Initiative_Indicator_Type ----

Dim_Initiative_Indicator_Wildlife <-
  left_join(dim.initiative.indicators.wildlife,
            pie.type.wildlife[,c("Initiative.indicator.key","pie.type","amount.achieved","amount.remaining","max.year.value")],
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


# ---- 3.4 Wildlife-specific Fact_Initiative_Indicators ----

Fact_Initiative_Indicator_Wildlife <-
  left_join(fact.initiative.indicators.wildlife,pie.type.wildlife[,c("Initiative.indicator.key","target.year","Target")],by="Initiative.indicator.key") %>%
  left_join(dim.initiative.indicators.wildlife[,c("Initiative.indicator.key","Units","new.key")],by="Initiative.indicator.key") %>%
  transmute(Year_Key=ifelse(!is.na(Year),Year,9999),
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],length(Year_Key)),
            Initiative_Key=Initiative.key,
            Indicator_Type_Key=new.key,
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Value_Trend=ifelse(grepl("reduction",Units,ignore.case=T)==T,-(Value),Value),
            Indicator_Target=ifelse(!is.na(Target) & Year==target.year,Target,NA),
            Target_Trend=ifelse(grepl("reduction",Units,ignore.case=T)==T,-(Indicator_Target),Indicator_Target))


# ---- 3.5 Wildlife-specific Fact_Initiative_Financials ----

Fact_Initiative_Financials_Wildlife <-
  dim.initiatives.wildlife %>%
  transmute(Date_Key=Date,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Wildlife"],length(Date_Key)),
            Initiative_Key=Initiative.key,
            Amount_Needed=Funds.needed,
            Amount_Secured=Funds.secured,
            Amount_Anticipated=Funds.anticipated,
            Amount_Remaining=Funds.needed-Funds.secured-Funds.anticipated)


# ---- 3.6 Wildlife-specific Milestone_Group_Bridge ----

Milestone_Group_Bridge_Wildlife <-
  left_join(dim.initiative.milestones.wildlife, dim.initiatives.wildlife, by=c("Initiative", "Practice")) %>%
  transmute(Milestone_Key=Milestone.key,
            Initiative_Key=Initiative.key)


# ---- 3.7 Wildlife-specific Dim_Milestone ----

Dim_Milestone_Wildlife <-
  dim.initiative.milestones.wildlife %>%
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
   Dim_Global_2030_Outcome1_Wildlife_E,
   Dim_Global_2030_Outcome2_Wildlife_A,
   Dim_Global_2030_Outcome2_Wildlife_B,
   Fact_Global_2030_Outcome1_Wildlife_A,
   Fact_Global_2030_Outcome1_Wildlife_B,
   Fact_Global_2030_Outcome1_Wildlife_C,
   Fact_Global_2030_Outcome1_Wildlife_D,
   Fact_Global_2030_Outcome1_Wildlife_E,
   Fact_Global_2030_Outcome2_Wildlife_A,
   Fact_Global_2030_Outcome2_Wildlife_B,
   dim_initiatives_Wildlife,
   dim_initiative_indicators_Wildlife,
   fact_initiative_indicators_Wildlife,
   dim_initiative_milestones_Wildlife,
   pie_type_Wildlife)
