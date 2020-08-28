# 
# code: Freshwater Practice Indicator and Initiative Tables FOR COMPS
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: August 2018
# modified: June 2020
# 
# ---- inputs ----
#  1) Freshwater-specific data tables
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
#  1) Global Context
#  2) Global 2030 Outcomes
#  3) Initiatives
# 
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: Global Context ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 1.1 Context - State ----


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
  import(last.file(dir.nam = dir.nam.Freshwater, nam = 'FW_LPI_output')) %>%
  subset(.,Year>1994 & Year<2016) %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Freshwater"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_State_FW_A$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=LPI_final,
            Indicator_Upper_Value=CI_high,
            Indicator_Lower_Value=CI_low,
            Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))

# Add Panel-specific measures

Dim_Context_State_FW_A <-
  Dim_Context_State_FW_A %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(Fact_Context_State_FW_A$Year_Key,na.rm=T),
         Panel_Max_Year=max(Fact_Context_State_FW_A$Year_Key,na.rm=T))


# ---- 1.2 Context - Threat ----

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
            Indicator_Lower_Value=NA) %>%
  mutate(Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))

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


# Add Panel-specific measures

Dim_Context_Threat_FW_A <-
  Dim_Context_Threat_FW_A %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(Fact_Context_Threat_FW_A$Year_Key,na.rm=T),
         Panel_Max_Year=max(Fact_Context_Threat_FW_A$Year_Key,na.rm=T))


# ---- 1.3 Context - Response ----

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
  import(last.file(dir.nam = dir.nam.Freshwater, nam = 'SDG_6.5.1_iwrm_dl')) %>%
  group_by(TimePeriod) %>%
  summarise(Value=length(GeoAreaName[Value>=80])/length(GeoAreaName[!is.na(Value)])) %>%
  transmute(Year_Key=TimePeriod,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Freshwater"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_Response_FW_A$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))

# Add Panel-specific measures

Dim_Context_Response_FW_A <-
  Dim_Context_Response_FW_A %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(Fact_Context_Response_FW_A$Year_Key,na.rm=T),
         Panel_Max_Year=max(Fact_Context_Response_FW_A$Year_Key,na.rm=T))


# ---- 1.4 Consolidated Freshwater-specific Global Context tables ----

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
# ---- SECTION 2: Global 2030 Outcomes ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 2.1 Freshwater Outcome 1 - HEALTHY HABITATS & SPECIES ----

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
            Indicator_Lower_Value=NA,
            Indicator_Target=NA)

# Add Indicator_Latest_Year and Indicator_Latest_Value based on Fact table

Dim_Global_2030_Outcome1_FW_A <- 
  Dim_Global_2030_Outcome1_FW_A %>%
  mutate(Indicator_Latest_Year=max(Fact_Global_2030_Outcome1_FW_A$Year_Key,na.rm=T),
         Indicator_Latest_Value=Fact_Global_2030_Outcome1_FW_A$Indicator_Value[Fact_Global_2030_Outcome1_FW_A$Year_Key==Indicator_Latest_Year])


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
  data.frame(Year_Key=2017,
             Practice_Key=practice_key_ref$id[practice_key_ref$practice_name=="Freshwater"],
             Indicator_Type_Key=Dim_Global_2030_Outcome1_FW_B$Indicator_Type_Key,
             Practice_Outcome_Key=practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Freshwater" &
                                                                    grepl("Habitats",practice_outcome_key_ref$practice_outcome)],
             Indicator_Value=16.0,
             Indicator_Upper_Value=NA,
             Indicator_Lower_Value=NA,
             Indicator_Target=NA)

# Add Indicator_Latest_Year and Indicator_Latest_Value based on Fact table

Dim_Global_2030_Outcome1_FW_B <- 
  Dim_Global_2030_Outcome1_FW_B %>%
  mutate(Indicator_Latest_Year=max(Fact_Global_2030_Outcome1_FW_B$Year_Key,na.rm=T),
         Indicator_Latest_Value=Fact_Global_2030_Outcome1_FW_B$Indicator_Value[Fact_Global_2030_Outcome1_FW_B$Year_Key==Indicator_Latest_Year])

# Add target value to Fact table

Fact_Global_2030_Outcome1_FW_B <-
  rbind.data.frame(Fact_Global_2030_Outcome1_FW_B,
                   data.frame(Year_Key=2030,
                              Practice_Key=practice_key_ref$id[practice_key_ref$practice_name=="Freshwater"],
                              Indicator_Type_Key=Dim_Global_2030_Outcome1_FW_B$Indicator_Type_Key,
                              Practice_Outcome_Key=practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Freshwater" &
                                                                                 grepl("Habitats",practice_outcome_key_ref$practice_outcome)],
                              Indicator_Value=NA,
                              Indicator_Upper_Value=NA,
                              Indicator_Lower_Value=NA,
                              Indicator_Target=Dim_Global_2030_Outcome1_FW_B$Indicator_Target))


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
  import(last.file(dir.nam = dir.nam.Freshwater, nam = 'Ramsar_sitedata_dl')) %>%
  select(.,c("Designation date","Area (ha)")) %>%
  mutate(Date=as.character(`Designation date`),
         Year=as.numeric(substr(Date, 1, 4))) %>%
  group_by(Year) %>%
  summarise(Area=sum(`Area (ha)`)) %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Freshwater"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome1_FW_C$Indicator_Type_Key, length(Year_Key)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Freshwater" &
                                                                   grepl("Habitats",practice_outcome_key_ref$practice_outcome)], length(Year_Key)),
            Indicator_Value=cumsum(Area)/1000000,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Indicator_Target=NA)

# Target value for this indicator is "double protected habitat from 2017 baseline", so we will calculate the target and 
# append the Dim table, using the Ramsar data source imported for the Fact table
# --Also, add Indicator_Latest_Year and Indicator_Latest_Value based on Fact table

Dim_Global_2030_Outcome1_FW_C <- 
  Dim_Global_2030_Outcome1_FW_C %>%
  mutate(Indicator_Target=2*Fact_Global_2030_Outcome1_FW_C$Indicator_Value[Fact_Global_2030_Outcome1_FW_C$Year_Key==2017 &
                                                                             !is.na(Fact_Global_2030_Outcome1_FW_C$Year_Key)],
         Indicator_Latest_Year=max(Fact_Global_2030_Outcome1_FW_C$Year_Key,na.rm=T),
         Indicator_Latest_Value=Fact_Global_2030_Outcome1_FW_C$Indicator_Value[Fact_Global_2030_Outcome1_FW_C$Year_Key==Indicator_Latest_Year])

# Add target value to Fact table

Fact_Global_2030_Outcome1_FW_C <-
  rbind.data.frame(Fact_Global_2030_Outcome1_FW_C,
                   data.frame(Year_Key=2030,
                              Practice_Key=practice_key_ref$id[practice_key_ref$practice_name=="Freshwater"],
                              Indicator_Type_Key=Dim_Global_2030_Outcome1_FW_C$Indicator_Type_Key,
                              Practice_Outcome_Key=practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Freshwater" &
                                                                                 grepl("Habitats",practice_outcome_key_ref$practice_outcome)],
                              Indicator_Value=NA,
                              Indicator_Upper_Value=NA,
                              Indicator_Lower_Value=NA,
                              Indicator_Target=Dim_Global_2030_Outcome1_FW_C$Indicator_Target))


# ---- 2.2 Freshwater Outcome 2 - CLEAN FLOWING RIVERS ----

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
            Indicator_Lower_Value=NA,
            Indicator_Target=NA)

# Add Indicator_Latest_Year and Indicator_Latest_Value based on Fact table

Dim_Global_2030_Outcome2_FW_A <- 
  Dim_Global_2030_Outcome2_FW_A %>%
  mutate(Indicator_Latest_Year=max(Fact_Global_2030_Outcome2_FW_A$Year_Key,na.rm=T),
         Indicator_Latest_Value=Fact_Global_2030_Outcome2_FW_A$Indicator_Value[Fact_Global_2030_Outcome2_FW_A$Year_Key==Indicator_Latest_Year])


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
             Indicator_Lower_Value=NA,
             Indicator_Target=NA)

# Add Indicator_Latest_Year and Indicator_Latest_Value based on Fact table

Dim_Global_2030_Outcome2_FW_B <- 
  Dim_Global_2030_Outcome2_FW_B %>%
  mutate(Indicator_Latest_Year=max(Fact_Global_2030_Outcome2_FW_B$Year_Key,na.rm=T),
         Indicator_Latest_Value=Fact_Global_2030_Outcome2_FW_B$Indicator_Value[Fact_Global_2030_Outcome2_FW_B$Year_Key==Indicator_Latest_Year])


# ---- 2.3 Consolidated Freshwater-specific Global 2030 Outcome tables ----

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
# ---- SECTION 3: Initiatives ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 3.1 Load data ----

dim_initiatives_FW <- 
  dim_initiatives %>% subset(goal=="Freshwater") 

dim_initiative_indicators_FW <-
  dim_initiative_indicators %>% subset(goal=="Freshwater")

fact_initiative_indicators_FW <-
  fact_initiative_indicators %>% subset(goal=="Freshwater")

dim_initiative_milestones_FW <-
  dim_initiative_milestones %>% subset(goal=="Freshwater")

pie_type_FW <-
  pie_type %>% subset(goal=="Freshwater")


# ---- 3.2 FW-specific Dim_Initiative ----

Dim_Initiative_FW <-
  dim_initiatives_FW %>%
  transmute(Initiative_Key=initiativekey,
            Initiative_Name=initiative,
            Initiative_Status=initiativestatus,
            Initiative_Status_Justification=initiativejust,
            Initiative_Goal=initiativestatement,
            Global_Initiative=globalinitiative,
            US_Initiative=usinitiative,
            Display_Order=displayorder)


# ---- 3.3 FW-specific Dim_Initiative_Indicator_Type ----

Dim_Initiative_Indicator_FW <-
  left_join(dim_initiative_indicators_FW,
            pie_type_FW[,c("indicatorkey","pie.type","amount.achieved","amount.remaining","max.year.value")],
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


# ---- 3.4 FW-specific Fact_Initiative_Indicators ----

Fact_Initiative_Indicator_FW <-
  left_join(fact_initiative_indicators_FW,pie_type_FW[,c("indicatorkey","target.year","subcattarget")],by="indicatorkey") %>%
  left_join(dim_initiative_indicators_FW[,c("indicatorkey","indicatorunits")],by="indicatorkey") %>%
  transmute(Year_Key=ifelse(!is.na(Year),Year,9999),
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Freshwater"],length(Year_Key)),
            Initiative_Key=initiativekey,
            Indicator_Type_Key=indicatorkey,
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Value_Trend=ifelse(grepl("reduction",indicatorunits,ignore.case=T)==T,-(Value),Value),
            Indicator_Target=ifelse(!is.na(subcattarget) & Year==target.year,subcattarget,NA),
            Target_Trend=ifelse(grepl("reduction",indicatorunits,ignore.case=T)==T,-(Indicator_Target),Indicator_Target))


# ---- 3.5 FW-specific Fact_Initiative_Financials ----

Fact_Initiative_Financials_FW <-
  dim_initiatives_FW %>%
  transmute(Date_Key=date,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Freshwater"],length(Date_Key)),
            Initiative_Key=initiativekey,
            Amount_Needed=fundsneeded,
            Amount_Secured=fundssecured,
            Amount_Anticipated=fundsanticipated,
            Amount_Remaining=fundsneeded-fundssecured-fundsanticipated)


# ---- 3.6 FW-specific Milestone_Group_Bridge ----

Milestone_Group_Bridge_FW <-
  left_join(dim_initiative_milestones_FW, dim_initiatives_FW, by=c("initiative", "goal")) %>%
  transmute(Milestone_Key=milestonekey,
            Initiative_Key=initiativekey)


# ---- 3.7 FW-specific Dim_Milestone ----

Dim_Milestone_FW <-
  dim_initiative_milestones_FW %>%
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
   dim_initiatives_FW,
   dim_initiative_indicators_FW,
   fact_initiative_indicators_FW,
   dim_initiative_milestones_FW,
   pie_type_FW)
