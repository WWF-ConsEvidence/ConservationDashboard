# 
# code: Oceans Practice Indicator and Initiative Tables FOR COMPS
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: August 2018
# modified: June 2020
# 
# ---- inputs ----
#  1) Oceans-specific data tables
# 
# ---- outputs ----
#  1) Oceans-specific back-end tables -- ready to be consolidated with other Practices, to go into back-end:
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

GBR <-
  import(last.file(dir.nam = dir.nam.Oceans, nam = 'AIMS_GBR_summaryreport_dl')) %>%
  mutate(Mean_coral_fixed=NA)

for(i in 1:length(GBR$Mean_coral_cover)) {
  GBR$Mean_coral_fixed[i] <- ifelse(is.na(GBR$Mean_coral_cover[i]) &
                                      GBR$Region[i+1]==GBR$Region[i],
                                    mean(c(GBR$Mean_coral_cover[GBR$Year==GBR$Year[i]-1 &
                                                                  GBR$Region==GBR$Region[i]],
                                           GBR$Mean_coral_cover[GBR$Year==GBR$Year[i]+1 &
                                                                  GBR$Region==GBR$Region[i]])),
                                    ifelse(is.na(GBR$Mean_coral_cover[i]) &
                                             GBR$Region[i+1]!=GBR$Region[i],
                                           GBR$Mean_coral_cover[GBR$Year==GBR$Year[i]-1 &
                                                                  GBR$Region==GBR$Region[i]],
                                           GBR$Mean_coral_cover[i]))
}

# -- COASTAL ECOSYSTEMS - MANGROVE AREA

Dim_Context_State_Oceans_A <- 
  data.frame(Indicator_Type_Key="GCS_OC_A",
             Indicator_Name="Global Mangrove coverage (M ha)",
             Indicator_Label="Total Mangrove Area & Great Barrier Reef (GBR) Coral Cover*",
             Panel_Label="Coastal Ecosystems",
             Panel="State",
             Indicator_Subcategory="Mangrove Extent",
             Indicator_Unit="M ha",
             Data_Source="Global Mangrove Alliance",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_State_Oceans_A <-
  import(last.file(dir.nam = dir.nam.Oceans, nam = 'GlobalMangroveWatch')) %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Oceans"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_State_Oceans_A$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Area,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))

# -- COASTAL ECOSYSTEMS - GBR CORAL COVER

Dim_Context_State_Oceans_B <- 
  data.frame(Indicator_Type_Key="GCS_OC_B",
             Indicator_Name="Percent coral cover in the Great Barrier Reef",
             Indicator_Label="Total Mangrove Area & Great Barrier Reef (GBR) Coral Cover*",
             Panel_Label="Coastal Ecosystems",
             Panel="State",
             Indicator_Subcategory="GBR Coral Cover",
             Indicator_Unit="%",
             Data_Source="AIMS Long-term Reef Monitoring Program - Annual Summary Report on coral reef condition for 2017-18",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_State_Oceans_B <-
  GBR[GBR$Year<2018,] %>%
  group_by(Year) %>%
  summarise(Mean=mean(Mean_coral_fixed),
            Std.dev=sd(Mean_coral_fixed)) %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Oceans"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_State_Oceans_B$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Mean,
            Indicator_Upper_Value=Mean+Std.dev,
            Indicator_Lower_Value=Mean-Std.dev,
            Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))

# Add Panel-specific measures

Dim_Context_State_Oceans_A <-
  Dim_Context_State_Oceans_A %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(unique(rbind(Fact_Context_State_Oceans_A$Year_Key,Fact_Context_State_Oceans_B$Year_Key))),
         Panel_Max_Year=max(unique(rbind(Fact_Context_State_Oceans_A$Year_Key,Fact_Context_State_Oceans_B$Year_Key))))

Dim_Context_State_Oceans_B <-
  Dim_Context_State_Oceans_B %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(unique(rbind(Fact_Context_State_Oceans_A$Year_Key,Fact_Context_State_Oceans_B$Year_Key))),
         Panel_Max_Year=max(unique(rbind(Fact_Context_State_Oceans_A$Year_Key,Fact_Context_State_Oceans_B$Year_Key))))


# ---- 1.2 Context - Threat ----

# -- THREATS TO MARINE LIFE - OVERFISHING

Dim_Context_Threat_Oceans_A <- 
  data.frame(Indicator_Type_Key="GCT_OC_A",
             Indicator_Name="Percent of fish stocks overfished",
             Indicator_Label="Overfishing & Coral Reef Loss*",
             Panel_Label="Threats to Marine Life",
             Panel="Threat",
             Indicator_Subcategory="Overfished Stocks",
             Indicator_Unit="%",
             Data_Source="FAO Fish Stock Assessment; FAO 2018 state of the world fisheries and aquaculture",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_Threat_Oceans_A <-
  import(last.file(dir.nam = dir.nam.Oceans, nam = 'FAO_percent_overfished')) %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Oceans"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_Threat_Oceans_A$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Percent.overfished,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))

# -- THREATS TO MARINE LIFE - CORAL REEF LOSS

Dim_Context_Threat_Oceans_B <- 
  data.frame(Indicator_Type_Key="GCT_OC_B",
             Indicator_Name="FORTHCOMING: Coral reefs lost to bleaching",
             Indicator_Label="Overfishing & Coral Reef Loss*",
             Panel_Label="Threats to Marine Life",
             Panel="Threat",
             Indicator_Subcategory="Bleached Reefs",
             Indicator_Unit="M ha per year",
             Data_Source="FORTHCOMING: internal (WWF) estimates of annual bleaching and habitat loss -- details being worked out",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_Threat_Oceans_B <-
  data.frame(Year_Key=9999,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Oceans"],length(1)),
            Indicator_Type_Key=rep(Dim_Context_Threat_Oceans_B$Indicator_Type_Key,length(1)),
            Indicator_Value=NA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA) %>%
  mutate(Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))

# Add Panel-specific measures

Dim_Context_Threat_Oceans_A <-
  Dim_Context_Threat_Oceans_A %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(Fact_Context_Threat_Oceans_A$Year_Key),
         Panel_Max_Year=max(Fact_Context_Threat_Oceans_A$Year_Key))

Dim_Context_Threat_Oceans_B <-
  Dim_Context_Threat_Oceans_B %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(Fact_Context_Threat_Oceans_A$Year_Key),
         Panel_Max_Year=max(Fact_Context_Threat_Oceans_A$Year_Key))


# ---- 1.3 Context - Response ----

# -- MARINE PROTECTION - GLOBAL MPA COVERAGE

Dim_Context_Response_Oceans_A <- 
  data.frame(Indicator_Type_Key="GCR_OC_A",
             Indicator_Name="Global Marine Protected Area coverage (M ha)",
             Indicator_Label="Protected & Pledged*",
             Panel_Label="Marine Protection",
             Panel="Response",
             Indicator_Subcategory="Protected",
             Indicator_Unit="M ha",
             Data_Source="WDPA",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_Response_Oceans_A <- 
  import(last.file(dir.nam = dir.nam.Forest, nam = "WDPA_time")) %>%
  filter(YEAR>1989) %>%
  transmute(Year_Key=YEAR,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Oceans"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_Response_Oceans_A$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=EEZ_AREA_MHA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))

# -- MARINE PROTECTION - AREA COMMITTED

Dim_Context_Response_Oceans_B <- 
  data.frame(Indicator_Type_Key="GCR_OC_B",
             Indicator_Name="Marine area committed to being protected (M ha)",
             Indicator_Label="Protected & Pledged*",
             Panel_Label="Marine Protection",
             Panel="Response",
             Indicator_Subcategory="Pledged",
             Indicator_Unit="M ha",
             Data_Source="MPAtlas",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_Response_Oceans_B <-
  data.frame(Year_Key=9999,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Oceans"],length(1)),
            Indicator_Type_Key=rep(Dim_Context_Response_Oceans_B$Indicator_Type_Key,length(1)),
            Indicator_Value=NA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA) %>%
  mutate(Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))

# Add Panel-specific measures

Dim_Context_Response_Oceans_A <-
  Dim_Context_Response_Oceans_A %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(Fact_Context_Response_Oceans_A$Year_Key),
         Panel_Max_Year=max(Fact_Context_Response_Oceans_A$Year_Key))

Dim_Context_Response_Oceans_B <-
  Dim_Context_Response_Oceans_B %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(Fact_Context_Response_Oceans_A$Year_Key),
         Panel_Max_Year=max(Fact_Context_Response_Oceans_A$Year_Key))


# ---- 1.4 Consolidated Oceans-specific Global Context tables ----

Dim_Context_Oceans <- 
  rbind.data.frame(Dim_Context_State_Oceans_A,
                   Dim_Context_State_Oceans_B,
                   Dim_Context_Threat_Oceans_A,
                   Dim_Context_Threat_Oceans_B,
                   Dim_Context_Response_Oceans_A,
                   Dim_Context_Response_Oceans_B)

Fact_Context_Oceans <-
  rbind.data.frame(Fact_Context_State_Oceans_A,
                   Fact_Context_State_Oceans_B,
                   Fact_Context_Threat_Oceans_A,
                   Fact_Context_Threat_Oceans_B,
                   Fact_Context_Response_Oceans_A,
                   Fact_Context_Response_Oceans_B)


#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Global 2030 Outcomes ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 2.1 Oceans Outcome 1 - HEALTHY & PRODUCTIVE ECOSYSTEMS ----

# EFFECTIVELY MANAGED - ASSESSED

Dim_Global_2030_Outcome1_Oceans_A <- 
  data.frame(Indicator_Type_Key="OUT1_OC_A",
             Indicator_Name="Management effectiveness, as proportion of MPA area that has been assessed using a METT",
             Indicator_Label="Effectively Managed (METT Assessed)*",
             Indicator_Subcategory=NA,
             Indicator_Unit="% of total MPA area",
             Data_source="METT & WDPA",
             Indicator_Target=100,
             Indicator_Type="Outcome",
             Panel_Label="Healthy & Productive Ecosystems",
             Display_Order=1,
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Global_2030_Outcome1_Oceans_A <-
  import(last.file(dir.nam = dir.nam.Oceans, nam = 'METT_MPA')) %>%
  transmute(Year_Key=2018,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Oceans"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome1_Oceans_A$Indicator_Type_Key, length(Year_Key)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Oceans" &
                                                                   grepl("Ecosystems",practice_outcome_key_ref$practice_outcome)], length(Year_Key)),
            Indicator_Value=MPA_METT_percent,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Indicator_Target=NA)

# Add Indicator_Latest_Year and Indicator_Latest_Value based on Fact table

Dim_Global_2030_Outcome1_Oceans_A <- 
  Dim_Global_2030_Outcome1_Oceans_A %>%
  mutate(Indicator_Latest_Year=max(Fact_Global_2030_Outcome1_Oceans_A$Year_Key,na.rm=T),
         Indicator_Latest_Value=Fact_Global_2030_Outcome1_Oceans_A$Indicator_Value[Fact_Global_2030_Outcome1_Oceans_A$Year_Key==Indicator_Latest_Year])

# Add target value to Fact table

Fact_Global_2030_Outcome1_Oceans_A <- 
  rbind.data.frame(Fact_Global_2030_Outcome1_Oceans_A,
                   data.frame(Year_Key=2030,
                              Practice_Key=practice_key_ref$id[practice_key_ref$practice_name=="Oceans"],
                              Indicator_Type_Key=Dim_Global_2030_Outcome1_Oceans_A$Indicator_Type_Key,
                              Practice_Outcome_Key=practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Oceans" &
                                                                                 grepl("Ecosystems",practice_outcome_key_ref$practice_outcome)],
                              Indicator_Value=NA,
                              Indicator_Upper_Value=NA,
                              Indicator_Lower_Value=NA,
                              Indicator_Target=Dim_Global_2030_Outcome1_Oceans_A$Indicator_Target))

# EFFECTIVELY MANAGED - MEETS THRESHOLD

Dim_Global_2030_Outcome1_Oceans_B <- 
  data.frame(Indicator_Type_Key="OUT1_OC_B",
             Indicator_Name="Management effectiveness, as proportion of assessed area that meets METT threshold of effectiveness",
             Indicator_Label="Effectively Managed (Meets Score Threshold)*",
             Indicator_Subcategory=NA,
             Indicator_Unit="% of assessed area",
             Data_source="METT & WDPA",
             Indicator_Target=100,
             Indicator_Type="Outcome",
             Panel_Label="Healthy & Productive Ecosystems",
             Display_Order=1,
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Global_2030_Outcome1_Oceans_B <-
  import(last.file(dir.nam = dir.nam.Oceans, nam = 'METT_MPA')) %>%
  transmute(Year_Key=2018,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Oceans"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome1_Oceans_B$Indicator_Type_Key, length(Year_Key)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Oceans" &
                                                                   grepl("Ecosystems",practice_outcome_key_ref$practice_outcome)], length(Year_Key)),
            Indicator_Value=MPA_threshold_percent,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Indicator_Target=NA)

# Add Indicator_Latest_Year and Indicator_Latest_Value based on Fact table

Dim_Global_2030_Outcome1_Oceans_B <- 
  Dim_Global_2030_Outcome1_Oceans_B %>%
  mutate(Indicator_Latest_Year=max(Fact_Global_2030_Outcome1_Oceans_B$Year_Key,na.rm=T),
         Indicator_Latest_Value=Fact_Global_2030_Outcome1_Oceans_B$Indicator_Value[Fact_Global_2030_Outcome1_Oceans_B$Year_Key==Indicator_Latest_Year])

# Add target value to Fact table

Fact_Global_2030_Outcome1_Oceans_B <-
  rbind.data.frame(Fact_Global_2030_Outcome1_Oceans_B,
                   data.frame(Year_Key=2030,
                              Practice_Key=practice_key_ref$id[practice_key_ref$practice_name=="Oceans"],
                              Indicator_Type_Key=Dim_Global_2030_Outcome1_Oceans_B$Indicator_Type_Key,
                              Practice_Outcome_Key=practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Oceans" &
                                                                                 grepl("Ecosystems",practice_outcome_key_ref$practice_outcome)],
                              Indicator_Value=NA,
                              Indicator_Upper_Value=NA,
                              Indicator_Lower_Value=NA,
                              Indicator_Target=Dim_Global_2030_Outcome1_Oceans_B$Indicator_Target))


# ---- 2.2 Oceans Outcome 2 - SUSTAINABLE FISHERIES ----

Dim_Global_2030_Outcome2_Oceans_A <- 
  data.frame(Indicator_Type_Key="OUT2_OC_A",
             Indicator_Name="FORTHCOMING",
             Indicator_Label=NA,
             Indicator_Subcategory="(under development by Oceans Practice)",
             Indicator_Unit=NA,
             Data_source="",
             Indicator_Target=NA,
             Indicator_Type="Outcome",
             Panel_Label="Sustainable Fisheries",
             Display_Order=2,
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Global_2030_Outcome2_Oceans_A <-
  data.frame(Year_Key=9999,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Oceans"],length(1)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome2_Oceans_A$Indicator_Type_Key, length(1)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Oceans" &
                                                                   grepl("Fisheries",practice_outcome_key_ref$practice_outcome)], length(1)),
            Indicator_Value=NA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Indicator_Target=NA)

# Add Indicator_Latest_Year and Indicator_Latest_Value based on Fact table

Dim_Global_2030_Outcome2_Oceans_A <- 
  Dim_Global_2030_Outcome2_Oceans_A %>%
  mutate(Indicator_Latest_Year=max(Fact_Global_2030_Outcome2_Oceans_A$Year_Key,na.rm=T),
         Indicator_Latest_Value=Fact_Global_2030_Outcome2_Oceans_A$Indicator_Value[Fact_Global_2030_Outcome2_Oceans_A$Year_Key==Indicator_Latest_Year])


# ---- 2.3 Consolidated Oceans-specific Global 2030 Outcome tables ----

Dim_Global_2030_Outcome_Oceans <-
  rbind.data.frame(Dim_Global_2030_Outcome1_Oceans_A,
                   Dim_Global_2030_Outcome1_Oceans_B,
                   Dim_Global_2030_Outcome2_Oceans_A)

Fact_Global_2030_Outcome_Oceans <-
  rbind.data.frame(Fact_Global_2030_Outcome1_Oceans_A,
                   Fact_Global_2030_Outcome1_Oceans_B,
                   Fact_Global_2030_Outcome2_Oceans_A)

#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Initiatives ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 3.1 Load data ----

dim_initiatives_Oceans <- 
  dim_initiatives %>% subset(goal=="Oceans") 

dim_initiative_indicators_Oceans <-
  dim_initiative_indicators %>% subset(goal=="Oceans")

fact_initiative_indicators_Oceans <-
  fact_initiative_indicators %>% subset(goal=="Oceans")

dim_initiative_milestones_Oceans <-
  dim_initiative_milestones %>% subset(goal=="Oceans")

pie_type_Oceans <-
  pie_type %>% subset(goal=="Oceans")


# ---- 3.2 Ocean-specific Dim_Initiative ----

Dim_Initiative_Oceans <-
  dim_initiatives_Oceans %>%
  transmute(Initiative_Key=initiativekey,
            Initiative_Name=initiative,
            Initiative_Status=initiativestatus,
            Initiative_Status_Justification=initiativejust,
            Initiative_Goal=initiativestatement,
            Global_Initiative=globalinitiative,
            US_Initiative=usinitiative,
            Display_Order=displayorder)


# ---- 3.3 Ocean-specific Dim_Initiative_Indicator_Type ----

Dim_Initiative_Indicator_Oceans <-
  left_join(dim_initiative_indicators_Oceans,
            pie_type_Oceans[,c("indicatorkey","pie.type","amount.achieved","amount.remaining","max.year.value")],
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


# ---- 3.4 Ocean-specific Fact_Initiative_Indicators ----

Fact_Initiative_Indicator_Oceans <-
  left_join(fact_initiative_indicators_Oceans,pie_type_Oceans[,c("indicatorkey","target.year","subcattarget")],by="indicatorkey") %>%
  left_join(dim_initiative_indicators_Oceans[,c("indicatorkey","indicatorunits")],by="indicatorkey") %>%
  transmute(Year_Key=ifelse(!is.na(Year),Year,9999),
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Oceans"],length(Year_Key)),
            Initiative_Key=initiativekey,
            Indicator_Type_Key=indicatorkey,
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Value_Trend=ifelse(grepl("reduction",indicatorunits,ignore.case=T)==T,-(Value),Value),
            Indicator_Target=ifelse(!is.na(subcattarget) & Year==target.year,subcattarget,NA),
            Target_Trend=ifelse(grepl("reduction",indicatorunits,ignore.case=T)==T,-(Indicator_Target),Indicator_Target))


# ---- 3.5 Ocean-specific Fact_Initiative_Financials ----

Fact_Initiative_Financials_Oceans <-
  dim_initiatives_Oceans %>%
  transmute(Date_Key=date,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Oceans"],length(Date_Key)),
            Initiative_Key=initiativekey,
            Amount_Needed=fundsneeded,
            Amount_Secured=fundssecured,
            Amount_Anticipated=fundsanticipated,
            Amount_Remaining=fundsneeded-fundssecured-fundsanticipated)


# ---- 3.6 Ocean-specific Milestone_Group_Bridge ----

Milestone_Group_Bridge_Oceans <-
  left_join(dim_initiative_milestones_Oceans, dim_initiatives_Oceans, by=c("initiative", "goal")) %>%
  transmute(Milestone_Key=milestonekey,
            Initiative_Key=initiativekey)


# ---- 3.7 Ocean-specific Dim_Milestone ----

Dim_Milestone_Oceans <-
  dim_initiative_milestones_Oceans %>%
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

rm(GBR,
   Dim_Context_State_Oceans_A,
   Dim_Context_State_Oceans_B,
   Dim_Context_Threat_Oceans_A,
   Dim_Context_Threat_Oceans_B,
   Dim_Context_Response_Oceans_A,
   Dim_Context_Response_Oceans_B,
   Fact_Context_State_Oceans_A,
   Fact_Context_State_Oceans_B,
   Fact_Context_Threat_Oceans_A,
   Fact_Context_Threat_Oceans_B,
   Fact_Context_Response_Oceans_A,
   Fact_Context_Response_Oceans_B,
   Dim_Global_2030_Outcome1_Oceans_A,
   Dim_Global_2030_Outcome1_Oceans_B,
   Dim_Global_2030_Outcome2_Oceans_A,
   Fact_Global_2030_Outcome1_Oceans_A,
   Fact_Global_2030_Outcome1_Oceans_B,
   Fact_Global_2030_Outcome2_Oceans_A,
   dim_initiatives_Oceans,
   dim_initiative_indicators_Oceans,
   fact_initiative_indicators_Oceans,
   dim_initiative_milestones_Oceans,
   pie_type_Oceans)
