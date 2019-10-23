# 
# code: Oceans Practice Indicator and Initiative Tables FOR 2019 US DASHBOARD
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: August 2018
# modified: July 2019
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

GBR <-
  read.csv('2_Oct2019_US/2_FlatDataFiles/ConsDB_Input_2019/AIMS_GBR_2019_summary_report_0723.csv') %>%
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
  read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/GlobalMangroveWatch_fromDom_2018_0827.csv') %>%
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


# ---- 2.2 Context - Threat ----

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
  read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/FAO_percent_overfished_2018_0827.csv') %>%
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


# ---- 2.3 Context - Response ----

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
  read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/WDPA_time.csv') %>%
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


# ---- 2.4 Consolidated Oceans-specific Global Context tables ----

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
# ---- SECTION 3: Global 2030 Outcomes ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 3.1 Oceans Outcome 1 - HEALTHY & PRODUCTIVE ECOSYSTEMS ----

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
  read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/METT/METT_MPA.csv') %>%
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
  read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/METT/METT_MPA.csv') %>%
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


# ---- 3.2 Oceans Outcome 2 - SUSTAINABLE FISHERIES ----

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


# ---- 3.3 Consolidated Oceans-specific Global 2030 Outcome tables ----

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
# ---- SECTION 4: Initiatives ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 4.1 Load data ----

dim.initiatives.oceans <- 
  dim.initiatives %>% subset(Practice=="Oceans") 

dim.initiative.indicators.oceans <-
  dim.initiative.indicators %>% subset(Practice=="Oceans")

fact.initiative.indicators.oceans <-
  fact.initiative.indicators %>% subset(Practice=="Oceans")

dim.initiative.milestones.oceans <-
  dim.initiative.milestones %>% subset(Practice=="Oceans")

pie.type.oceans <-
  pie.type %>% subset(Practice=="Oceans")


# ---- 4.2 Oceans-specific Dim_Initiative ----

Dim_Initiative_Oceans <-
  dim.initiatives.oceans %>%
  transmute(Initiative_Key=Initiative.key,
            Initiative_Name=Initiative,
            Initiative_Status=Overall.status,
            Initiative_Status_Justification=Overall.just,
            Initiative_Goal=Initiative.statement,
            Global_Initiative=Global.initiative,
            US_Initiative=US.initiative,
            Display_Order=Display.order)


# ---- 4.3 Oceans-specific Dim_Initiative_Indicator_Type ----

Dim_Initiative_Indicator_Oceans <-
  left_join(dim.initiative.indicators.oceans,
            pie.type.oceans[,c("Initiative.indicator.key","pie.type","amount.achieved","amount.remaining","max.year.value")],
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


# ---- 4.4 Oceans-specific Fact_Initiative_Indicators ----

Fact_Initiative_Indicator_Oceans <-
  left_join(fact.initiative.indicators.oceans,pie.type.oceans[,c("Initiative.indicator.key","target.year","Target")],by="Initiative.indicator.key") %>%
  left_join(dim.initiative.indicators.oceans[,c("Initiative.indicator.key","Units","new.key")],by="Initiative.indicator.key") %>%
  transmute(Year_Key=ifelse(!is.na(Year),Year,9999),
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Oceans"],length(Year_Key)),
            Initiative_Key=Initiative.key,
            Indicator_Type_Key=new.key,
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Value_Trend=ifelse(grepl("reduction",Units,ignore.case=T)==T,-(Value),Value),
            Indicator_Target=ifelse(!is.na(Target) & Year==target.year,Target,NA),
            Target_Trend=ifelse(grepl("reduction",Units,ignore.case=T)==T,-(Indicator_Target),Indicator_Target))



# ---- 4.5 Oceans-specific Fact_Initiative_Financials ----

Fact_Initiative_Financials_Oceans <-
  dim.initiatives.oceans %>%
  transmute(Date_Key=Date,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Oceans"],length(Date_Key)),
            Initiative_Key=Initiative.key,
            Amount_Needed=Funds.needed,
            Amount_Secured=Funds.secured,
            Amount_Anticipated=Funds.anticipated,
            Amount_Remaining=Funds.needed-Funds.secured-Funds.anticipated)


# ---- 4.6 Oceans-specific Milestone_Group_Bridge ----

Milestone_Group_Bridge_Oceans <-
  left_join(dim.initiative.milestones.oceans, dim.initiatives.oceans, by=c("Initiative", "Practice")) %>%
  transmute(Milestone_Key=Milestone.key,
            Initiative_Key=Initiative.key)


# ---- 4.7 Oceans-specific Dim_Milestone ----

Dim_Milestone_Oceans <-
  dim.initiative.milestones.oceans %>%
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
   dim.initiatives.oceans,
   dim.initiative.indicators.oceans,
   fact.initiative.indicators.oceans,
   dim.initiative.milestones.oceans,
   pie.type.oceans)
