# 
# code: Food Practice Indicator and Initiative Tables FOR COMPS
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: August 2018
# modified: June 2020
# 
# ---- inputs ----
#  1) Food-specific data tables
# 
# ---- outputs ----
#  1) Food-specific back-end tables -- ready to be consolidated with other Practices, to go into back-end:
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

FAOSTAT_FoodBalance <- import(last.file(dir.nam = dir.nam.Food, nam = 'FAOSTAT_FoodBalanceSheets_dl'))

# -- FOOD SUPPLY - DIETARY ENERGY

Dim_Context_State_Food_A <- 
  data.frame(Indicator_Type_Key="GCS_FD_A",
             Indicator_Name="Global dietary energy supply (kcal per capita per day)",
             Indicator_Label="Daily Per Capita Supply of Dietary Energy, Protein, & Fat",
             Panel_Label="Food Supply",
             Panel="State",
             Indicator_Subcategory="Dietary Energy",
             Indicator_Unit="Kcal",
             Data_Source="FAOSTAT, FAO Food Balance Sheets",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_State_Food_A <-
  FAOSTAT_FoodBalance %>%
  subset(.,grepl("Food supply",Element,ignore.case=T)) %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Food"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_State_Food_A$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))

# -- FOOD SUPPLY - PROTEIN

Dim_Context_State_Food_B <- 
  data.frame(Indicator_Type_Key="GCS_FD_B",
             Indicator_Name="Global dietary protein supply (g per capita per day)",
             Indicator_Label="Daily Per Capita Supply of Dietary Energy, Protein, & Fat",
             Panel_Label="Food Supply",
             Panel="State",
             Indicator_Subcategory="Protein",
             Indicator_Unit="Grams",
             Data_Source="FAOSTAT, FAO Food Balance Sheets",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_State_Food_B <-
  FAOSTAT_FoodBalance %>%
  subset(.,grepl("Protein supply",Element,ignore.case=T)) %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Food"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_State_Food_B$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))

# -- FOOD SUPPLY - FAT

Dim_Context_State_Food_C <- 
  data.frame(Indicator_Type_Key="GCS_FD_C",
             Indicator_Name="Global dietary fat supply (g per capita per day)",
             Indicator_Label="Daily Per Capita Supply of Dietary Energy, Protein, & Fat",
             Panel_Label="Food Supply",
             Panel="State",
             Indicator_Subcategory="Fat",
             Indicator_Unit="Grams",
             Data_Source="FAOSTAT, FAO Food Balance Sheets",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_State_Food_C <-
  FAOSTAT_FoodBalance %>%
  subset(.,grepl("Fat supply",Element,ignore.case=T)) %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Food"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_State_Food_C$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))

# Add Panel-specific measures

Dim_Context_State_Food_A <-
  Dim_Context_State_Food_A %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(unique(rbind(Fact_Context_State_Food_A$Year_Key,Fact_Context_State_Food_B$Year_Key,Fact_Context_State_Food_C$Year_Key))),
         Panel_Max_Year=max(unique(rbind(Fact_Context_State_Food_A$Year_Key,Fact_Context_State_Food_B$Year_Key,Fact_Context_State_Food_C$Year_Key))))

Dim_Context_State_Food_B <-
  Dim_Context_State_Food_B %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(unique(rbind(Fact_Context_State_Food_A$Year_Key,Fact_Context_State_Food_B$Year_Key,Fact_Context_State_Food_C$Year_Key))),
         Panel_Max_Year=max(unique(rbind(Fact_Context_State_Food_A$Year_Key,Fact_Context_State_Food_B$Year_Key,Fact_Context_State_Food_C$Year_Key))))

Dim_Context_State_Food_C <-
  Dim_Context_State_Food_C %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(unique(rbind(Fact_Context_State_Food_A$Year_Key,Fact_Context_State_Food_B$Year_Key,Fact_Context_State_Food_C$Year_Key))),
         Panel_Max_Year=max(unique(rbind(Fact_Context_State_Food_A$Year_Key,Fact_Context_State_Food_B$Year_Key,Fact_Context_State_Food_C$Year_Key))))


# ---- 1.2 Context - Threat ----

OECDStat_LandUse <- import(last.file(dir.nam = dir.nam.Food, nam = 'LandUse_OECDStat_dl'))

# -- LAND USE - CROPLAND 

Dim_Context_Threat_Food_A <- 
  data.frame(Indicator_Type_Key="GCT_FD_A",
             Indicator_Name="Land use type (percentage of total land area)",
             Indicator_Label="Type of Use",
             Panel_Label="Land Use",
             Panel="Threat",
             Indicator_Subcategory="Arable and Cropland",
             Indicator_Unit="% of total land",
             Data_Source="OECDStat, from FAO",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_Threat_Food_A <-
  OECDStat_LandUse %>%
  subset(.,Country=="World" & 
           Year>1989 & 
           grepl("arable", Variable, ignore.case=T) &
           Unit=="Percentage") %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Food"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_Threat_Food_A$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))

# -- LAND USE - MEADOWS & PASTURE

Dim_Context_Threat_Food_B <- 
  data.frame(Indicator_Type_Key="GCT_FD_B",
             Indicator_Name="Land use (percentage of total land area)",
             Indicator_Label="Type of Use",
             Panel_Label="Land Use",
             Panel="Threat",
             Indicator_Subcategory="Meadows and Pastures",
             Indicator_Unit="% of total land",
             Data_Source="OECDStat, from FAO",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_Threat_Food_B <-
  OECDStat_LandUse %>%
  subset(.,Country=="World" & 
           Year>1989 & 
           grepl("meadow", Variable, ignore.case=T) &
           Unit=="Percentage") %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Food"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_Threat_Food_B$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))

# -- LAND USE - FOREST

Dim_Context_Threat_Food_C <- 
  data.frame(Indicator_Type_Key="GCT_FD_C",
             Indicator_Name="Land use (percentage of total land area)",
             Indicator_Label="Type of Use",
             Panel_Label="Land Use",
             Panel="Threat",
             Indicator_Subcategory="Forest",
             Indicator_Unit="% of total land",
             Data_Source="OECDStat, from FAO",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_Threat_Food_C <-
  OECDStat_LandUse %>%
  subset(.,Country=="World" & 
           Year>1989 & 
           grepl("forest", Variable, ignore.case=T) &
           Unit=="Percentage") %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Food"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_Threat_Food_C$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))

# -- LAND USE - OTHER

Dim_Context_Threat_Food_D <- 
  data.frame(Indicator_Type_Key="GCT_FD_D",
             Indicator_Name="Land use (percentage of total land area)",
             Indicator_Label="Type of Use",
             Panel_Label="Land Use",
             Panel="Threat",
             Indicator_Subcategory="Other",
             Indicator_Unit="% of total land",
             Data_Source="OECDStat, from FAO",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_Threat_Food_D <-
  OECDStat_LandUse %>%
  subset(.,Country=="World" & 
           Year>1989 & 
           grepl("other", Variable, ignore.case=T) &
           Unit=="Percentage") %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Food"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_Threat_Food_D$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))

# Add Panel-specific measures

Dim_Context_Threat_Food_A <-
  Dim_Context_Threat_Food_A %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(unique(rbind(Fact_Context_Threat_Food_A$Year_Key,Fact_Context_Threat_Food_B$Year_Key,Fact_Context_Threat_Food_C$Year_Key,Fact_Context_Threat_Food_D$Year_Key))),
         Panel_Max_Year=max(unique(rbind(Fact_Context_Threat_Food_A$Year_Key,Fact_Context_Threat_Food_B$Year_Key,Fact_Context_Threat_Food_C$Year_Key,Fact_Context_Threat_Food_D$Year_Key))))

Dim_Context_Threat_Food_B <-
  Dim_Context_Threat_Food_B %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(unique(rbind(Fact_Context_Threat_Food_A$Year_Key,Fact_Context_Threat_Food_B$Year_Key,Fact_Context_Threat_Food_C$Year_Key,Fact_Context_Threat_Food_D$Year_Key))),
         Panel_Max_Year=max(unique(rbind(Fact_Context_Threat_Food_A$Year_Key,Fact_Context_Threat_Food_B$Year_Key,Fact_Context_Threat_Food_C$Year_Key,Fact_Context_Threat_Food_D$Year_Key))))

Dim_Context_Threat_Food_C <-
  Dim_Context_Threat_Food_C %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(unique(rbind(Fact_Context_Threat_Food_A$Year_Key,Fact_Context_Threat_Food_B$Year_Key,Fact_Context_Threat_Food_C$Year_Key,Fact_Context_Threat_Food_D$Year_Key))),
         Panel_Max_Year=max(unique(rbind(Fact_Context_Threat_Food_A$Year_Key,Fact_Context_Threat_Food_B$Year_Key,Fact_Context_Threat_Food_C$Year_Key,Fact_Context_Threat_Food_D$Year_Key))))

Dim_Context_Threat_Food_D <-
  Dim_Context_Threat_Food_D %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(unique(rbind(Fact_Context_Threat_Food_A$Year_Key,Fact_Context_Threat_Food_B$Year_Key,Fact_Context_Threat_Food_C$Year_Key,Fact_Context_Threat_Food_D$Year_Key))),
         Panel_Max_Year=max(unique(rbind(Fact_Context_Threat_Food_A$Year_Key,Fact_Context_Threat_Food_B$Year_Key,Fact_Context_Threat_Food_C$Year_Key,Fact_Context_Threat_Food_D$Year_Key))))


# ---- 1.3 Context - Response ----

CAIT.emissions <- import(last.file(dir.nam = dir.nam.CEP, nam = 'CAIT_country_GHG'))
FAOSTAT_worldpop <- import(last.file(dir.nam = dir.nam.Food, nam = 'FAOSTAT_world_pop_dl'))

# -- EMISSIONS & EFFICIENCY - EMISSIONS

Dim_Context_Response_Food_A <- 
  data.frame(Indicator_Type_Key="GCR_FD_A",
             Indicator_Name="Global GHG emissions from the agricultural sector",
             Indicator_Label="Agricultural GHG Emissions & Units of Food Produced per Tonne of Emissions",
             Panel_Label="Emissions & Efficiency",
             Panel="Response",
             Indicator_Subcategory="Agricultural GHG Emissions",
             Indicator_Unit="Gt CO2e",
             Data_Source="CAIT Climate Data Explorer. 2017. Washington, DC: World Resources Institute",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_Response_Food_A <-
  CAIT.emissions %>%
  subset(.,Country=="World",select=c(1,2,14)) %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Food"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_Response_Food_A$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=`Agriculture (MtCO2e)`/1000,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))

# -- EMISSIONS & EFFICIENCY - EFFICIENCY

Dim_Context_Response_Food_B <- 
  data.frame(Indicator_Type_Key="GCR_FD_B",
             Indicator_Name="Millions of kcals produced per ton of GHGs emitted annually",
             Indicator_Label="Agricultural GHG Emissions & Units of Food Produced per Tonne of Emissions",
             Panel_Label="Emissions & Efficiency",
             Panel="Response",
             Indicator_Subcategory="Efficiency",
             Indicator_Unit="M kcal per tonne CO2e",
             Data_Source="CAIT Climate Data Explorer (2017); FAO Food Balance Sheets",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_Response_Food_B <-
  left_join(CAIT.emissions[CAIT.emissions$Country=="World" & CAIT.emissions$Year<2014,c(1,2,14)],
            FAOSTAT_FoodBalance[grepl("Food supply",FAOSTAT_FoodBalance$Element,ignore.case=T)==T,c("Element","Year","Unit","Value")],
            by="Year") %>%
  left_join(.,data.frame(Year=FAOSTAT_worldpop[,"Year"],
                         World.Pop=FAOSTAT_worldpop[,"Value"]*1000),
            by="Year") %>%
  mutate(Annual.kcal=(Value*World.Pop*365)/1000000) %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Food"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_Response_Food_B$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Annual.kcal/(`Agriculture (MtCO2e)`*1000000),
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))

# Add Panel-specific measures

Dim_Context_Response_Food_A <-
  Dim_Context_Response_Food_A %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(unique(rbind(Fact_Context_Response_Food_A$Year_Key,Fact_Context_Response_Food_B$Year_Key))),
         Panel_Max_Year=max(unique(rbind(Fact_Context_Response_Food_A$Year_Key,Fact_Context_Response_Food_B$Year_Key))))

Dim_Context_Response_Food_B <-
  Dim_Context_Response_Food_B %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(unique(rbind(Fact_Context_Response_Food_A$Year_Key,Fact_Context_Response_Food_B$Year_Key))),
         Panel_Max_Year=max(unique(rbind(Fact_Context_Response_Food_A$Year_Key,Fact_Context_Response_Food_B$Year_Key))))



# plotting.food.response.b <- 
#   Fact_Context_Response_Food_B %>%
#   transmute(Year_Key=Year_Key,
#             Practice_Key=Practice_Key,
#             Indicator_Type_Key=Indicator_Type_Key,
#             Indicator_Value=Indicator_Value*4,
#             Indicator_Upper_Value=Indicator_Upper_Value,
#             Indicator_Lower_Value=Indicator_Lower_Value)


# ---- 1.4 Consolidated Food-specific Global Context tables ----

Dim_Context_Food <- 
  rbind.data.frame(Dim_Context_State_Food_A,
                   Dim_Context_State_Food_B,
                   Dim_Context_State_Food_C,
                   Dim_Context_Threat_Food_A,
                   Dim_Context_Threat_Food_B,
                   Dim_Context_Threat_Food_C,
                   Dim_Context_Threat_Food_D,
                   Dim_Context_Response_Food_A,
                   Dim_Context_Response_Food_B)

Fact_Context_Food <-
  rbind.data.frame(Fact_Context_State_Food_A,
                   Fact_Context_State_Food_B,
                   Fact_Context_State_Food_C,
                   Fact_Context_Threat_Food_A,
                   Fact_Context_Threat_Food_B,
                   Fact_Context_Threat_Food_C,
                   Fact_Context_Threat_Food_D,
                   Fact_Context_Response_Food_A,
                   Fact_Context_Response_Food_B)


#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Global 2030 Outcomes ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 2.1 Food Outcome 1 - SUSTAINABLE LAND MANAGEMENT / HABITAT CONVERSION FOR FOOD ----

# -- LAND DEGRADATION -- GLOBAL-SPECIFIC

Dim_Global_2030_Outcome1_Food_A <- 
  data.frame(Indicator_Type_Key="OUT1_FD_A",
             Indicator_Name="FORTHCOMING: SDG 15.3.1 - Proportion of land that is degraded over total land area",
             Indicator_Label="Degraded Land and Sustainable Agricultural Land",
             Indicator_Subcategory="Land Degradation*",
             Indicator_Unit="% of total land",
             Data_source="UN SDG Indicator Bank -- UNCCD, FAO, UNSD, UNEP, UNFCCC, CBD",
             Indicator_Target=NA,
             Indicator_Type="Outcome",
             Panel_Label="Sustainable Land Management",
             Display_Order=1,
             Global_Indicator="Yes",
             US_Indicator="No")

Fact_Global_2030_Outcome1_Food_A <-
  data.frame(Year_Key=9999,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Food"],length(1)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome1_Food_A$Indicator_Type_Key, length(1)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Food" &
                                                                   grepl("Production",practice_outcome_key_ref$practice_outcome)], length(1)),
            Indicator_Value=NA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Indicator_Target=NA)

# Add Indicator_Latest_Year and Indicator_Latest_Value based on Fact table

Dim_Global_2030_Outcome1_Food_A <- 
  Dim_Global_2030_Outcome1_Food_A %>%
  mutate(Indicator_Latest_Year=max(Fact_Global_2030_Outcome1_Food_A$Year_Key,na.rm=T),
         Indicator_Latest_Value=Fact_Global_2030_Outcome1_Food_A$Indicator_Value[Fact_Global_2030_Outcome1_Food_A$Year_Key==Indicator_Latest_Year])


# -- SUSTAINABLE AGRICULTURE -- GLOBAL-SPECIFIC

Dim_Global_2030_Outcome1_Food_B <- 
  data.frame(Indicator_Type_Key="OUT1_FD_B",
             Indicator_Name="FORTHCOMING: SDG 2.4.1 - Proportion of agricultural land under productive and sustainable agriculture",
             Indicator_Label="Degraded Land and Sustainable Agricultural Land",
             Indicator_Subcategory="Sustainable Agriculture*",
             Indicator_Unit="% of agricultural land",
             Data_source="UN SDG Indicator Bank -- FAO",
             Indicator_Target=NA,
             Indicator_Type="Outcome",
             Panel_Label="Sustainable Land Management",
             Display_Order=1,
             Global_Indicator="Yes",
             US_Indicator="No")

Fact_Global_2030_Outcome1_Food_B <-
  data.frame(Year_Key=9999,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Food"],length(1)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome1_Food_B$Indicator_Type_Key, length(1)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Food" &
                                                                   grepl("Production",practice_outcome_key_ref$practice_outcome)], length(1)),
            Indicator_Value=NA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Indicator_Target=NA)

# Add Indicator_Latest_Year and Indicator_Latest_Value based on Fact table

Dim_Global_2030_Outcome1_Food_B <- 
  Dim_Global_2030_Outcome1_Food_B %>%
  mutate(Indicator_Latest_Year=max(Fact_Global_2030_Outcome1_Food_B$Year_Key,na.rm=T),
         Indicator_Latest_Value=Fact_Global_2030_Outcome1_Food_B$Indicator_Value[Fact_Global_2030_Outcome1_Food_B$Year_Key==Indicator_Latest_Year])


# -- TERRESTRIAL HABITAT CONVERSION -- US-SPECIFIC 

Dim_Global_2030_Outcome1_Food_C <- 
  data.frame(Indicator_Type_Key="OUT1_FD_C",
             Indicator_Name="Commodity-driven forest cover loss",
             Indicator_Label="Terrestrial & Mangrove Habitat Conversion",
             Indicator_Subcategory="Commodity Driven Forest Cover Loss",
             Indicator_Unit="M ha per year",
             Data_source="Global Forest Watch - Curtis et al (2018) Global drivers of forest loss",
             Indicator_Target=0,
             Indicator_Type="Outcome",
             Panel_Label="Habitat Conversion for Food",
             Display_Order=1,
             Global_Indicator="No",
             US_Indicator="Yes")

Fact_Global_2030_Outcome1_Food_C <-
  import(last.file(dir.nam = dir.nam.Forest, nam = 'GFW_treeloss_bydriver')) %>%
  subset(.,Loss_type=="Commodity driven deforestation") %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Food"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome1_Food_C$Indicator_Type_Key, length(Year_Key)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Food" &
                                                                   grepl("Production",practice_outcome_key_ref$practice_outcome)], length(Year_Key)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Indicator_Target=NA)

# Add Indicator_Latest_Year and Indicator_Latest_Value based on Fact table

Dim_Global_2030_Outcome1_Food_C <- 
  Dim_Global_2030_Outcome1_Food_C %>%
  mutate(Indicator_Latest_Year=max(Fact_Global_2030_Outcome1_Food_C$Year_Key,na.rm=T),
         Indicator_Latest_Value=Fact_Global_2030_Outcome1_Food_C$Indicator_Value[Fact_Global_2030_Outcome1_Food_C$Year_Key==Indicator_Latest_Year])

# Add target value to Fact table

Fact_Global_2030_Outcome1_Food_C <-
  rbind.data.frame(Fact_Global_2030_Outcome1_Food_C,
                   data.frame(Year_Key=2030,
                              Practice_Key=practice_key_ref$id[practice_key_ref$practice_name=="Food"],
                              Indicator_Type_Key=Dim_Global_2030_Outcome1_Food_C$Indicator_Type_Key,
                              Practice_Outcome_Key=practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Food" &
                                                                                 grepl("Production",practice_outcome_key_ref$practice_outcome)],
                              Indicator_Value=NA,
                              Indicator_Upper_Value=NA,
                              Indicator_Lower_Value=NA,
                              Indicator_Target=Dim_Global_2030_Outcome1_Food_C$Indicator_Target))


# -- MANGROVE HABITAT CONVERSION -- US-SPECIFIC

Dim_Global_2030_Outcome1_Food_D <- 
  data.frame(Indicator_Type_Key="OUT1_FD_D",
             Indicator_Name="Mangrove forest change",
             Indicator_Label="Terrestrial & Mangrove Habitat Conversion",
             Indicator_Subcategory="Aquaculture Driven Mangrove Cover Loss",
             Indicator_Unit="M ha per year",
             Data_source="Thomas et al (2017) Distribution and drivers of global mangrove forest change, 1996-2010",
             Indicator_Target=0,
             Indicator_Type="Outcome",
             Panel_Label="Habitat Conversion for Food",
             Display_Order=1,
             Global_Indicator="No",
             US_Indicator="Yes")

Fact_Global_2030_Outcome1_Food_D <-
  data.frame(Year_Key=9999,
             Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Food"],length(1)),
             Indicator_Type_Key=rep(Dim_Global_2030_Outcome1_Food_D$Indicator_Type_Key, length(1)),
             Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Food" &
                                                                    grepl("Production",practice_outcome_key_ref$practice_outcome)], length(1)),
             Indicator_Value=NA,
             Indicator_Upper_Value=NA,
             Indicator_Lower_Value=NA,
             Indicator_Target=NA)

# Add Indicator_Latest_Year and Indicator_Latest_Value based on Fact table

Dim_Global_2030_Outcome1_Food_D <- 
  Dim_Global_2030_Outcome1_Food_D %>%
  mutate(Indicator_Latest_Year=max(Fact_Global_2030_Outcome1_Food_D$Year_Key,na.rm=T),
         Indicator_Latest_Value=Fact_Global_2030_Outcome1_Food_D$Indicator_Value[Fact_Global_2030_Outcome1_Food_D$Year_Key==Indicator_Latest_Year])


# ---- 2.2 Food Outcome 2 - FOOD LOSS & WASTE ----

# -- LOSS

Dim_Global_2030_Outcome2_Food_A <- 
  data.frame(Indicator_Type_Key="OUT2_FD_A",
             Indicator_Name="FORTHCOMING: SDG 12.3.1.b -- Global Food Loss Index (GFLI)",
             Indicator_Label="Loss Index and Kilograms Wasted",
             Indicator_Subcategory="Per Capita Global Food Loss",
             Indicator_Unit="% reduction since 2011 baseline",
             Data_source="FAO. (2011). Global food losses and food waste [Report]: Dusseldorf, Germany.",
             Indicator_Target=50, # baseline value from 2011 is 162 kg/yr
             Indicator_Type="Outcome",
             Panel_Label="Food Loss & Waste",
             Display_Order=2,
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Global_2030_Outcome2_Food_A <-
  data.frame(Year_Key=2011,
            Practice_Key=practice_key_ref$id[practice_key_ref$practice_name=="Food"],
            Indicator_Type_Key=Dim_Global_2030_Outcome2_Food_A$Indicator_Type_Key,
            Practice_Outcome_Key=practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Food" &
                                                                   grepl("Waste",practice_outcome_key_ref$practice_outcome)],
            Indicator_Value=0,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Indicator_Target=NA) 

# Add Indicator_Latest_Year and Indicator_Latest_Value based on Fact table

Dim_Global_2030_Outcome2_Food_A <- 
  Dim_Global_2030_Outcome2_Food_A %>%
  mutate(Indicator_Latest_Year=max(Fact_Global_2030_Outcome2_Food_A$Year_Key,na.rm=T),
         Indicator_Latest_Value=Fact_Global_2030_Outcome2_Food_A$Indicator_Value[Fact_Global_2030_Outcome2_Food_A$Year_Key==Indicator_Latest_Year])

# Add target value to Fact table

Fact_Global_2030_Outcome2_Food_A <-
  rbind.data.frame(Fact_Global_2030_Outcome2_Food_A,
                   data.frame(Year_Key=2030,
                              Practice_Key=practice_key_ref$id[practice_key_ref$practice_name=="Food"],
                              Indicator_Type_Key=Dim_Global_2030_Outcome2_Food_A$Indicator_Type_Key,
                              Practice_Outcome_Key=practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Food" &
                                                                                 grepl("Waste",practice_outcome_key_ref$practice_outcome)],
                              Indicator_Value=NA,
                              Indicator_Upper_Value=NA,
                              Indicator_Lower_Value=NA,
                              Indicator_Target=Dim_Global_2030_Outcome2_Food_A$Indicator_Target))

# -- WASTE

Dim_Global_2030_Outcome2_Food_B <- 
  data.frame(Indicator_Type_Key="OUT2_FD_B",
             Indicator_Name="FORTHCOMING: SDG 12.3.1.a -- Per capita food waste (kg/year)",
             Indicator_Label="Loss Index and Kilograms Wasted",
             Indicator_Subcategory="Per Capita Global Food Waste",
             Indicator_Unit="% reduction since 2011 baseline",
             Data_source="FAO. (2011). Global food losses and food waste [Report]: Dusseldorf, Germany.",
             Indicator_Target=50, # baseline value from 2011 is 51 kg/yr
             Indicator_Type="Outcome",
             Panel_Label="Food Loss & Waste",
             Display_Order=2,
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Global_2030_Outcome2_Food_B <-
  data.frame(Year_Key=2011,
            Practice_Key=practice_key_ref$id[practice_key_ref$practice_name=="Food"],
            Indicator_Type_Key=Dim_Global_2030_Outcome2_Food_B$Indicator_Type_Key,
            Practice_Outcome_Key=practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Food" &
                                                                   grepl("Waste",practice_outcome_key_ref$practice_outcome)],
            Indicator_Value=0,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Indicator_Target=NA)

# Add Indicator_Latest_Year and Indicator_Latest_Value based on Fact table

Dim_Global_2030_Outcome2_Food_B <- 
  Dim_Global_2030_Outcome2_Food_B %>%
  mutate(Indicator_Latest_Year=max(Fact_Global_2030_Outcome2_Food_B$Year_Key,na.rm=T),
         Indicator_Latest_Value=Fact_Global_2030_Outcome2_Food_B$Indicator_Value[Fact_Global_2030_Outcome2_Food_B$Year_Key==Indicator_Latest_Year])

# Add target value to Fact table

Fact_Global_2030_Outcome2_Food_B <-
  rbind.data.frame(Fact_Global_2030_Outcome2_Food_B,
                   data.frame(Year_Key=2030,
                              Practice_Key=practice_key_ref$id[practice_key_ref$practice_name=="Food"],
                              Indicator_Type_Key=Dim_Global_2030_Outcome2_Food_B$Indicator_Type_Key,
                              Practice_Outcome_Key=practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Food" &
                                                                                 grepl("Waste",practice_outcome_key_ref$practice_outcome)],
                              Indicator_Value=NA,
                              Indicator_Upper_Value=NA,
                              Indicator_Lower_Value=NA,
                              Indicator_Target=Dim_Global_2030_Outcome2_Food_B$Indicator_Target))


# ---- 2.3 Food Outcome 3 - HEALTHY DIETS ----

Dim_Global_2030_Outcome3_Food_A <- 
  data.frame(Indicator_Type_Key="OUT3_FD_A",
             Indicator_Name="FORTHCOMING: Actual global food plate (FAO Food Balance sheets) compared to EAT-Lancet diet",
             Indicator_Label="Global Food Plate*",
             Indicator_Subcategory="(under development by members of Food Practice & WWF-UK)",
             Indicator_Unit="% matched to EAT-Lancet diet",
             Data_source="Developed and analyzed by WWF's Food Practice -- in collaboation with WWF-UK, Tanya Steele",
             Indicator_Target=NA,
             Indicator_Type="Outcome",
             Panel_Label="Healthy Diets",
             Display_Order=3,
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Global_2030_Outcome3_Food_A <-
  data.frame(Year_Key=9999,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Food"],length(1)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome3_Food_A$Indicator_Type_Key, length(1)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Food" &
                                                                   grepl("Diets",practice_outcome_key_ref$practice_outcome)], length(1)),
            Indicator_Value=NA,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Indicator_Target=NA)

# Add Indicator_Latest_Year and Indicator_Latest_Value based on Fact table

Dim_Global_2030_Outcome3_Food_A <- 
  Dim_Global_2030_Outcome3_Food_A %>%
  mutate(Indicator_Latest_Year=max(Fact_Global_2030_Outcome3_Food_A$Year_Key,na.rm=T),
         Indicator_Latest_Value=Fact_Global_2030_Outcome3_Food_A$Indicator_Value[Fact_Global_2030_Outcome3_Food_A$Year_Key==Indicator_Latest_Year])


# ---- 2.4 Consolidated Food-specific Global 2030 Outcome tables ----

Dim_Global_2030_Outcome_Food <-
  rbind.data.frame(Dim_Global_2030_Outcome1_Food_A,
                   Dim_Global_2030_Outcome1_Food_B,
                   Dim_Global_2030_Outcome1_Food_C,
                   Dim_Global_2030_Outcome1_Food_D,
                   Dim_Global_2030_Outcome2_Food_A,
                   Dim_Global_2030_Outcome2_Food_B,
                   Dim_Global_2030_Outcome3_Food_A)

Fact_Global_2030_Outcome_Food <-
  rbind.data.frame(Fact_Global_2030_Outcome1_Food_A,
                   Fact_Global_2030_Outcome1_Food_B,
                   Fact_Global_2030_Outcome1_Food_C,
                   Fact_Global_2030_Outcome1_Food_D,
                   Fact_Global_2030_Outcome2_Food_A,
                   Fact_Global_2030_Outcome2_Food_B,
                   Fact_Global_2030_Outcome3_Food_A)


#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Initiatives ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 3.1 Load data ----

dim_initiatives_Food <- 
  dim_initiatives %>% subset(goal=="Food") 

dim_initiative_indicators_Food <-
  dim_initiative_indicators %>% subset(goal=="Food")

fact_initiative_indicators_Food <-
  fact_initiative_indicators %>% subset(goal=="Food")

dim_initiative_milestones_Food <-
  dim_initiative_milestones %>% subset(goal=="Food")

pie_type_Food <-
  pie_type %>% subset(goal=="Food")


# ---- 3.2 Food-specific Dim_Initiative ----

Dim_Initiative_Food <-
  dim_initiatives_Food %>%
  transmute(Initiative_Key=initiativekey,
            Initiative_Name=initiative,
            Initiative_Status=initiativestatus,
            Initiative_Status_Justification=initiativejust,
            Initiative_Goal=initiativestatement,
            Global_Initiative=globalinitiative,
            US_Initiative=usinitiative,
            Display_Order=displayorder)


# ---- 3.3 Food-specific Dim_Initiative_Indicator_Type ----

Dim_Initiative_Indicator_Food <-
  left_join(dim_initiative_indicators_Food,
            pie_type_Food[,c("indicatorkey","pie.type","amount.achieved","amount.remaining","max.year.value")],
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


# ---- 3.4 Food-specific Fact_Initiative_Indicators ----

Fact_Initiative_Indicator_Food <-
  left_join(fact_initiative_indicators_Food,pie_type_Food[,c("indicatorkey","target.year","subcattarget")],by="indicatorkey") %>%
  left_join(dim_initiative_indicators_Food[,c("indicatorkey","indicatorunits")],by="indicatorkey") %>%
  transmute(Year_Key=ifelse(!is.na(Year),Year,9999),
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Food"],length(Year_Key)),
            Initiative_Key=initiativekey,
            Indicator_Type_Key=indicatorkey,
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Value_Trend=ifelse(grepl("reduction",indicatorunits,ignore.case=T)==T,-(Value),Value),
            Indicator_Target=ifelse(!is.na(subcattarget) & Year==target.year,subcattarget,NA),
            Target_Trend=ifelse(grepl("reduction",indicatorunits,ignore.case=T)==T,-(Indicator_Target),Indicator_Target))


# ---- 3.5 Food-specific Fact_Initiative_Financials ----

Fact_Initiative_Financials_Food <-
  dim_initiatives_Food %>%
  transmute(Date_Key=date,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Food"],length(Date_Key)),
            Initiative_Key=initiativekey,
            Amount_Needed=fundsneeded,
            Amount_Secured=fundssecured,
            Amount_Anticipated=fundsanticipated,
            Amount_Remaining=fundsneeded-fundssecured-fundsanticipated)


# ---- 3.6 Food-specific Milestone_Group_Bridge ----

Milestone_Group_Bridge_Food <-
  left_join(dim_initiative_milestones_Food, dim_initiatives_Food, by=c("initiative", "goal")) %>%
  transmute(Milestone_Key=milestonekey,
            Initiative_Key=initiativekey)


# ---- 3.7 Food-specific Dim_Milestone ----

Dim_Milestone_Food <-
  dim_initiative_milestones_Food %>%
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

rm(FAOSTAT_FoodBalance,
   OECDStat_LandUse,
   CAIT.emissions,
   FAOSTAT_worldpop,
   Dim_Context_State_Food_A,
   Dim_Context_State_Food_B,
   Dim_Context_State_Food_C,
   Dim_Context_Threat_Food_A,
   Dim_Context_Threat_Food_B,
   Dim_Context_Threat_Food_C,
   Dim_Context_Threat_Food_D,
   Dim_Context_Response_Food_A,
   Dim_Context_Response_Food_B,
   Fact_Context_State_Food_A,
   Fact_Context_State_Food_B,
   Fact_Context_State_Food_C,
   Fact_Context_Threat_Food_A,
   Fact_Context_Threat_Food_B,
   Fact_Context_Threat_Food_C,
   Fact_Context_Threat_Food_D,
   Fact_Context_Response_Food_A,
   Fact_Context_Response_Food_B,
   Dim_Global_2030_Outcome1_Food_A,
   Dim_Global_2030_Outcome1_Food_B,
   Dim_Global_2030_Outcome1_Food_C,
   Dim_Global_2030_Outcome1_Food_D,
   Dim_Global_2030_Outcome2_Food_A,
   Dim_Global_2030_Outcome2_Food_B,
   Dim_Global_2030_Outcome3_Food_A,
   Fact_Global_2030_Outcome1_Food_A,
   Fact_Global_2030_Outcome1_Food_B,
   Fact_Global_2030_Outcome1_Food_C,
   Fact_Global_2030_Outcome1_Food_D,
   Fact_Global_2030_Outcome2_Food_A,
   Fact_Global_2030_Outcome2_Food_B,
   Fact_Global_2030_Outcome3_Food_A,
   dim_initiatives_Food,
   dim_initiative_indicators_Food,
   fact_initiative_indicators_Food,
   dim_initiative_milestones_Food,
   pie_type_Food)
