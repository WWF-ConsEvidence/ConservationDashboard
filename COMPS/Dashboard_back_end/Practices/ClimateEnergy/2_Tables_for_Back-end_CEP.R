# 
# code: Climate & Energy Practice Indicator and Initiative Tables FOR COMPS DASHBOARD
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: August 2018
# modified: June 2020
# 
# ---- inputs ----
#  1) CEP-specific data tables
# 
# ---- outputs ----
#  1) CEP-specific back-end tables -- ready to be consolidated with other Practices, to go into back-end:
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

Temp_land_sea <- 
  import(last.file(dir.nam = dir.nam.CEP, nam = 'Berkeley_land_sea_dl'))

Temp_scaling_factor <- mean(Temp_land_sea$Annual_Anomaly[Temp_land_sea$Year>2005 &
                                    Temp_land_sea$Year<2016]) - 0.98

# -- SURFACE TEMPERATURE

Dim_Context_State_CEP_A <- 
  data.frame(Indicator_Type_Key="GCS_CE_A",
             Indicator_Name="Global annual summary of surface temperature over land & sea (temperature anomaly from pre-industrial average)",
             Indicator_Label="Global Average Surface Temperature",
             Panel_Label="Surface Temperature",
             Panel="State",
             Indicator_Subcategory=NA,
             Indicator_Unit="Pre-industrial temperature anomaly (Celsius)",
             Data_Source="Berkeley Earth Land & Ocean summary data, http://berkeleyearth.lbl.gov/auto/Global/Land_and_Ocean_summary.txt",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_State_CEP_A <-
  Temp_land_sea %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_State_CEP_A$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Annual_Anomaly - Temp_scaling_factor, 
            Indicator_Upper_Value=Annual_Unc + Annual_Anomaly - Temp_scaling_factor,
            Indicator_Lower_Value=-Annual_Unc + Annual_Anomaly - Temp_scaling_factor,
            Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))

# Add Panel-specific measures

Dim_Context_State_CEP_A <-
  Dim_Context_State_CEP_A %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(Fact_Context_State_CEP_A$Year_Key,na.rm=T),
         Panel_Max_Year=max(Fact_Context_State_CEP_A$Year_Key,na.rm=T))


# ---- 1.2 Context - Threat ----

EIA_fossil_fuel <- 
  import(last.file(dir.nam = dir.nam.CEP, nam = 'US_EIA_fossil_fuel_consump_dl'),
                            sheet='Data') %>%
  subset(.,Geography=="World") %>%
  melt(.,id.vars=c("Primary_Energy","Geography","Units"),
       variable.name="Year",value.name="Value") %>%
  group_by(Year) %>%
  summarise(Total_energy=sum(Value),
            Coal=sum(Value[Primary_Energy=="Coal"]),
            Gas=sum(Value[Primary_Energy=="Natural Gas"]),
            Oil=sum(Value[Primary_Energy=="Petroleum and Other Liquids"]))

# -- FOSSIL FUEL CONSUMPTION - COAL

Dim_Context_Threat_CEP_A <- 
  data.frame(Indicator_Type_Key="GCT_CE_A",
             Indicator_Name="Global annual coal fuel consumption",
             Indicator_Label="Global Fossil Fuel Consumption, by Energy Type",
             Panel_Label="Fossil Fuel Consumption",
             Panel="Threat",
             Indicator_Subcategory="Coal",
             Indicator_Unit="% of total energy consumption",
             Data_Source="U.S. Energy and Information Administration",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_Threat_CEP_A <-
  EIA_fossil_fuel %>%
  transmute(Year_Key=as.numeric(substr(Year,2,5)),
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_Threat_CEP_A$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Coal/Total_energy, 
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))

# -- FOSSIL FUEL CONSUMPTION - NATURAL GAS

Dim_Context_Threat_CEP_B <- 
  data.frame(Indicator_Type_Key="GCT_CE_B",
             Indicator_Name="Global annual natural gas fuel consumption",
             Indicator_Label="Global Fossil Fuel Consumption, by Energy Type",
             Panel_Label="Fossil Fuel Consumption",
             Panel="Threat",
             Indicator_Subcategory="Natural Gas",
             Indicator_Unit="% of total energy consumption",
             Data_Source="U.S. Energy and Information Administration",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_Threat_CEP_B <-
  EIA_fossil_fuel %>%
  transmute(Year_Key=as.numeric(substr(Year,2,5)),
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_Threat_CEP_B$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Gas/Total_energy, 
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))

# -- FOSSIL FUEL CONSUMPTION - OIL

Dim_Context_Threat_CEP_C <- 
  data.frame(Indicator_Type_Key="GCT_CE_C",
             Indicator_Name="Global annual petroleum and other liquid fuel consumption",
             Indicator_Label="Global Fossil Fuel Consumption, by Energy Type",
             Panel_Label="Fossil Fuel Consumption",
             Panel="Threat",
             Indicator_Subcategory="Oil",
             Indicator_Unit="% of total energy consumption",
             Data_Source="U.S. Energy and Information Administration",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_Threat_CEP_C <-
  EIA_fossil_fuel %>%
  transmute(Year_Key=as.numeric(substr(Year,2,5)),
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_Threat_CEP_C$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Oil/Total_energy, 
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))

# Add Panel-specific measures

Dim_Context_Threat_CEP_A <-
  Dim_Context_Threat_CEP_A %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(unique(rbind(Fact_Context_Threat_CEP_A$Year_Key,Fact_Context_Threat_CEP_B$Year_Key,Fact_Context_Threat_CEP_C$Year_Key))),
         Panel_Max_Year=max(unique(rbind(Fact_Context_Threat_CEP_A$Year_Key,Fact_Context_Threat_CEP_B$Year_Key,Fact_Context_Threat_CEP_C$Year_Key))))

Dim_Context_Threat_CEP_B <-
  Dim_Context_Threat_CEP_B %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(unique(rbind(Fact_Context_Threat_CEP_A$Year_Key,Fact_Context_Threat_CEP_B$Year_Key,Fact_Context_Threat_CEP_C$Year_Key))),
         Panel_Max_Year=max(unique(rbind(Fact_Context_Threat_CEP_A$Year_Key,Fact_Context_Threat_CEP_B$Year_Key,Fact_Context_Threat_CEP_C$Year_Key))))

Dim_Context_Threat_CEP_C <-
  Dim_Context_Threat_CEP_C %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(unique(rbind(Fact_Context_Threat_CEP_A$Year_Key,Fact_Context_Threat_CEP_B$Year_Key,Fact_Context_Threat_CEP_C$Year_Key))),
         Panel_Max_Year=max(unique(rbind(Fact_Context_Threat_CEP_A$Year_Key,Fact_Context_Threat_CEP_B$Year_Key,Fact_Context_Threat_CEP_C$Year_Key))))


# ---- 1.3 Context - Response ----

emissions.gap <- import(last.file(dir.nam = dir.nam.CEP, nam = 'UNEP_emissions_gap'))

# -- EMISSIONS GAP - CURRENT POLICY TRAJECTORY

Dim_Context_Response_CEP_A <- 
  data.frame(Indicator_Type_Key="GCR_CE_A",
             Indicator_Name="Global GHG Emissions Gap, current policy trajectory",
             Indicator_Label="Global GHG Emissions: Three Scenarios",
             Panel_Label="Emissions Gap",
             Panel="Response",
             Indicator_Subcategory="Current Policy Trajectory",
             Indicator_Unit="Gt CO2e",
             Data_Source="UNEP, The Emissions Gap Report 2017 -- p.13-15",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_Response_CEP_A <-
  emissions.gap[emissions.gap$Trajectory=="Current policy",] %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_Response_CEP_A$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Median, 
            Indicator_Upper_Value=Upper,
            Indicator_Lower_Value=Lower,
            Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))

# -- EMISSIONS GAP - 2 DEGREE C PATHWAY

Dim_Context_Response_CEP_B <- 
  data.frame(Indicator_Type_Key="GCR_CE_B",
             Indicator_Name="Global GHG Emissions Gap, 2 degree C pathway (median and 10th and 90th percentiles shown, for 10 scenarios) -- more than 66% chance 2 degrees, least-cost from 2020",
             Indicator_Label="Global GHG Emissions: Three Scenarios",
             Panel_Label="Emissions Gap",
             Panel="Response",
             Indicator_Subcategory="2 Degree C Pathway",
             Indicator_Unit="Gt CO2e",
             Data_Source="UNEP, The Emissions Gap Report 2017 -- p.13-15",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_Response_CEP_B <-
  emissions.gap[emissions.gap$Trajectory=="2 degree pathway",] %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_Response_CEP_B$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Median, 
            Indicator_Upper_Value=Upper,
            Indicator_Lower_Value=Lower,
            Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))

# -- EMISSIONS GAP - 2 DEGREE C PATHWAY

Dim_Context_Response_CEP_C <- 
  data.frame(Indicator_Type_Key="GCR_CE_C",
             Indicator_Name="Global GHG Emissions Gap, 1.5 degree C pathway (median and 10th and 90th percentiles shown, for 6 scenarios) -- 50-66% chance 1.5 degrees, least-cost from 2020",
             Indicator_Label="Global GHG Emissions: Three Scenarios",
             Panel_Label="Emissions Gap",
             Panel="Response",
             Indicator_Subcategory="1.5 Degree C Pathway",
             Indicator_Unit="Gt CO2e",
             Data_Source="UNEP, The Emissions Gap Report 2017 -- p.13-15",
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Context_Response_CEP_C <-
  emissions.gap[emissions.gap$Trajectory=="1.5 degree pathway",] %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Context_Response_CEP_C$Indicator_Type_Key,length(Year_Key)),
            Indicator_Value=Median, 
            Indicator_Upper_Value=Upper,
            Indicator_Lower_Value=Lower,
            Value_First_Last=ifelse(Year_Key==max(Year_Key) | Year_Key==min(Year_Key),Indicator_Value,NA))

# Add Panel-specific measures

Dim_Context_Response_CEP_A <-
  Dim_Context_Response_CEP_A %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(unique(rbind(Fact_Context_Response_CEP_A$Year_Key,Fact_Context_Response_CEP_B$Year_Key,Fact_Context_Response_CEP_C$Year_Key))),
         Panel_Max_Year=max(unique(rbind(Fact_Context_Response_CEP_A$Year_Key,Fact_Context_Response_CEP_B$Year_Key,Fact_Context_Response_CEP_C$Year_Key))))

Dim_Context_Response_CEP_B <-
  Dim_Context_Response_CEP_B %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(unique(rbind(Fact_Context_Response_CEP_A$Year_Key,Fact_Context_Response_CEP_B$Year_Key,Fact_Context_Response_CEP_C$Year_Key))),
         Panel_Max_Year=max(unique(rbind(Fact_Context_Response_CEP_A$Year_Key,Fact_Context_Response_CEP_B$Year_Key,Fact_Context_Response_CEP_C$Year_Key))))

Dim_Context_Response_CEP_C <-
  Dim_Context_Response_CEP_C %>%
  mutate(Panel_Label_Upper=toupper(Panel_Label),
         Panel_Min_Year=min(unique(rbind(Fact_Context_Response_CEP_A$Year_Key,Fact_Context_Response_CEP_B$Year_Key,Fact_Context_Response_CEP_C$Year_Key))),
         Panel_Max_Year=max(unique(rbind(Fact_Context_Response_CEP_A$Year_Key,Fact_Context_Response_CEP_B$Year_Key,Fact_Context_Response_CEP_C$Year_Key))))


# ---- 1.4 Consolidated CEP-specific Global Context tables ----

Dim_Context_CEP <- 
  rbind.data.frame(Dim_Context_State_CEP_A,
                   Dim_Context_Threat_CEP_A,
                   Dim_Context_Threat_CEP_B,
                   Dim_Context_Threat_CEP_C,
                   Dim_Context_Response_CEP_A,
                   Dim_Context_Response_CEP_B,
                   Dim_Context_Response_CEP_C)

Fact_Context_CEP <-
  rbind.data.frame(Fact_Context_State_CEP_A,
                   Fact_Context_Threat_CEP_A,
                   Fact_Context_Threat_CEP_B,
                   Fact_Context_Threat_CEP_C,
                   Fact_Context_Response_CEP_A,
                   Fact_Context_Response_CEP_B,
                   Fact_Context_Response_CEP_C)

#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Global 2030 Outcomes ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 2.1 CEP Outcome 1 - MITIGATION ----

Dim_Global_2030_Outcome1_CEP_A <- 
  data.frame(Indicator_Type_Key="OUT1_CE_A",
             Indicator_Name="Global GHG emissions, including Land-Use Change and Forestry",
             Indicator_Label="Total GHG Emissions",
             Indicator_Subcategory=NA,
             Indicator_Unit="Gt CO2e",
             Data_source="CAIT Climate Data Explorer. 2017. Washington, DC: World Resources Institute",
             Indicator_Target=NA,
             Indicator_Type="Outcome",
             Panel_Label="Mitigation",
             Display_Order=1,
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Global_2030_Outcome1_CEP_A <-
  import(last.file(dir.nam = dir.nam.CEP, nam = 'CAIT_country_GHG')) %>%
  subset(.,Country=="World" & Year>2009, select=c(1,2,4)) %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome1_CEP_A$Indicator_Type_Key, length(Year_Key)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Climate & Energy" &
                                                                   grepl("Mitigation",practice_outcome_key_ref$practice_outcome)], length(Year_Key)),
            Indicator_Value=Total.GHG.Emissions.Including.Land.Use.Change.and.Forestry..MtCO.e../1000,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Indicator_Target=NA)

# Target value for this indicator is "at least 40% decrease in emissions from 2010 baseline", so we will calculate the target and 
# append the Dim table, using the CAIT data source imported for the Fact table.  
# -- Also add Indicator_Latest_Year and Indicator_Latest_Value based on Fact table

Dim_Global_2030_Outcome1_CEP_A <- 
  Dim_Global_2030_Outcome1_CEP_A %>%
  mutate(Indicator_Target=0.6*Fact_Global_2030_Outcome1_CEP_A$Indicator_Value[Fact_Global_2030_Outcome1_CEP_A$Year_Key==2010],
         Indicator_Latest_Year=max(Fact_Global_2030_Outcome1_CEP_A$Year_Key,na.rm=T),
         Indicator_Latest_Value=Fact_Global_2030_Outcome1_CEP_A$Indicator_Value[Fact_Global_2030_Outcome1_CEP_A$Year_Key==Indicator_Latest_Year])
  

# Add target value to Fact table

Fact_Global_2030_Outcome1_CEP_A <-
  rbind.data.frame(Fact_Global_2030_Outcome1_CEP_A,
                   data.frame(Year_Key=2030,
                              Practice_Key=practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],
                              Indicator_Type_Key=Dim_Global_2030_Outcome1_CEP_A$Indicator_Type_Key,
                              Practice_Outcome_Key=practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Climate & Energy" &
                                                                                 grepl("Mitigation",practice_outcome_key_ref$practice_outcome)],
                              Indicator_Value=NA,
                              Indicator_Upper_Value=NA,
                              Indicator_Lower_Value=NA,
                              Indicator_Target=Dim_Global_2030_Outcome1_CEP_A$Indicator_Target))


# ---- 2.2 CEP Outcome 2 - ENERGY ----

sdg.7.energy <- import(last.file(dir.nam = dir.nam.CEP, nam = 'SDG_7_energy'))

# -- RENEWABLE ENERGY

Dim_Global_2030_Outcome2_CEP_A <- 
  data.frame(Indicator_Type_Key="OUT2_CE_A",
             Indicator_Name="Renewable energy share in the total final energy consumption",
             Indicator_Label="Renewable, Efficient, Universal, & Clean",
             Indicator_Subcategory="Renewable Energy",
             Indicator_Unit="% of total energy consumption",
             Data_source="UN SDG Indicator Bank - World Bank analysis based on World Energy Statistics and Balances, IEA (2017); 
             Energy Balances, UN Statistics Division (2017)",
             Indicator_Target=40,
             Indicator_Type="Outcome",
             Panel_Label="Energy",
             Display_Order=2,
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Global_2030_Outcome2_CEP_A <-
  sdg.7.energy[grepl("Renewable",sdg.7.energy$SeriesDescription,ignore.case=T)==T,] %>%
  transmute(Year_Key=TimePeriod,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome2_CEP_A$Indicator_Type_Key, length(Year_Key)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Climate & Energy" &
                                                                   grepl("Energy",practice_outcome_key_ref$practice_outcome)], length(Year_Key)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Indicator_Target=NA)

# Add Indicator_Latest_Year and Indicator_Latest_Value based on Fact table

Dim_Global_2030_Outcome2_CEP_A <- 
  Dim_Global_2030_Outcome2_CEP_A %>%
  mutate(Indicator_Latest_Year=max(Fact_Global_2030_Outcome2_CEP_A$Year_Key,na.rm=T),
         Indicator_Latest_Value=Fact_Global_2030_Outcome2_CEP_A$Indicator_Value[Fact_Global_2030_Outcome2_CEP_A$Year_Key==Indicator_Latest_Year])

# Add target value to Fact table

Fact_Global_2030_Outcome2_CEP_A <-
  rbind.data.frame(Fact_Global_2030_Outcome2_CEP_A,
                   data.frame(Year_Key=2030,
                              Practice_Key=practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],
                              Indicator_Type_Key=Dim_Global_2030_Outcome2_CEP_A$Indicator_Type_Key,
                              Practice_Outcome_Key=practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Climate & Energy" &
                                                                                 grepl("Energy",practice_outcome_key_ref$practice_outcome)],
                              Indicator_Value=NA,
                              Indicator_Upper_Value=NA,
                              Indicator_Lower_Value=NA,
                              Indicator_Target=Dim_Global_2030_Outcome2_CEP_A$Indicator_Target))


# -- ENERGY INTENSITY

Dim_Global_2030_Outcome2_CEP_B <- 
  data.frame(Indicator_Type_Key="OUT2_CE_B",
             Indicator_Name="Energy intensity level of primary energy (megajoules per constant 2011 purchasing power parity GDP)",
             Indicator_Label="Renewable, Efficient, Universal, & Clean",
             Indicator_Subcategory="Energy Intensity",
             Indicator_Unit="MJ/2011 USD PPP",
             Data_source="UN SDG Indicator Bank - World Bank analysis based on World Energy Statistics and Balances, IEA (2017); 
             Energy Balances, UN Statistics Division (2017)",
             Indicator_Target=0.5*sdg.7.energy$Value[grepl("Intensity",sdg.7.energy$SeriesDescription,ignore.case=T)==T &
                                                       sdg.7.energy$TimePeriod==2010],
             Indicator_Type="Outcome",
             Panel_Label="Energy",
             Display_Order=2,
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Global_2030_Outcome2_CEP_B <-
  sdg.7.energy[grepl("Intensity",sdg.7.energy$SeriesDescription,ignore.case=T)==T,] %>%
  transmute(Year_Key=TimePeriod,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome2_CEP_B$Indicator_Type_Key, length(Year_Key)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Climate & Energy" &
                                                                   grepl("Energy",practice_outcome_key_ref$practice_outcome)], length(Year_Key)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Indicator_Target=NA)

# Add Indicator_Latest_Year and Indicator_Latest_Value based on Fact table

Dim_Global_2030_Outcome2_CEP_B <- 
  Dim_Global_2030_Outcome2_CEP_B %>%
  mutate(Indicator_Latest_Year=max(Fact_Global_2030_Outcome2_CEP_B$Year_Key,na.rm=T),
         Indicator_Latest_Value=Fact_Global_2030_Outcome2_CEP_B$Indicator_Value[Fact_Global_2030_Outcome2_CEP_B$Year_Key==Indicator_Latest_Year])

# Add target value to Fact table

Fact_Global_2030_Outcome2_CEP_B <-
  rbind.data.frame(Fact_Global_2030_Outcome2_CEP_B,
                   data.frame(Year_Key=2030,
                              Practice_Key=practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],
                              Indicator_Type_Key=Dim_Global_2030_Outcome2_CEP_B$Indicator_Type_Key,
                              Practice_Outcome_Key=practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Climate & Energy" &
                                                                                 grepl("Energy",practice_outcome_key_ref$practice_outcome)],
                              Indicator_Value=NA,
                              Indicator_Upper_Value=NA,
                              Indicator_Lower_Value=NA,
                              Indicator_Target=Dim_Global_2030_Outcome2_CEP_B$Indicator_Target))


# -- UNIVERSAL ENERGY

Dim_Global_2030_Outcome2_CEP_C <- 
  data.frame(Indicator_Type_Key="OUT2_CE_C",
             Indicator_Name="Proportion of world population with access to electricity",
             Indicator_Label="Renewable, Efficient, Universal, & Clean",
             Indicator_Subcategory="Universal Energy",
             Indicator_Unit="% of population with electricity access",
             Data_source="UN SDG Indicator Bank - Global Tracking Framework 2018",
             Indicator_Target=100,
             Indicator_Type="Outcome",
             Panel_Label="Energy",
             Display_Order=2,
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Global_2030_Outcome2_CEP_C <-
  sdg.7.energy[grepl("Electricity",sdg.7.energy$SeriesDescription,ignore.case=T)==T &
                 sdg.7.energy$X.Location.=="ALLAREA",] %>%
  transmute(Year_Key=TimePeriod,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome2_CEP_C$Indicator_Type_Key, length(Year_Key)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Climate & Energy" &
                                                                   grepl("Energy",practice_outcome_key_ref$practice_outcome)], length(Year_Key)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Indicator_Target=NA)

# Add Indicator_Latest_Year and Indicator_Latest_Value based on Fact table

Dim_Global_2030_Outcome2_CEP_C <- 
  Dim_Global_2030_Outcome2_CEP_C %>%
  mutate(Indicator_Latest_Year=max(Fact_Global_2030_Outcome2_CEP_C$Year_Key,na.rm=T),
         Indicator_Latest_Value=Fact_Global_2030_Outcome2_CEP_C$Indicator_Value[Fact_Global_2030_Outcome2_CEP_C$Year_Key==Indicator_Latest_Year])

# Add target value to Fact table

Fact_Global_2030_Outcome2_CEP_C <-
  rbind.data.frame(Fact_Global_2030_Outcome2_CEP_C,
                   data.frame(Year_Key=2030,
                              Practice_Key=practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],
                              Indicator_Type_Key=Dim_Global_2030_Outcome2_CEP_C$Indicator_Type_Key,
                              Practice_Outcome_Key=practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Climate & Energy" &
                                                                                 grepl("Energy",practice_outcome_key_ref$practice_outcome)],
                              Indicator_Value=NA,
                              Indicator_Upper_Value=NA,
                              Indicator_Lower_Value=NA,
                              Indicator_Target=Dim_Global_2030_Outcome2_CEP_C$Indicator_Target))

# -- COAL PLANT PIPELINE

Dim_Global_2030_Outcome2_CEP_D <- 
  data.frame(Indicator_Type_Key="OUT2_CE_D",
             Indicator_Name="Capacity of Coal Plants currently in pipeline (Megawatts)",
             Indicator_Label="Renewable, Efficient, Universal, & Clean",
             Indicator_Subcategory="Coal Plant Pipeline",
             Indicator_Unit="Megawatts of capacity",
             Data_source="Global Coal Plant Tracker, Boom & Bust 2018 Report",
             Indicator_Target=0,
             Indicator_Type="Outcome",
             Panel_Label="Energy",
             Display_Order=2,
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Global_2030_Outcome2_CEP_D <-
  import(last.file(dir.nam = dir.nam.CEP, nam = 'CoalPlantTracker_pipeline'),sheet='Data') %>%
  subset(.,Pipeline=="All Pipeline") %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome2_CEP_D$Indicator_Type_Key, length(Year_Key)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Climate & Energy" &
                                                                   grepl("Energy",practice_outcome_key_ref$practice_outcome)], length(Year_Key)),
            Indicator_Value=Capacity,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Indicator_Target=NA)

# Add Indicator_Latest_Year and Indicator_Latest_Value based on Fact table

Dim_Global_2030_Outcome2_CEP_D <- 
  Dim_Global_2030_Outcome2_CEP_D %>%
  mutate(Indicator_Latest_Year=max(Fact_Global_2030_Outcome2_CEP_D$Year_Key,na.rm=T),
         Indicator_Latest_Value=Fact_Global_2030_Outcome2_CEP_D$Indicator_Value[Fact_Global_2030_Outcome2_CEP_D$Year_Key==Indicator_Latest_Year])

# Add target value to Fact table

Fact_Global_2030_Outcome2_CEP_D <-
  rbind.data.frame(Fact_Global_2030_Outcome2_CEP_D,
                   data.frame(Year_Key=2030,
                              Practice_Key=practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],
                              Indicator_Type_Key=Dim_Global_2030_Outcome2_CEP_D$Indicator_Type_Key,
                              Practice_Outcome_Key=practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Climate & Energy" &
                                                                                 grepl("Energy",practice_outcome_key_ref$practice_outcome)],
                              Indicator_Value=NA,
                              Indicator_Upper_Value=NA,
                              Indicator_Lower_Value=NA,
                              Indicator_Target=Dim_Global_2030_Outcome2_CEP_D$Indicator_Target))


# ---- 2.3 CEP Outcome 3 - ADAPTATION ----

Dim_Global_2030_Outcome3_CEP_A <- 
  data.frame(Indicator_Type_Key="OUT3_CE_A",
             Indicator_Name="Number of countries with a National Adaptation Plan, or equivalent (for developed countries)",
             Indicator_Label="National Adaptation Plans, or equivalent",
             Indicator_Subcategory=NA,
             Indicator_Unit="Number of countries",
             Data_source="UNFCCC NAP Central",
             Indicator_Target=100,
             Indicator_Type="Outcome",
             Panel_Label="Adaptation",
             Display_Order=3,
             Global_Indicator="Yes",
             US_Indicator="Yes")

Fact_Global_2030_Outcome3_CEP_A <-
  import(last.file(dir.nam = dir.nam.CEP, nam = 'NAPCentral_countrylists'),sheet='Sheet1') %>%
  group_by(Year) %>%
  summarise(NumCountries=length(Country)) %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome3_CEP_A$Indicator_Type_Key, length(Year_Key)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Climate & Energy" &
                                                                   grepl("Adaptation",practice_outcome_key_ref$practice_outcome)], length(Year_Key)),
            Indicator_Value=cumsum(NumCountries),
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Indicator_Target=NA)

# Add Indicator_Latest_Year and Indicator_Latest_Value based on Fact table

Dim_Global_2030_Outcome3_CEP_A <- 
  Dim_Global_2030_Outcome3_CEP_A %>%
  mutate(Indicator_Latest_Year=max(Fact_Global_2030_Outcome3_CEP_A$Year_Key,na.rm=T),
         Indicator_Latest_Value=Fact_Global_2030_Outcome3_CEP_A$Indicator_Value[Fact_Global_2030_Outcome3_CEP_A$Year_Key==Indicator_Latest_Year])

# Add target value to Fact table

Fact_Global_2030_Outcome3_CEP_A <-
  rbind.data.frame(Fact_Global_2030_Outcome3_CEP_A,
                   data.frame(Year_Key=2030,
                              Practice_Key=practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],
                              Indicator_Type_Key=Dim_Global_2030_Outcome3_CEP_A$Indicator_Type_Key,
                              Practice_Outcome_Key=practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Climate & Energy" &
                                                                                 grepl("Adaptation",practice_outcome_key_ref$practice_outcome)],
                              Indicator_Value=NA,
                              Indicator_Upper_Value=NA,
                              Indicator_Lower_Value=NA,
                              Indicator_Target=Dim_Global_2030_Outcome3_CEP_A$Indicator_Target))


# ---- 2.4 Consolidated CEP-specific Global 2030 Outcome tables ----

Dim_Global_2030_Outcome_CEP <-
  rbind.data.frame(Dim_Global_2030_Outcome1_CEP_A,
                   Dim_Global_2030_Outcome2_CEP_A,
                   Dim_Global_2030_Outcome2_CEP_B,
                   Dim_Global_2030_Outcome2_CEP_C,
                   Dim_Global_2030_Outcome2_CEP_D,
                   Dim_Global_2030_Outcome3_CEP_A)

Fact_Global_2030_Outcome_CEP <-
  rbind.data.frame(Fact_Global_2030_Outcome1_CEP_A,
                   Fact_Global_2030_Outcome2_CEP_A,
                   Fact_Global_2030_Outcome2_CEP_B,
                   Fact_Global_2030_Outcome2_CEP_C,
                   Fact_Global_2030_Outcome2_CEP_D,
                   Fact_Global_2030_Outcome3_CEP_A)


#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Initiatives ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 3.1 Load data ----

dim_initiatives_CEP <- 
  dim_initiatives %>% subset(goal=="Climate") 

dim_initiative_indicators_CEP <-
  dim_initiative_indicators %>% subset(goal=="Climate")

fact_initiative_indicators_CEP <-
  fact_initiative_indicators %>% subset(goal=="Climate")

dim_initiative_milestones_CEP <-
  dim_initiative_milestones %>% subset(goal=="Climate")

pie_type_CEP <-
  pie_type %>% subset(goal=="Climate")


# ---- 3.2 CEP-specific Dim_Initiative ----

Dim_Initiative_CEP <-
  dim_initiatives_CEP %>%
  transmute(Initiative_Key=initiativekey,
            Initiative_Name=initiative,
            Initiative_Status=initiativestatus,
            Initiative_Status_Justification=initiativejust,
            Initiative_Goal=initiativestatement,
            Global_Initiative=globalinitiative,
            US_Initiative=usinitiative,
            Display_Order=displayorder)


# ---- 3.3 CEP-specific Dim_Initiative_Indicator_Type ----

Dim_Initiative_Indicator_CEP <-
  left_join(dim_initiative_indicators_CEP,
            pie_type_CEP[,c("indicatorkey","pie.type","amount.achieved","amount.remaining","max.year.value")],
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
            Indicator_Label_Abbr=toupper(Indicator.label.abbr), # MUST MANUALLY CALCULATE!!
            Subcategory_Abbr=Subcategory.abbr, # MUST MANUALLY CALCULATE!!
            Amount_Achieved=amount.achieved,
            Amount_Remaining=amount.remaining,
            Pie_Type=pie.type,
            Indicator_Label_Caps=toupper(indicatorlabel),
            Indicator_Latest=max.year.value)


# ---- 3.4 CEP-specific Fact_Initiative_Indicators ----

Fact_Initiative_Indicator_CEP <-
  left_join(fact_initiative_indicators_CEP,pie_type_CEP[,c("indicatorkey","target.year","subcattarget")],by="indicatorkey") %>%
  left_join(dim_initiative_indicators_CEP[,c("indicatorkey","indicatorunits")],by="indicatorkey") %>%
  transmute(Year_Key=ifelse(!is.na(Year),Year,9999),
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],length(Year_Key)),
            Initiative_Key=initiativekey,
            Indicator_Type_Key=indicatorkey,
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA,
            Value_Trend=ifelse(grepl("reduction",indicatorunits,ignore.case=T)==T,-(Value),Value),
            Indicator_Target=ifelse(!is.na(subcattarget) & Year==target.year,Target,NA),
            Target_Trend=ifelse(grepl("reduction",indicatorunits,ignore.case=T)==T,-(Indicator_Target),Indicator_Target))


# ---- 3.5 CEP-specific Fact_Initiative_Financials ----

Fact_Initiative_Financials_CEP <-
  dim_initiatives_CEP %>%
  transmute(Date_Key=Date,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],length(Date_Key)),
            Initiative_Key=initiativekey,
            Amount_Needed=fundsneeded,
            Amount_Secured=fundssecured,
            Amount_Anticipated=fundsanticipated,
            Amount_Remaining=fundsneeded-fundssecured-fundsanticipated)


# ---- 3.6 CEP-specific Milestone_Group_Bridge ----

Milestone_Group_Bridge_CEP <-
  left_join(dim_initiative_milestones_CEP, dim_initiatives_CEP, by=c("initiative", "goal")) %>%
  transmute(Milestone_Key=milestonekey,
            Initiative_Key=initiativekey)


# ---- 3.7 CEP-specific Dim_Milestone ----

Dim_Milestone_CEP <-
  dim_initiative_milestones_CEP %>%
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

rm(Temp_land_sea,
   EIA_fossil_fuel,
   emissions.gap,
   sdg.7.energy,
   Dim_Context_State_CEP_A,
   Dim_Context_Threat_CEP_A,
   Dim_Context_Threat_CEP_B,
   Dim_Context_Threat_CEP_C,
   Dim_Context_Response_CEP_A,
   Dim_Context_Response_CEP_B,
   Dim_Context_Response_CEP_C,
   Fact_Context_State_CEP_A,
   Fact_Context_Threat_CEP_A,
   Fact_Context_Threat_CEP_B,
   Fact_Context_Threat_CEP_C,
   Fact_Context_Response_CEP_A,
   Fact_Context_Response_CEP_B,
   Fact_Context_Response_CEP_C,
   Dim_Global_2030_Outcome1_CEP_A,
   Dim_Global_2030_Outcome2_CEP_A,
   Dim_Global_2030_Outcome2_CEP_B,
   Dim_Global_2030_Outcome2_CEP_C,
   Dim_Global_2030_Outcome2_CEP_D,
   Dim_Global_2030_Outcome3_CEP_A,
   Fact_Global_2030_Outcome1_CEP_A,
   Fact_Global_2030_Outcome2_CEP_A,
   Fact_Global_2030_Outcome2_CEP_B,
   Fact_Global_2030_Outcome2_CEP_C,
   Fact_Global_2030_Outcome2_CEP_D,
   Fact_Global_2030_Outcome3_CEP_A,
   dim_initiatives_CEP,
   dim_initiative_indicators_CEP,
   fact_initiative_indicators_CEP,
   dim_initiative_milestones_CEP,
   pie_type_CEP)
