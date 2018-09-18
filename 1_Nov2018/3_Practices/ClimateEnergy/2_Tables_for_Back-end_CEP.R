# 
# code: Climate & Energy Practice Global Context and Global 2030 Outcome Indicator Tables
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: August 2018
# modified: 
# 
# ---- inputs ----
#  1) CEP-specific data tables (in 1_Nov2018/2_FlatDataFiles/ConsDB_Input)
# 
# ---- outputs ----
#  1) CEP-specific back-end tables -- ready to be consolidated with other Practices, to go into back-end:
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

pacman::p_load(dplyr, xlsx, reshape2)


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

BerkeleyEarth_airtemp <- 
  read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/BerkeleyEarth_land_only_air_temp_dl_2018_0831.xlsx',
            sheetName="Data")

# -- SURFACE TEMPERATURE

Dim_Context_State_CEP_A <- 
  data.frame(Indicator_Type_Key="GCS_CE_A",
             Indicator_Name="Global annual summary of monthly air temperature over land (Celsius)",
             Indicator_Label="Annual Average Air Temperature",
             Panel_Label="Surface Temperature",
             Panel="State",
             Indicator_Subcategory="A",
             Indicator_Unit="Celsius",
             Data_Source="Berkeley Earth, http://berkeleyearth.org/data/")

Fact_Context_State_CEP_A <-
  BerkeleyEarth_airtemp %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],length(Year)),
            Indicator_Type_Key=rep(Dim_Context_State_CEP_A$Indicator_Type_Key,length(Year)),
            # 8.62 degrees (C) is the estimated absolute temperature between 1951-1980 that is used as a reference for all "annual anomalies" 
            Indicator_Value=Annual_Anomaly+8.62, 
            Indicator_Upper_Value=Annual_Unc+Annual_Anomaly+8.62,
            Indicator_Lower_Value=-Annual_Unc+Annual_Anomaly+8.62)


ggplot(Fact_Context_State_CEP_A, aes(x=Year_Key,y=Indicator_Value,
                                     ymin=Indicator_Lower_Value,
                                     ymax=Indicator_Upper_Value)) +
  geom_ribbon(alpha=0.2) +
  geom_line(colour="blue") + 
  scale_x_continuous(breaks=c(1750,1800,1850,1900,1950,1975,2000,2015),
                     expand=c(0,0)) +
  labs(x="Year", y="Average Annual Temperature (Celsius)") +
  plot.theme


# ---- 2.2 Context - Threat ----

EIA_fossil_fuel <- read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/US_EIA_fossil_fuel_consump_dl_2018_0830.xlsx',
                            sheetName='Data')

# -- FOSSIL FUEL CONSUMPTION - COAL

Dim_Context_Threat_CEP_A <- 
  data.frame(Indicator_Type_Key="GCT_CE_A",
             Indicator_Name="Global annual coal fuel consumption",
             Indicator_Label="Coal",
             Panel_Label="Fossil Fuel Consumption",
             Panel="Threat",
             Indicator_Subcategory="A",
             Indicator_Unit="Quad BTU",
             Data_Source="U.S. Energy and Information Administration")

Fact_Context_Threat_CEP_A <-
  melt(EIA_fossil_fuel[EIA_fossil_fuel$Geography=="World" & EIA_fossil_fuel$Primary_Energy=="Coal",], 
       id.vars=c("Primary_Energy","Geography","Units"),
       variable.name="Year",value.name="Value") %>%
  transmute(Year_Key=as.numeric(substr(Year,2,5)),
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],length(Year)),
            Indicator_Type_Key=rep(Dim_Context_Threat_CEP_A$Indicator_Type_Key,length(Year)),
            Indicator_Value=Value, 
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)

# -- FOSSIL FUEL CONSUMPTION - NATURAL GAS

Dim_Context_Threat_CEP_B <- 
  data.frame(Indicator_Type_Key="GCT_CE_B",
             Indicator_Name="Global annual natural gas fuel consumption",
             Indicator_Label="Natural Gas",
             Panel_Label="Fossil Fuel Consumption",
             Panel="Threat",
             Indicator_Subcategory="B",
             Indicator_Unit="Quad BTU",
             Data_Source="U.S. Energy and Information Administration")

Fact_Context_Threat_CEP_B <-
  melt(EIA_fossil_fuel[EIA_fossil_fuel$Geography=="World" & EIA_fossil_fuel$Primary_Energy=="Natural Gas",], 
       id.vars=c("Primary_Energy","Geography","Units"),
       variable.name="Year",value.name="Value") %>%
  transmute(Year_Key=as.numeric(substr(Year,2,5)),
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],length(Year)),
            Indicator_Type_Key=rep(Dim_Context_Threat_CEP_B$Indicator_Type_Key,length(Year)),
            Indicator_Value=Value, 
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)

# -- FOSSIL FUEL CONSUMPTION - OIL

Dim_Context_Threat_CEP_C <- 
  data.frame(Indicator_Type_Key="GCT_CE_C",
             Indicator_Name="Global annual petroleum and other liquid fuel consumption",
             Indicator_Label="Oil",
             Panel_Label="Fossil Fuel Consumption",
             Panel="Threat",
             Indicator_Subcategory="C",
             Indicator_Unit="Quad BTU",
             Data_Source="U.S. Energy and Information Administration")

Fact_Context_Threat_CEP_C <-
  melt(EIA_fossil_fuel[EIA_fossil_fuel$Geography=="World" & grepl("Petroleum",EIA_fossil_fuel$Primary_Energy,ignore.case=T)==T,], 
       id.vars=c("Primary_Energy","Geography","Units"),
       variable.name="Year",value.name="Value") %>%
  transmute(Year_Key=as.numeric(substr(Year,2,5)),
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],length(Year)),
            Indicator_Type_Key=rep(Dim_Context_Threat_CEP_C$Indicator_Type_Key,length(Year)),
            Indicator_Value=Value, 
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


ggplot(rbind.data.frame(Fact_Context_Threat_CEP_A,Fact_Context_Threat_CEP_B,Fact_Context_Threat_CEP_C),
       aes(x=Year_Key,y=Indicator_Value)) +
  geom_line(aes(group=Indicator_Type_Key,colour=Indicator_Type_Key)) +
  scale_colour_manual(values=c("brown","orange","blue"),
                      labels=c("Coal","Gas","Oil")) +
  scale_x_continuous(breaks=c(1980,1985,1990,1995,2000,2005,2010,2015),
                     expand=c(0,0)) +
  plot.theme + labs(x="Year",y="Quad BTU")


# ---- 2.3 Context - Response ----

# -- EMISSIONS GAP



# ---- 2.4 Consolidated CEP-specific Global Context tables ----


#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Global 2030 Outcomes ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 3.1 CEP Outcome 1 - MITIGATION ----

Dim_Global_WWF_2030_Outcome1_CEP <- 
  data.frame(Indicator_Type_Key="OUT1_CE_A",
             Indicator_Name="Global GHG emissions, including Land-Use Change and Forestry",
             Indicator_Label="Total GHG Emissions",
             Indicator_Subcategory=NA,
             Indicator_Unit="Gt CO2e",
             Data_source="CAIT Climate Data Explorer. 2017. Washington, DC: World Resources Institute",
             Indicator_Target=350,
             Indicator_Type="Outcome",
             Panel_Label="Mitigation",
             Display_Order=1)

Fact_Global_2030_Outcome1_CEP <-
  read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/CAIT Country GHG Emissions.csv') %>%
  subset(.,Country=="World", select=1:3) %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],length(Year)),
            Indicator_Type_Key=rep(Dim_Global_WWF_2030_Outcome1_CEP$Indicator_Type_Key, length(Year)),
            Indicator_Value=Total.GHG.Emissions.Excluding.Land.Use.Change.and.Forestry..MtCO2e./1000,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)

# Target value for this indicator is "at least 40% decrease in emissions from 2010 baseline", so we will calculate the target and 
# append the Dim table, using the CAIT data source imported for the Fact table

Dim_Global_WWF_2030_Outcome1_CEP$Indicator_Target <- 
  0.6*Fact_Global_2030_Outcome1_CEP$Indicator_Value[Fact_Global_2030_Outcome1_CEP$Year_Key==2010]


# ---- 3.2 CEP Outcome 2 - ENERGY ----

sdg.7.energy <- read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/SDG_7_energy_dl_2018_0827.csv')

# -- RENEWABLE ENERGY
Dim_Global_WWF_2030_Outcome2_CEP_A <- 
  data.frame(Indicator_Type_Key="OUT2_CE_A",
             Indicator_Name="Renewable energy share in the total final energy consumption",
             Indicator_Label="Renewable Energy",
             Indicator_Subcategory="A",
             Indicator_Unit="% of total energy consumption",
             Data_source="UN SDG Indicator Bank - World Bank analysis based on World Energy Statistics and Balances, IEA (2017); 
             Energy Balances, UN Statistics Division (2017)",
             Indicator_Target=40,
             Indicator_Type="Outcome",
             Panel_Label="Energy",
             Display_Order=2)

Fact_Global_2030_Outcome2_CEP_A <-
  sdg.7.energy[grepl("Renewable",sdg.7.energy$SeriesDescription,ignore.case=T)==T,] %>%
  transmute(Year_Key=TimePeriod,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],length(TimePeriod)),
            Indicator_Type_Key=rep(Dim_Global_WWF_2030_Outcome2_CEP_A$Indicator_Type_Key, length(TimePeriod)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)

# -- ENERGY INTENSITY
Dim_Global_WWF_2030_Outcome2_CEP_B <- 
  data.frame(Indicator_Type_Key="OUT2_CE_B",
             Indicator_Name="Energy intensity level of primary energy (megajoules per constant 2011 purchasing power parity GDP)",
             Indicator_Label="Energy Intensity",
             Indicator_Subcategory="B",
             Indicator_Unit="MJ/2011 USD PPP",
             Data_source="UN SDG Indicator Bank - World Bank analysis based on World Energy Statistics and Balances, IEA (2017); 
             Energy Balances, UN Statistics Division (2017)",
             Indicator_Target=0.5*sdg.7.energy$Value[grepl("Intensity",sdg.7.energy$SeriesDescription,ignore.case=T)==T &
                                                       sdg.7.energy$TimePeriod==2010],
             Indicator_Type="Outcome",
             Panel_Label="Energy",
             Display_Order=2)

Fact_Global_2030_Outcome2_CEP_B <-
  sdg.7.energy[grepl("Intensity",sdg.7.energy$SeriesDescription,ignore.case=T)==T,] %>%
  transmute(Year_Key=TimePeriod,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],length(TimePeriod)),
            Indicator_Type_Key=rep(Dim_Global_WWF_2030_Outcome2_CEP_A$Indicator_Type_Key, length(TimePeriod)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)

# -- UNIVERSAL ENERGY
Dim_Global_WWF_2030_Outcome2_CEP_C <- 
  data.frame(Indicator_Type_Key="OUT2_CE_C",
             Indicator_Name="Proportion of world population with access to electricity",
             Indicator_Label="Universal Energy",
             Indicator_Subcategory="C",
             Indicator_Unit="% of population with electricity access",
             Data_source="UN SDG Indicator Bank - Global Tracking Framework 2018",
             Indicator_Target=100,
             Indicator_Type="Outcome",
             Panel_Label="Energy",
             Display_Order=2)

Fact_Global_2030_Outcome2_CEP_C <-
  sdg.7.energy[grepl("Electricity",sdg.7.energy$SeriesDescription,ignore.case=T)==T &
                 sdg.7.energy$X.Location.=="ALLAREA",] %>%
  transmute(Year_Key=TimePeriod,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],length(TimePeriod)),
            Indicator_Type_Key=rep(Dim_Global_WWF_2030_Outcome2_CEP_C$Indicator_Type_Key, length(TimePeriod)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)

# -- COAL PHASE OUT 

# ---- 3.3 CEP Outcome 3 - ADAPTATION ----

Dim_Global_WWF_2030_Outcome3_CEP <- 
  data.frame(Indicator_Type_Key="OUT3_CE_A",
             Indicator_Name="Number of countries with a National Adaptation Plan, or equivalent (for developed countries)",
             Indicator_Label="National Adaptation Plans",
             Indicator_Subcategory=NA,
             Indicator_Unit="",
             Data_source="",
             Indicator_Target=100,
             Indicator_Type="Outcome",
             Panel_Label="Adaptation",
             Display_Order=3)

Fact_Global_2030_Outcome3_CEP <-
  read.csv() %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],length(Year)),
            Indicator_Type_Key=rep(Dim_Global_WWF_2030_Outcome3_CEP$Indicator_Type_Key, length(Year)),
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)

# ---- 3.4 Consolidated CEP-specific Global 2030 Outcome tables ----


