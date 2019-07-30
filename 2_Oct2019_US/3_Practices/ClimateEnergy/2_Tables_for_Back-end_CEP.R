# 
# code: Climate & Energy Practice Indicator and Initiative Tables FOR 2019 US DASHBOARD
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: August 2018
# modified: July 2019
# 
# ---- inputs ----
#  1) CEP-specific data tables (in 2_Oct2019_US/2_FlatDataFiles/ConsDB_Input_2019)
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

pacman::p_load(dplyr, xlsx, reshape2, ggplot2)


practice_key_ref <- read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/cons_dashboard_dim_tables_20180828.xlsx',
                              sheetName='Dim_Practice')

practice_outcome_key_ref <- read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/cons_dashboard_dim_tables_20180828.xlsx',
                                      sheetName='Dim_Practice_Outcome')


plot.theme <- theme(plot.title=element_text(hjust=0.5),
                    plot.margin=margin(t=5,r=20,b=5,l=5,unit="pt"),
                    axis.ticks=element_blank(),
                    panel.background=element_rect(fill="white",
                                                  colour="#909090"),
                    panel.border=element_rect(fill=NA,
                                              size=0.25,
                                              colour="#C0C0C0"),
                    panel.grid.major.x=element_line(colour="#C0C0C0",
                                                    size=0.25,
                                                    linetype=3),
                    panel.grid.major.y=element_blank(),
                    axis.title=element_text(size=rel(0.9),
                                            angle=0,
                                            face="bold",
                                            colour="#303030"),
                    axis.text=element_text(size=rel(0.9),
                                           angle=0,
                                           colour="#303030"),
                    legend.position="top",
                    legend.justification="right",
                    legend.box.spacing=unit(0.1,"cm"),
                    legend.title=element_blank())


#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Global Context ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 2.1 Context - State ----

Temp_land_sea <- 
  read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/Berkeley_land_sea_dl_2018_1026.csv')

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
            Indicator_Lower_Value=-Annual_Unc + Annual_Anomaly - Temp_scaling_factor)


# ggplot(Fact_Context_State_CEP_A, aes(x=Year_Key,y=Indicator_Value,
#                                      ymin=Indicator_Lower_Value,
#                                      ymax=Indicator_Upper_Value)) +
#   geom_ribbon(alpha=0.2) +
#   geom_line(colour="blue") +
#   scale_x_continuous(breaks=c(1850,1900,1950,1975,2000,2015),
#                      expand=c(0,0)) +
#   labs(x="Year", y="Average Annual Temperature (Celsius)") +
#   plot.theme


# ---- 2.2 Context - Threat ----

EIA_fossil_fuel <- 
  read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/US_EIA_fossil_fuel_consump_dl_2018_0830.xlsx',
                            sheetName='Data') %>%
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
            Indicator_Lower_Value=NA)

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
            Indicator_Lower_Value=NA)

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
            Indicator_Lower_Value=NA)


# ggplot(rbind.data.frame(Fact_Context_Threat_CEP_A,Fact_Context_Threat_CEP_B,Fact_Context_Threat_CEP_C),
#        aes(x=Year_Key,y=Indicator_Value)) +
#   geom_line(aes(group=Indicator_Type_Key,colour=Indicator_Type_Key)) +
#   scale_colour_manual(values=c("brown","orange","blue"),
#                       labels=c("Coal","Gas","Oil")) +
#   scale_x_continuous(breaks=c(1980,1985,1990,1995,2000,2005,2010,2015),
#                      expand=c(0,0)) +
#   plot.theme + labs(x="Year",y="% of total energy consumption")


# ---- 2.3 Context - Response ----

emissions.gap <- read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/UNEP_emissions_gap2017_2018_0908.xlsx',sheetName="Data")

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
            Indicator_Lower_Value=Lower)

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
            Indicator_Lower_Value=Lower)

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
            Indicator_Lower_Value=Lower)


# ggplot(rbind.data.frame(Fact_Context_Response_CEP_A,Fact_Context_Response_CEP_B,Fact_Context_Response_CEP_C),
#        aes(x=Year_Key,y=Indicator_Value, ymin=Indicator_Lower_Value, ymax=Indicator_Upper_Value)) +
#   geom_line(aes(group=Indicator_Type_Key,colour=Indicator_Type_Key)) +
#   geom_ribbon(aes(group=Indicator_Type_Key, fill=Indicator_Type_Key),
#               alpha=0.2) +
#   scale_colour_manual(values=c("brown","orange","blue"),
#                       labels=c("Current Policy","2 Degrees","1.5 Degrees")) +
#   scale_fill_manual(values=c("brown","orange","blue"),
#                       labels=c("Current Policy","2 Degrees","1.5 Degrees")) +
#   scale_x_continuous(breaks=c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025, 2030),
#                      expand=c(0,0)) +
#   plot.theme + labs(x="Year",y="Total emissions (Gt CO2e)")


# ---- 2.4 Consolidated CEP-specific Global Context tables ----

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
# ---- SECTION 3: Global 2030 Outcomes ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 3.1 CEP Outcome 1 - MITIGATION ----

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
  read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/CAIT Country GHG Emissions.csv') %>%
  subset(.,Country=="World" & Year>2009, select=c(1,2,4)) %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome1_CEP_A$Indicator_Type_Key, length(Year_Key)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Climate & Energy" &
                                                                   grepl("Mitigation",practice_outcome_key_ref$practice_outcome)], length(Year_Key)),
            Indicator_Value=Total.GHG.Emissions.Including.Land.Use.Change.and.Forestry..MtCO.e../1000,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)

# Target value for this indicator is "at least 40% decrease in emissions from 2010 baseline", so we will calculate the target and 
# append the Dim table, using the CAIT data source imported for the Fact table

Dim_Global_2030_Outcome1_CEP_A$Indicator_Target <- 
  0.6*Fact_Global_2030_Outcome1_CEP_A$Indicator_Value[Fact_Global_2030_Outcome1_CEP_A$Year_Key==2010]

# Add target value to Fact table

Fact_Global_2030_Outcome1_CEP_A <-
  rbind.data.frame(Fact_Global_2030_Outcome1_CEP_A,
                   data.frame(Year_Key=2030,
                              Practice_Key=practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],
                              Indicator_Type_Key=Dim_Global_2030_Outcome1_CEP_A$Indicator_Type_Key,
                              Practice_Outcome_Key=practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Climate & Energy" &
                                                                                 grepl("Mitigation",practice_outcome_key_ref$practice_outcome)],
                              Indicator_Value=Dim_Global_2030_Outcome1_CEP_A$Indicator_Target,
                              Indicator_Upper_Value=NA,
                              Indicator_Lower_Value=NA))


# ---- 3.2 CEP Outcome 2 - ENERGY ----

sdg.7.energy <- read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/SDG_7_energy_dl_2018_0827.csv')

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
            Indicator_Lower_Value=NA) %>%
  rbind.data.frame(.,data.frame(Year_Key=2030,
                                Practice_Key=practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],
                                Indicator_Type_Key=Dim_Global_2030_Outcome2_CEP_A$Indicator_Type_Key,
                                Practice_Outcome_Key=practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Climate & Energy" &
                                                                                   grepl("Energy",practice_outcome_key_ref$practice_outcome)],
                                Indicator_Value=Dim_Global_2030_Outcome2_CEP_A$Indicator_Target,
                                Indicator_Upper_Value=NA,
                                Indicator_Lower_Value=NA))

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
            Indicator_Lower_Value=NA) %>%
  rbind.data.frame(.,data.frame(Year_Key=2030,
                                Practice_Key=practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],
                                Indicator_Type_Key=Dim_Global_2030_Outcome2_CEP_B$Indicator_Type_Key,
                                Practice_Outcome_Key=practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Climate & Energy" &
                                                                                   grepl("Energy",practice_outcome_key_ref$practice_outcome)],
                                Indicator_Value=Dim_Global_2030_Outcome2_CEP_B$Indicator_Target,
                                Indicator_Upper_Value=NA,
                                Indicator_Lower_Value=NA))

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
            Indicator_Lower_Value=NA) %>%
  rbind.data.frame(.,data.frame(Year_Key=2030,
                                Practice_Key=practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],
                                Indicator_Type_Key=Dim_Global_2030_Outcome2_CEP_C$Indicator_Type_Key,
                                Practice_Outcome_Key=practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Climate & Energy" &
                                                                                   grepl("Energy",practice_outcome_key_ref$practice_outcome)],
                                Indicator_Value=Dim_Global_2030_Outcome2_CEP_C$Indicator_Target,
                                Indicator_Upper_Value=NA,
                                Indicator_Lower_Value=NA))

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
  read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/CoalPlantTracker_pipeline_2018_0906.xlsx',sheetName='Data') %>%
  subset(.,Pipeline=="All Pipeline") %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome2_CEP_D$Indicator_Type_Key, length(Year_Key)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Climate & Energy" &
                                                                   grepl("Energy",practice_outcome_key_ref$practice_outcome)], length(Year_Key)),
            Indicator_Value=Capacity,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA) %>%
  rbind.data.frame(.,data.frame(Year_Key=2030,
                                Practice_Key=practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],
                                Indicator_Type_Key=Dim_Global_2030_Outcome2_CEP_D$Indicator_Type_Key,
                                Practice_Outcome_Key=practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Climate & Energy" &
                                                                                   grepl("Energy",practice_outcome_key_ref$practice_outcome)],
                                Indicator_Value=Dim_Global_2030_Outcome2_CEP_D$Indicator_Target,
                                Indicator_Upper_Value=NA,
                                Indicator_Lower_Value=NA))


# ---- 3.3 CEP Outcome 3 - ADAPTATION ----

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
  read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/NAPCentral_countrylists_2018_0906.xlsx',sheetName='Sheet1') %>%
  group_by(Year) %>%
  summarise(NumCountries=length(Country)) %>%
  transmute(Year_Key=Year,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],length(Year_Key)),
            Indicator_Type_Key=rep(Dim_Global_2030_Outcome3_CEP_A$Indicator_Type_Key, length(Year_Key)),
            Practice_Outcome_Key=rep(practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Climate & Energy" &
                                                                   grepl("Adaptation",practice_outcome_key_ref$practice_outcome)], length(Year_Key)),
            Indicator_Value=cumsum(NumCountries),
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA) %>%
  rbind.data.frame(.,data.frame(Year_Key=2030,
                                Practice_Key=practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],
                                Indicator_Type_Key=Dim_Global_2030_Outcome3_CEP_A$Indicator_Type_Key,
                                Practice_Outcome_Key=practice_outcome_key_ref$id[practice_outcome_key_ref$practice_name=="Climate & Energy" &
                                                                                   grepl("Adaptation",practice_outcome_key_ref$practice_outcome)],
                                Indicator_Value=Dim_Global_2030_Outcome3_CEP_A$Indicator_Target,
                                Indicator_Upper_Value=NA,
                                Indicator_Lower_Value=NA))


# ---- 3.4 Consolidated CEP-specific Global 2030 Outcome tables ----

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
# ---- SECTION 4: Initiatives ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 4.1 Load data ----

dim.initiatives.CEP <- 
  read.xlsx('2_Oct2019_US/2_FlatDataFiles/ConsDB_Input_2019/fy19_initiative_reporting_dim_2019_0703.xlsx',sheetName="Sheet1") %>%
  subset(.,Practice=="Climate & Energy") 

dim.initiative.indicators.CEP <-
  read.xlsx('2_Oct2019_US/2_FlatDataFiles/ConsDB_Input_2019/fy19_initiative_indicators_dim_2019_0703.xlsx',sheetName="Sheet1") %>%
  subset(.,Practice=="Climate & Energy")

fact.initiative.indicators.CEP <-
  read.xlsx('2_Oct2019_US/2_FlatDataFiles/ConsDB_Input_2019/fy19_initiative_indicators_fact_2019_0703.xlsx',sheetName="Sheet1") %>%
  left_join(.,dim.initiatives.CEP[,c("Initiative.key","Practice")], by="Initiative.key") %>%
  subset(.,Practice=="Climate & Energy")

dim.initiative.milestones.CEP <-
  read.xlsx('2_Oct2019_US/2_FlatDataFiles/ConsDB_Input_2019/fy19_initiative_milestones_2019_0703.xlsx',sheetName="Sheet1") %>%
  subset(.,Practice=="Climate & Energy")


# ---- 4.2 CEP-specific Dim_Initiative ----

Dim_Initiative_CEP <-
  dim.initiatives.CEP %>%
  transmute(Initiative_Key=Initiative.key,
            Initiative_Name=Initiative,
            Initiative_Status=Overall.status,
            Initiative_Status_Justification=Overall.just,
            Initiative_Goal=Initiative.statement,
            Global_Initiative=Global.initiative,
            US_Initiative=US.initiative,
            Display_Order=Display.order)


# ---- 4.3 CEP-specific Dim_Initiative_Indicator_Type ----

Dim_Initiative_Indicator_CEP <-
  dim.initiative.indicators.CEP %>%
  transmute(Indicator_Type_Key=Initiative.indicator.key,
            Indicator_Type=Indicator.type,
            Indicator_Name=ifelse(!is.na(Indicator.name),as.character(Indicator.name),"FORTHCOMING"),
            Indicator_Label=ifelse(!is.na(Indicator.label),as.character(Indicator.label),"Not Yet Identified"),
            Indicator_Subcategory=Subcategory,
            Indicator_Unit=Units,
            Data_Source=Source,
            Indicator_Target=Target,
            Display_Order=Display.order,
            Indicator_Statement=Statement)

# ---- 4.4 CEP-specific Fact_Initiative_Indicators ----

Fact_Initiative_Indicator_CEP <-
  fact.initiative.indicators.CEP %>%
  transmute(Year_Key=ifelse(!is.na(Year),Year,9999),
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],length(Year_Key)),
            Initiative_Key=Initiative.key,
            Indicator_Type_Key=Initiative.indicator.key,
            Indicator_Value=Value,
            Indicator_Upper_Value=NA,
            Indicator_Lower_Value=NA)


# ---- 4.5 CEP-specific Fact_Initiative_Financials ----

Fact_Initiative_Financials_CEP <-
  dim.initiatives.CEP %>%
  transmute(Date_Key=Date,
            Practice_Key=rep(practice_key_ref$id[practice_key_ref$practice_name=="Climate & Energy"],length(Date_Key)),
            Initiative_Key=Initiative.key,
            Amount_Needed=Funds.needed,
            Amount_Secured=Funds.secured,
            Amount_Anticipated=Funds.anticipated)


# ---- 4.6 CEP-specific Milestone_Group_Bridge ----

Milestone_Group_Bridge_CEP <-
  left_join(dim.initiative.milestones.CEP, dim.initiatives.CEP, by=c("Initiative", "Practice")) %>%
  transmute(Milestone_Key=Milestone.key,
            Initiative_Key=Initiative.key)


# ---- 4.7 CEP-specific Dim_Milestone ----

Dim_Milestone_CEP <-
  dim.initiative.milestones.CEP %>%
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
   dim.initiatives.CEP,
   dim.initiative.indicators.CEP,
   fact.initiative.indicators.CEP,
   dim.initiative.milestones.CEP)
