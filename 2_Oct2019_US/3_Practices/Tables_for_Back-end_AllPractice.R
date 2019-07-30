# 
# code: All Practices: Back-end Tables FOR 2019 US DASHBOARD
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: August 2018
# modified: July 2019
# 
# ---- inputs ----
#  1) 2_Tables_for_Back-end_CEP.R
#  2) 2_Tables_for_Back-end_Food.R
#  3) 2_Tables_for_Back-end_Forest.R
#  4) 2_Tables_for_Back-end_FreshwaterR
#  5) 2_Tables_for_Back-end_Oceans.R
#  6) 2_Tables_for_Back-end_Wildlife.R
#  7) 2_Tables_for_Back-end_Finance.R
#  8) 2_Tables_for_Back-end_Governance.R
#  9) 2_Tables_for_Back-end_Markets.R
# 
# ---- outputs ----
#  1) Consolidated back-end tables -- ready to go into back-end:
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

# ---- 1.1 Dim_Context_Indicator_Type ----

Dim_Context_Indicator_Type <-
  rbind.data.frame(Dim_Context_CEP,
                   Dim_Context_Food,
                   Dim_Context_Forest,
                   Dim_Context_FW,
                   Dim_Context_Oceans,
                   Dim_Context_Wildlife)


# ---- 1.2 Fact_Global_Context_Indicators ----

Fact_Global_Context_Indicators <-
  rbind.data.frame(Fact_Context_CEP,
                   Fact_Context_Food,
                   Fact_Context_Forest,
                   Fact_Context_FW,
                   Fact_Context_Oceans,
                   Fact_Context_Wildlife)


# ---- 1.3 Export Global Context Files ----

file.name <- paste("2_Oct2019_US/2_FlatDataFiles/ConsDB_Output_2019/Tables_Back-end_produced_", format(Sys.Date(),format="%Y_%m%d"), ".xlsx",sep="")

write.xlsx(Dim_Context_Indicator_Type,file.name,sheetName="Dim_Context_Indicator_Type", row.names=F, showNA=F)
write.xlsx(Fact_Global_Context_Indicators,file.name,sheetName="Fact_Global_Context_Indicators", append=T, row.names=F, showNA=F)


#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Global 2030 Outcomes ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 2.1 Dim_Global_WWF_2030_Indicator_Type ----

Dim_Global_WWF_2030_Indicator_Type <-
  rbind.data.frame(Dim_Global_2030_Outcome_CEP,
                   Dim_Global_2030_Outcome_Food,
                   Dim_Global_2030_Outcome_Forest,
                   Dim_Global_2030_Outcome_FW,
                   Dim_Global_2030_Outcome_Oceans,
                   Dim_Global_2030_Outcome_Wildlife)


# ---- 2.2 Fact_Global_2030_Outcomes ----

Fact_Global_2030_Outcomes <-
  rbind.data.frame(Fact_Global_2030_Outcome_CEP,
                   Fact_Global_2030_Outcome_Food,
                   Fact_Global_2030_Outcome_Forest,
                   Fact_Global_2030_Outcome_FW,
                   Fact_Global_2030_Outcome_Oceans,
                   Fact_Global_2030_Outcome_Wildlife)


# ---- 2.3 Export Global 2030 Outcomes Files ----

write.xlsx(Dim_Global_WWF_2030_Indicator_Type,file.name,sheetName="Dim_Global_WWF_2030_Indicator_Type", append=T, row.names=F, showNA=F)
write.xlsx(Fact_Global_2030_Outcomes,file.name,sheetName="Fact_Global_2030_Outcomes", append=T, row.names=F, showNA=F)


#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 3: Initiatives ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#

# ---- 3.1 Dim_Initiative ----

Dim_Initiative <-
  rbind.data.frame(Dim_Initiative_Food,
                   Dim_Initiative_Forest,
                   Dim_Initiative_FW)


# ---- 3.2 Dim_Initiative_Indicator_Type ----

Dim_Initiative_Indicator_Type <-
  rbind.data.frame(Dim_Initiative_Indicator_Food,
                   Dim_Initiative_Indicator_Forest,
                   Dim_Initiative_Indicator_FW)


# ---- 3.3 Fact_Initiative_Indicators ----

Fact_Initiative_Indicators <-
  rbind.data.frame(Fact_Initiative_Indicator_Food,
                   Fact_Initiative_Indicator_Forest,
                   Fact_Initiative_Indicator_FW)


# ---- 3.4 Fact_Initiative_Financials ----

Fact_Initiative_Financials <-
  rbind.data.frame(Fact_Initiative_Financials_Food,
                   Fact_Initiative_Financials_Forest,
                   Fact_Initiative_Financials_FW)


# ---- 3.5 Milestone_Group_Bridge ----

Milestone_Group_Bridge <-
  rbind.data.frame(Milestone_Group_Bridge_Food,
                   Milestone_Group_Bridge_Forest,
                   Milestone_Group_Bridge_FW)


# ---- 3.6 Dim_Milestone ----

Dim_Milestone <-
  rbind.data.frame(Dim_Milestone_Food,
                   Dim_Milestone_Forest,
                   Dim_Milestone_FW)


# ---- 3.7 Export Initiative Files ----

write.xlsx(Dim_Initiative,file.name,sheetName="Dim_Initiative", append=T, row.names=F, showNA=F)
write.xlsx(Dim_Initiative_Indicator_Type,file.name,sheetName="Dim_Initiative_Indicator_Type", append=T, row.names=F, showNA=F)
write.xlsx(Fact_Initiative_Indicators,file.name,sheetName="Fact_Initiative_Indicators", append=T, row.names=F, showNA=F)
write.xlsx(Fact_Initiative_Financials,file.name,sheetName="Fact_Initiative_Financials", append=T, row.names=F, showNA=F)
write.xlsx(Milestone_Group_Bridge,file.name,sheetName="Milestone_Group_Bridge", append=T, row.names=F, showNA=F)
write.xlsx(Dim_Milestone,file.name,sheetName="Dim_Milestone", append=T, row.names=F, showNA=F)
