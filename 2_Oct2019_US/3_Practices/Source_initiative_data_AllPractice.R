# 
# code: Source initiative data from Excel
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: August 2019
# 
# ---- inputs ----
#  1) Initiative reporting Excel tables (x4)
# 
# 
# ---- code sections ----
#  1) Load libraries, add reference tables, source data
#  2) Calculate added initiative info
# 
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: Load libraries, add reference tables, source data ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#


# ---- 1.1 Libraries, reference tables ----

pacman::p_load(rio, stringi, xlsx, reshape2, ggplot2, dplyr)


practice_key_ref <- read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/cons_dashboard_dim_tables_20180828.xlsx',
                              sheetName='Dim_Practice')

practice_outcome_key_ref <- read.xlsx('1_Nov2018/2_FlatDataFiles/ConsDB_Input/cons_dashboard_dim_tables_20180828.xlsx',
                                      sheetName='Dim_Practice_Outcome')


# ---- 1.2 Initiative data ----

dim.initiatives <- 
  import('2_Oct2019_US/2_FlatDataFiles/ConsDB_Input_2019/fy19_initiative_reporting_dim_2019_0715.xlsx')

dim.initiative.indicators <-
  import('2_Oct2019_US/2_FlatDataFiles/ConsDB_Input_2019/fy19_initiative_indicators_dim_2019_0715.xlsx') %>%
  mutate(new.key=ifelse(grepl("OUT_",Initiative.indicator.key)==T,as.numeric(paste("10",stri_extract_last_regex(Initiative.indicator.key, "\\d{4}"),sep="")),
                        ifelse(grepl("PATH_",Initiative.indicator.key)==T,as.numeric(paste("20",stri_extract_last_regex(Initiative.indicator.key, "\\d{4}"),sep="")),
                               NA)))

fact.initiative.indicators <-
  import('2_Oct2019_US/2_FlatDataFiles/ConsDB_Input_2019/fy19_initiative_indicators_fact_2019_0715.xlsx') %>%
  left_join(.,dim.initiatives[,c("Initiative.key","Practice")], by="Initiative.key")

dim.initiative.milestones <-
  import('2_Oct2019_US/2_FlatDataFiles/ConsDB_Input_2019/fy19_initiative_milestones_2019_0715.xlsx')


#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Calculate added initiative info ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
left_join(fact.initiative.indicators,dim.initiative.indicators[,c("Initiative.indicator.key","Target")],by="Initiative.indicator.key") %>% group_by(Initiative.indicator.key) %>%
  summarise(Target=unique(Target),
            target.year=max(Year))

# ---- 2.1 Calculate pie.type info ---- 

pie.type <-
  left_join(fact.initiative.indicators,dim.initiative.indicators[,c("Initiative.indicator.key","Target")],by="Initiative.indicator.key") %>%
  group_by(Initiative.indicator.key,Initiative.key,Practice) %>%
  summarise(has.data=ifelse(length(Year[!is.na(Value)])>0, "Yes", "No"),
            max.year=ifelse(has.data=="Yes", max(Year[!is.na(Value)]), NA),
            min.year=ifelse(has.data=="Yes", min(Year[!is.na(Value)]), NA),
            max.year.value=ifelse(has.data=="Yes", Value[Year==max.year], NA),
            min.year.value=ifelse(has.data=="Yes", Value[Year==min.year], NA),
            target.year=ifelse(!is.na(unique(Target)),max(Year),NA),
            trending=ifelse(has.data=="Yes", ifelse(max.year.value>min.year.value, 1, 
                                                    ifelse(max.year.value<min.year.value, -1, 
                                                           ifelse(max.year.value==min.year.value, 0, NA))),
                            NA)) %>%
  left_join(dim.initiative.indicators[,c("Initiative.indicator.key","Target","Desired.trend")], by="Initiative.indicator.key") %>%
  mutate(Target=as.numeric(Target),
         good.bad.trend=ifelse((Desired.trend=="Up" & (trending==1 | trending==0)) |
                                 (Desired.trend=="Down" & (trending==-1 | trending==0)), "Good",
                               ifelse((Desired.trend=="Up" & trending==-1) |
                                        (Desired.trend=="Down" & trending==1), "Bad", NA)),
         pie.type=ifelse(has.data=="No" & !is.na(Target), "No Data - Yes Target",
                         ifelse(has.data=="Yes" & is.na(Target), "Yes Data - No Target",
                                ifelse(has.data=="No" & is.na(Target), "No Data - No Target",
                                       ifelse(has.data=="Yes" & !is.na(Target) & good.bad.trend=="Good", "Yes Data - Yes Target - Good Trend",
                                              ifelse(has.data=="Yes" & !is.na(Target) & good.bad.trend=="Bad", "Yes Data - Yes Target - Bad Trend", NA))))),
         amount.achieved=ifelse(max.year.value==0 & (is.na(Target) | Target==0), 1,
                                ifelse(max.year.value==0 & !is.na(Target) & Target!=0, 0,
                                ifelse((pie.type=="Yes Data - Yes Target - Good Trend" |
                                          pie.type=="Yes Data - Yes Target - Bad Trend") & Desired.trend=="Up" & max.year.value!=0, 
                                       (max.year.value-min.year.value)/(Target-min.year.value),
                                       ifelse((pie.type=="Yes Data - Yes Target - Good Trend" |
                                                 pie.type=="Yes Data - Yes Target - Bad Trend") & Desired.trend=="Down" & max.year.value!=0, 
                                              (min.year.value-max.year.value)/(min.year.value-Target),
                                              ifelse(pie.type=="Yes Data - No Target", max.year.value, NA))))),
         amount.achieved=ifelse(amount.achieved>=1,1,amount.achieved),
         amount.remaining=ifelse(pie.type=="No Data - Yes Target", 1, 
                                 ifelse(pie.type=="Yes Data - No Target", NA,
                                        ifelse(!is.na(amount.achieved),1-abs(amount.achieved),NA))))

