# 
# code: Source initiative data from Excel
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: August 2019
# modified: June 2020
# 
# ---- inputs ----
#  1) Initiative reporting csv files (x4)
#  2) Practice reference table
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


# ***STOP! IMPORTANT!***
# 
# Before running this script, make sure that you've copied the responses from the four .csvs from the initiative reporting app and saved them into a new folder in 
# 'COMPS/Dashboard_back_end/x_Flat_data_files/Input/x_Reporting'.  Name the folder by the current date (in the format [YYYYMMDD]), and remove any dates/reporting years/etc. from the file names.  
# 
# You'll end up with four files in your new folder, named exactly:
# (1) Initiative_dim.csv
# (2) Init_indicator_dim.csv
# (3) Init_indicator_fact.csv
# (4) Milestones.csv

# After copying these files into the new folder, you must manually calculate a few columns:
# 
# (1) "desiredtrend" in Init_indicator_dim.csv.  This column indicates whether the indicator is meant to be trending "Up" or "Down" (most indicators trend "Up").  
#     Check older time stamps of the same indicators -- as long as the text of the indicator has not changed, the desired trend will remain the same.
#     E.g., an indicator that has units of "% change since baseline" will have a desired trend of "Up", since the baseline value would be 0 and the target would be >0.  
# 
# (2) "indicatorlabelabbr" in Init_indicator_dim.csv.  This column provides an abbreviated version of the label for the overview page of the Dashboard.  
#      Check older time stamps of the same indicators for previous abbreviations -- as long as the text of the indicator label has not changed, the abbreviated label has not changed.
# 
# (3) "subcatlabelabbr" in Init_indicator_dim.csv.  This column also provides an abbreviated version of the subcategory.  Check older time stamps for this as well.
# 


# ---- 1.1 Libraries, functions, reference tables ----

pacman::p_load(rio, stringi, xlsx, reshape2, ggplot2, dplyr)


# define last.file function, for pulling files of external data
last.file <- function(dir.nam,nam){
  paste0(dir.nam,last(sort(grep(nam,list.files(dir.nam), value=T))))}


# define directory name for each goal/Practice team external data files
dir.nam.CEP <- 'COMPS/Dashboard_back_end/x_Flat_data_files/Input/ClimateEnergy/'
dir.nam.Food <- 'COMPS/Dashboard_back_end/x_Flat_data_files/Input/Food/'
dir.nam.Forest <- 'COMPS/Dashboard_back_end/x_Flat_data_files/Input/Forest/'
dir.nam.Freshwater <- 'COMPS/Dashboard_back_end/x_Flat_data_files/Input/Freshwater/'
dir.nam.Oceans <- 'COMPS/Dashboard_back_end/x_Flat_data_files/Input/Oceans/'
dir.nam.Wildlife <- 'COMPS/Dashboard_back_end/x_Flat_data_files/Input/Wildlife/'


# reference tables
practice_key_ref <- import('COMPS/Dashboard_back_end/x_Flat_data_files/Input/cons_dashboard_dim_tables_20180828.xlsx',
                              sheet='Dim_Practice')

practice_outcome_key_ref <- import('COMPS/Dashboard_back_end/x_Flat_data_files/Input/cons_dashboard_dim_tables_20180828.xlsx',
                                      sheet='Dim_Practice_Outcome')


# ---- 1.2 Initiative data ----

latest_folder <- suppressWarnings(paste('COMPS/Dashboard_back_end/x_Flat_data_files/Input/x_Reporting/',
                                      max(as.numeric(list.files('COMPS/Dashboard_back_end/x_Flat_data_files/Input/x_Reporting')), na.rm=T), 
                                      sep = ""))

dim_initiatives <- 
  import(paste(latest_folder,"Initiative_dim.csv", sep = "/")) %>% 
  group_by(initiativekey) %>%
  slice(which.max(timestamp)) %>%
  ungroup() %>%
  mutate(date = substr(as.numeric(timestamp), 1, 8))


dim_initiative_indicators <-
  import(paste(latest_folder,"Init_indicator_dim.csv", sep = "/")) %>%
  filter(!is.na(indicatorlabel) & indicatorlabel!="") %>%
  group_by(initiative, displayorder) %>%
  mutate(subcatnum = row_number(),
         num.na.subcat = length(subcat[is.na(subcat) | subcat==""])) %>%
  ungroup() %>%
  filter(num.na.subcat==0 | (num.na.subcat%in%c(2,3) & subcatnum==1) | (num.na.subcat==1 & subcatnum%in%c(1,2)))


fact_initiative_indicators <-
  import(paste(latest_folder,"Init_indicator_fact.csv", sep = "/")) %>%
  left_join(.,dim_initiatives[,c("goal","initiative")], by="initiative") %>% 
  left_join(.,dim_initiative_indicators[,c("indicatorkey","statement", "subcattarget")], by="indicatorkey") %>%
  group_by(indicatorkey) %>%
  mutate(targetyear=max(Year),
         lengthdata=length(Value[!is.na(Value)]),
         is.targetyear=ifelse(Year==targetyear,"yes","no"),
         # below, we identify rows that need to be removed, to ensure the proper visualization in Tableau:
         # the rows identified in remove.data1 contain the target year, blank value, and there is no target identified 
         # (when no target is identified, there is no need to have a blank placeholder row for target year - it messes up the centering of the data on the Tableau plots)
         remove.data1=ifelse(is.targetyear=="yes" & !is.na(is.targetyear) & is.na(subcattarget), "yes", "no"),
         # the rows identified in remove.data2 have a blank year/value, but otherwise have data - we need to maintain a dummy row for those
         # indicators that do not have data yet, but we do not need the extra row of an NA year/value when we already have other data
         # (this is likely an artifact from initiative reporting app input)
         remove.data2=ifelse(is.na(Year) & lengthdata>0, "yes", "no")) %>%
  ungroup() %>%
  filter(!is.na(statement) & remove.data1=="no" & remove.data2=="no") %>%
  select(-subcattarget,-targetyear,-lengthdata,-is.targetyear,-remove.data1,-remove.data2)



dim_initiative_milestones <-
  import(paste(latest_folder,"Milestones.csv", sep = "/")) %>% 
  left_join(.,dim_initiatives[,c("initiativekey","initiative")], by="initiative") %>%
  mutate(target = gsub("m","",target))


#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: Calculate added initiative info ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#



# ---- 2.1 Calculate pie.type info ---- 

pie_type <-
  left_join(fact_initiative_indicators,dim_initiative_indicators[,c("indicatorkey","subcattarget")],by="indicatorkey") %>%
  group_by(indicatorkey,initiativekey,goal) %>%
  summarise(has.data=ifelse(length(Year[!is.na(Value)])>0, "Yes", "No"),
            max.year=ifelse(has.data=="Yes", max(Year[!is.na(Value)]), NA),
            min.year=ifelse(has.data=="Yes", min(Year[!is.na(Value)]), NA),
            max.year.value=ifelse(has.data=="Yes", Value[Year==max.year], NA),
            min.year.value=ifelse(has.data=="Yes", Value[Year==min.year], NA),
            target.year=ifelse(!is.na(unique(subcattarget)),max(Year),NA),
            trending=ifelse(has.data=="Yes", ifelse(max.year.value>min.year.value, 1, 
                                                    ifelse(max.year.value<min.year.value, -1, 
                                                           ifelse(max.year.value==min.year.value, 0, NA))),
                            NA)) %>%
  left_join(dim_initiative_indicators[,c("indicatorkey","subcattarget","desiredtrend")], by="indicatorkey") %>%
  mutate(subcattarget=as.numeric(subcattarget),
         good.bad.trend=ifelse((desiredtrend=="Up" & (trending==1 | trending==0)) |
                                 (desiredtrend=="Down" & (trending==-1 | trending==0)), "Good",
                               ifelse((desiredtrend=="Up" & trending==-1) |
                                        (desiredtrend=="Down" & trending==1), "Bad", NA)),
         pie.type=ifelse(has.data=="No" & !is.na(subcattarget), "No Data - Yes Target",
                         ifelse(has.data=="Yes" & is.na(subcattarget), "Yes Data - No Target",
                                ifelse(has.data=="No" & is.na(subcattarget), "No Data - No Target",
                                       ifelse(has.data=="Yes" & !is.na(subcattarget) & good.bad.trend=="Good", "Yes Data - Yes Target - Good Trend",
                                              ifelse(has.data=="Yes" & !is.na(subcattarget) & good.bad.trend=="Bad", "Yes Data - Yes Target - Bad Trend", NA))))),
         amount.achieved=ifelse((max.year.value==0 & (is.na(subcattarget) | subcattarget==0)) | (max.year.value==subcattarget & !is.na(subcattarget)), 1,
                                ifelse(max.year.value==0 & !is.na(subcattarget) & subcattarget!=0, 0,
                                  ifelse(initiativekey=="i0027" & pie.type=="Yes Data - Yes Target - Good Trend", max.year.value/subcattarget, #THIS IS SPECIFICALLY FOR TIGERS INITIATIVE -- wanted pies to reflect starting value of 0, but our data do not include baseline data, nor data from when indicator was 0
                                    ifelse((pie.type=="Yes Data - Yes Target - Good Trend" |
                                            pie.type=="Yes Data - Yes Target - Bad Trend") & desiredtrend=="Up" & max.year.value!=0, 
                                       (max.year.value-min.year.value)/(subcattarget-min.year.value),
                                       ifelse((pie.type=="Yes Data - Yes Target - Good Trend" |
                                                 pie.type=="Yes Data - Yes Target - Bad Trend") & desiredtrend=="Down" & max.year.value!=0, 
                                              (min.year.value-max.year.value)/(min.year.value-subcattarget),
                                              ifelse(pie.type=="Yes Data - No Target", max.year.value, NA)))))),
         amount.achieved=ifelse(amount.achieved>=1,1,amount.achieved),
         amount.remaining=ifelse(pie.type=="No Data - Yes Target", 1, 
                                 ifelse(pie.type=="Yes Data - No Target", NA,
                                        ifelse(!is.na(amount.achieved),1-abs(amount.achieved),NA))))

