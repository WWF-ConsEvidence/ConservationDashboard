
# code: global script for initiative reporting app


# ---- load libraries ----

library(rio)
library(rdrop2)
library(rsconnect)
library(bit64)
library(tidyr)
library(stringr)
library(dplyr)
library(shiny)
library(shinyalert)
library(shinythemes)
library(shinyjs)
library(shinyBS)
library(shinycssloaders)
library(DT)


# ---- save dropbox token for remote access on shiny.io ----

token <- readRDS("droptoken.rds")


# ---- define fields to be saved from form ----

initiative_dim_fields <- c("goal","initiative","initiativelead","email","initiativestart","initiativeend",
                           "initiativestatement","initiativestatus","initiativejust","fundsneeded","fundssecured","fundsanticipated")

init_indicator_dim_fields <- c("goal","initiative","statement","indicatordescription","indicatorlabel","indicatorunits",
                               "indicatorsource","subcat","subcattarget","indicatortype","displayorder","indicatorkey",
                               "desiredtrend","indicatorlabelabbr","subcatlabelabbr")

out1_subcat1_indicator_dim_fields <- c("goal","initiative","outcome1statement","out1indicator","out1indicatorlabel",
                                       "out1indicatorunits","out1indicatorsource","out1subcat1","out1subcat1target")
out1_subcat2_indicator_dim_fields <- c("goal","initiative","outcome1statement","out1indicator","out1indicatorlabel",
                                       "out1indicatorunits","out1indicatorsource","out1subcat2","out1subcat2target")
out1_subcat3_indicator_dim_fields <- c("goal","initiative","outcome1statement","out1indicator","out1indicatorlabel",
                                       "out1indicatorunits","out1indicatorsource","out1subcat3","out1subcat3target")
out2_subcat1_indicator_dim_fields <- c("goal","initiative","outcome2statement","out2indicator","out2indicatorlabel",
                                       "out2indicatorunits","out2indicatorsource","out2subcat1","out2subcat1target")
out2_subcat2_indicator_dim_fields <- c("goal","initiative","outcome2statement","out2indicator","out2indicatorlabel",
                                       "out2indicatorunits","out2indicatorsource","out2subcat2","out2subcat2target")
out2_subcat3_indicator_dim_fields <- c("goal","initiative","outcome2statement","out2indicator","out2indicatorlabel",
                                       "out2indicatorunits","out2indicatorsource","out2subcat3","out2subcat3target")
path1_subcat1_indicator_dim_fields <- c("goal","initiative","pathway1statement","path1indicator","path1indicatorlabel",
                                        "path1indicatorunits","path1indicatorsource","path1subcat1","path1subcat1target")
path1_subcat2_indicator_dim_fields <- c("goal","initiative","pathway1statement","path1indicator","path1indicatorlabel",
                                        "path1indicatorunits","path1indicatorsource","path1subcat2","path1subcat2target")
path1_subcat3_indicator_dim_fields <- c("goal","initiative","pathway1statement","path1indicator","path1indicatorlabel",
                                        "path1indicatorunits","path1indicatorsource","path1subcat3","path1subcat3target")
path2_subcat1_indicator_dim_fields <- c("goal","initiative","pathway2statement","path2indicator","path2indicatorlabel",
                                        "path2indicatorunits","path2indicatorsource","path2subcat1","path2subcat1target")
path2_subcat2_indicator_dim_fields <- c("goal","initiative","pathway2statement","path2indicator","path2indicatorlabel",
                                        "path2indicatorunits","path2indicatorsource","path2subcat2","path2subcat2target")
path2_subcat3_indicator_dim_fields <- c("goal","initiative","pathway2statement","path2indicator","path2indicatorlabel",
                                        "path2indicatorunits","path2indicatorsource","path2subcat3","path2subcat3target")

# ---- source images for added guidance ----

Dashboard_intro <- base64enc::dataURI(file="guidance/Dashboard_intro.PNG", mime="image/png")

Outcome1_detailintro <- base64enc::dataURI(file="guidance/Outcome1_detailintro.PNG", mime="image/png")
Outcome1_labels <- base64enc::dataURI(file="guidance/Outcome1_labels.PNG", mime="image/png")
Outcome1_trends <- base64enc::dataURI(file="guidance/Outcome1_trends.PNG", mime="image/png")

Outcome2_detailintro <- base64enc::dataURI(file="guidance/Outcome2_detailintro.PNG", mime="image/png")
Outcome2_labels <- base64enc::dataURI(file="guidance/Outcome2_labels.PNG", mime="image/png")
Outcome2_trends <- base64enc::dataURI(file="guidance/Outcome2_trends.PNG", mime="image/png")

Pathway1_detailintro <- base64enc::dataURI(file="guidance/Pathway1_detailintro.PNG", mime="image/png")
Pathway1_labels <- base64enc::dataURI(file="guidance/Pathway1_labels.PNG", mime="image/png")
Pathway1_trends <- base64enc::dataURI(file="guidance/Pathway1_trends.PNG", mime="image/png")

Pathway2_detailintro <- base64enc::dataURI(file="guidance/Pathway2_detailintro.PNG", mime="image/png")
Pathway2_labels <- base64enc::dataURI(file="guidance/Pathway2_labels.PNG", mime="image/png")
Pathway2_trends <- base64enc::dataURI(file="guidance/Pathway2_trends.PNG", mime="image/png")
