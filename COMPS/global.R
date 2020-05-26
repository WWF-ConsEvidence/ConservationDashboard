

# ---- load libraries ----

pacman::p_load(rio, bit64, dplyr, shiny, shinyBS, DT)


# ---- call data frames ----

milestones <- data.frame("milestone"=c("A","B","C","D"),
                        "date" =c(11/2019, 12/2020, 06/2021, 03/2020),
                        "status"= c("Barrier","Barrier","Progress","Support"),
                        "justification"=c("xyz", "abc", "def", "ghi"))


# ---- define fields to be saved from form ----

initiative_dim_fields <- c("goal","initiative","initiativelead","email","initiativestart","initiativeend",
                           "initiativestatement","initiativestatus","initiativejust","fundsneeded","fundssecured","fundsanticipated")

init_indicator_dim_fields <- c("goal","initiative","statement","indicatordescription","indicatorlabel","indicatorlabelabbr","indicatorunits",
                               "indicatorsource","subcat","subcattarget","indicatortype","displayorder","indicatorkey")

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

