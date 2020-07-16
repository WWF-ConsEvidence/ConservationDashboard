# 
# code: form data for saving data to Dropbox
# 
# sourced in: COMPS/Initiative_reporting_app/app.R
# 


# initiative dim data
formData1 <- reactive({
  initiative_dim_data <- data.frame(t(as.matrix(sapply(initiative_dim_fields, function(x) input[[x]])))) %>%
    mutate(securedanticipatedsum = input$fundssecured+input$fundsanticipated,
           initiativekey = as.character(unique(initiative_dim$initiativekey[initiative_dim$initiative==input$initiativeoptions])),
           displayorder = unique(initiative_dim$displayorder[initiative_dim$initiative==input$initiativeoptions]),
           globalinitiative = unique(initiative_dim$globalinitiative[initiative_dim$initiative==input$initiativeoptions]),
           usinitiative = unique(initiative_dim$usinitiative[initiative_dim$initiative==input$initiativeoptions]))
  initiative_dim_data
})


# initiative indicator dim data
formData2 <- reactive({
  init_indicator_dim_data <- rbind.data.frame(as.data.frame(t(sapply(out1_subcat1_indicator_dim_fields, function(x) input[[x]], USE.NAMES = F))),
                                              as.data.frame(t(sapply(out1_subcat2_indicator_dim_fields, function(x) input[[x]], USE.NAMES = F))),
                                              as.data.frame(t(sapply(out1_subcat3_indicator_dim_fields, function(x) input[[x]], USE.NAMES = F))),
                                              as.data.frame(t(sapply(out2_subcat1_indicator_dim_fields, function(x) input[[x]], USE.NAMES = F))),
                                              as.data.frame(t(sapply(out2_subcat2_indicator_dim_fields, function(x) input[[x]], USE.NAMES = F))),
                                              as.data.frame(t(sapply(out2_subcat3_indicator_dim_fields, function(x) input[[x]], USE.NAMES = F))),
                                              as.data.frame(t(sapply(path1_subcat1_indicator_dim_fields, function(x) input[[x]], USE.NAMES = F))),
                                              as.data.frame(t(sapply(path1_subcat2_indicator_dim_fields, function(x) input[[x]], USE.NAMES = F))),
                                              as.data.frame(t(sapply(path1_subcat3_indicator_dim_fields, function(x) input[[x]], USE.NAMES = F))),
                                              as.data.frame(t(sapply(path2_subcat1_indicator_dim_fields, function(x) input[[x]], USE.NAMES = F))),
                                              as.data.frame(t(sapply(path2_subcat2_indicator_dim_fields, function(x) input[[x]], USE.NAMES = F))),
                                              as.data.frame(t(sapply(path2_subcat3_indicator_dim_fields, function(x) input[[x]], USE.NAMES = F)))) %>%
    mutate(indicatortype = c(rep("Outcome",6),rep("Pathway",6)),
           displayorder = c(rep(1,3),rep(2,3),rep(3,3),rep(4,3)),
           indicatorkey = as.numeric(paste0(c(rep(1,6),rep(2,6)),rv$initiativekey,"0",c(seq(1:6),seq(1:6)))),
           desiredtrend = "",
           indicatorlabelabbr = "",
           subcatlabelabbr = "")
  colnames(init_indicator_dim_data) <- init_indicator_dim_fields
  init_indicator_dim_data
})


# milestones
formData3 <- reactive({
  newdf <- milestonedata()
  newdf <- newdf %>% filter(Milestone!="[CLICK HERE TO INSERT NEW MILESTONE TEXT]")
  
  numMilestones <- length(milestones$milestone[milestones$initiative==input$initiativeoptions & !is.na(milestones$initiative)])
  
  final_milestone_data <- data.frame(milestonekey = as.numeric(paste0("3",rv$initiativekey,
                                                                      (numMilestones+1):(numMilestones+length(newdf$Milestone)),
                                                                      sep="")),
                                     goal = input$goal,
                                     initiative = input$initiative,
                                     milestone = newdf$Milestone,
                                     target = paste0("m",newdf$Target,sep=""),
                                     milestonestatus = newdf$Status,
                                     milestonejust = newdf$Status.Justification,
                                     milestonecreation = NA,
                                     milestonestart = NA,
                                     milestoneend = ifelse(newdf$Display.On.FY20.Dashboard=="No", 
                                                           as.numeric(format(Sys.time(), format="%Y%m%d")),
                                                           NA),
                                     milestoneactive = newdf$Display.On.FY20.Dashboard)
  na.omit(final_milestone_data)
  final_milestone_data
})


# initiative indicator fact
formData4 <- reactive({
  # OUTCOME 1
  out1subcat1df <- out1subcat1()
  out1subcat2df <- out1subcat2()
  out1subcat3df <- out1subcat3()
  
  newout1subcat1 <- data.frame(initiative = input$initiative,
                               indicatorkey = rv$out1subcat1key,
                               indicatorlabel = input$out1indicatorlabel,
                               subcat = input$out1subcat1,
                               rbind.data.frame(data.frame(Year = as.numeric(out1subcat1df$Year),
                                                           Value = as.numeric(out1subcat1df$Value)),
                                                data.frame(Year = input$out1subcat1year, Value = NA)))
  newout1subcat2 <- data.frame(initiative = input$initiative,
                               indicatorkey = rv$out1subcat2key,
                               indicatorlabel = input$out1indicatorlabel,
                               subcat = input$out1subcat2,
                               rbind.data.frame(data.frame(Year = as.numeric(out1subcat2df$Year),
                                                           Value = as.numeric(out1subcat2df$Value)),
                                                data.frame(Year = input$out1subcat2year, Value = NA)))
  newout1subcat3 <- data.frame(initiative = input$initiative,
                               indicatorkey = rv$out1subcat3key,
                               indicatorlabel = input$out1indicatorlabel,
                               subcat = input$out1subcat3,
                               rbind.data.frame(data.frame(Year = as.numeric(out1subcat3df$Year),
                                                           Value = as.numeric(out1subcat3df$Value)),
                                                data.frame(Year = input$out1subcat3year, Value = NA)))
  
  # OUTCOME 2
  out2subcat1df <- out2subcat1()
  out2subcat2df <- out2subcat2()
  out2subcat3df <- out2subcat3()
  
  newout2subcat1 <- data.frame(initiative = input$initiative,
                               indicatorkey = rv$out2subcat1key,
                               indicatorlabel = input$out2indicatorlabel,
                               subcat = input$out2subcat1,
                               rbind.data.frame(data.frame(Year = as.numeric(out2subcat1df$Year),
                                                           Value = as.numeric(out2subcat1df$Value)),
                                                data.frame(Year = input$out2subcat1year, Value = NA)))
  newout2subcat2 <- data.frame(initiative = input$initiative,
                               indicatorkey = rv$out2subcat2key,
                               indicatorlabel = input$out2indicatorlabel,
                               subcat = input$out2subcat2,
                               rbind.data.frame(data.frame(Year = as.numeric(out2subcat2df$Year),
                                                           Value = as.numeric(out2subcat2df$Value)),
                                                data.frame(Year = input$out2subcat2year, Value = NA)))
  newout2subcat3 <- data.frame(initiative = input$initiative,
                               indicatorkey = rv$out2subcat3key,
                               indicatorlabel = input$out2indicatorlabel,
                               subcat = input$out2subcat3,
                               rbind.data.frame(data.frame(Year = as.numeric(out2subcat3df$Year),
                                                           Value = as.numeric(out2subcat3df$Value)),
                                                data.frame(Year = input$out2subcat3year, Value = NA)))
  
  # PATHWAY 1
  path1subcat1df <- path1subcat1()
  path1subcat2df <- path1subcat2()
  path1subcat3df <- path1subcat3()
  
  newpath1subcat1 <- data.frame(initiative = input$initiative,
                               indicatorkey = rv$path1subcat1key,
                               indicatorlabel = input$path1indicatorlabel,
                               subcat = input$path1subcat1,
                               rbind.data.frame(data.frame(Year = as.numeric(path1subcat1df$Year),
                                                           Value = as.numeric(path1subcat1df$Value)),
                                                data.frame(Year = input$path1subcat1year, Value = NA)))
  newpath1subcat2 <- data.frame(initiative = input$initiative,
                               indicatorkey = rv$path1subcat2key,
                               indicatorlabel = input$path1indicatorlabel,
                               subcat = input$path1subcat2,
                               rbind.data.frame(data.frame(Year = as.numeric(path1subcat2df$Year),
                                                           Value = as.numeric(path1subcat2df$Value)),
                                                data.frame(Year = input$path1subcat2year, Value = NA)))
  newpath1subcat3 <- data.frame(initiative = input$initiative,
                               indicatorkey = rv$path1subcat3key,
                               indicatorlabel = input$path1indicatorlabel,
                               subcat = input$path1subcat3,
                               rbind.data.frame(data.frame(Year = as.numeric(path1subcat3df$Year),
                                                           Value = as.numeric(path1subcat3df$Value)),
                                                data.frame(Year = input$path1subcat3year, Value = NA)))
  
  # PATHWAY 2
  path2subcat1df <- path2subcat1()
  path2subcat2df <- path2subcat2()
  path2subcat3df <- path2subcat3()
  
  newpath2subcat1 <- data.frame(initiative = input$initiative,
                                indicatorkey = rv$path2subcat1key,
                                indicatorlabel = input$path2indicatorlabel,
                                subcat = input$path2subcat1,
                                rbind.data.frame(data.frame(Year = as.numeric(path2subcat1df$Year),
                                                            Value = as.numeric(path2subcat1df$Value)),
                                                 data.frame(Year = input$path2subcat1year, Value = NA)))
  newpath2subcat2 <- data.frame(initiative = input$initiative,
                                indicatorkey = rv$path2subcat2key,
                                indicatorlabel = input$path2indicatorlabel,
                                subcat = input$path2subcat2,
                                rbind.data.frame(data.frame(Year = as.numeric(path2subcat2df$Year),
                                                            Value = as.numeric(path2subcat2df$Value)),
                                                 data.frame(Year = input$path2subcat2year, Value = NA)))
  newpath2subcat3 <- data.frame(initiative = input$initiative,
                                indicatorkey = rv$path2subcat3key,
                                indicatorlabel = input$path2indicatorlabel,
                                subcat = input$path2subcat3,
                                rbind.data.frame(data.frame(Year = as.numeric(path2subcat3df$Year),
                                                            Value = as.numeric(path2subcat3df$Value)),
                                                 data.frame(Year = input$path2subcat3year, Value = NA)))
  
  new_init_indicator_fact <- rbind.data.frame(newout1subcat1, newout1subcat2, newout1subcat3, newout2subcat1, newout2subcat2, newout2subcat3,
                                              newpath1subcat1, newpath1subcat2, newpath1subcat3, newpath2subcat1, newpath2subcat2, newpath2subcat3)
  na.omit(new_init_indicator_fact)
  new_init_indicator_fact
})