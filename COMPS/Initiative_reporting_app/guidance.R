
# code: guidance text, character limits, shiny alerts for app


# ---- shiny alerts and guidance for text input fields ----

observeEvent(input[["initiativestatementinfo"]], {
  shinyalert("The Initiative Statement",
             "A description of the desired impact that an initiative is working to achieve.  Initiative statements need to identify: (1) the end date of the initiative, (2) the initiative's intended outcome(s), and (3) the major pathways (based on the theory of change) that will lead to achievement of the final outcome(s).",
             type = "info")
})

observeEvent(input[["initiativestatusinfo"]], {
  shinyalert("Completing a Status Assessment",
             "The process of assessing milestones should encourage collective reflection within an initiative team, and succinctly communicate the initiative's current status and progress to senior leadership. Assessment should also capture the initiative's progress, and the state of the system the initiative seeks to influence. 
               
               There is no formula for aggregating the status of individual milestones into a status for the initiative. Initiative teams are encouraged to reflect on how the status of individual milestones affects the overall initiative. In some cases, delays or barriers affecting an individual milestone may have limited impact on overall implementation. In others, critical opportunities at the milestone level may influence, for example, the scaling potential of the initiative. The initiative status will be accompanied by a concise, constructive, critical reflection on progress towards the initiative goal.",
             type = "info")
})

observeEvent(input[["initiativestatuscategoriesinfo"]], {
  shinyalert("Status Assessment Categories",
             "<strong>Opportunity:</strong>  Critical opportunity to scale or leverage Initiative. <br>
                <strong>Progress:</strong>  On track or affected by minor issues that are being addressed by Initiative team. <br>
                <strong>Barrier:</strong>  Implementation is delayed by external factors (e.g., political will, partners engagement). <br>
                <strong>Support:</strong>  Initiative requires leadership support to address one or more issues. <br>
                <strong>Contingent:</strong>  Contingent on achievement of preceding milestone.",
             html = TRUE, type = "info")
})

observeEvent(input[["outcome1statementinfo"]], {
  shinyalert("An Outcome Statement",
             "A SMART descriptive statement that details the intended effects of an initiative. It describes the real-world ecological, social, or environmental changes that an initiative is designed to deliver. \n
               The statement should also include: the anticipated end date by which the target will be reached; and where applicable, the specifics of the indicator used to measure success for this outcome.",
             type = "info")
})

observeEvent(input[["outcome2statementinfo"]], {
  shinyalert("An Outcome Statement",
             "A SMART descriptive statement that details the intended effects of an initiative. It describes the real-world ecological, social, or environmental changes that an initiative is designed to deliver. \n
               The statement should also include: the anticipated end date by which the target will be reached; and where applicable, the specifics of the indicator used to measure success for this outcome.",
             type = "info")
})

observeEvent(input[["pathway1statementinfo"]], {
  shinyalert("An Pathway Statement",
             "A SMART descriptive statement that highlights intermediate ecological or social changes needed to achieve the initiative's outcomes, based on a clear, evidence-based theory of change. \n
               The statement should also include: the anticipated end date by which the target will be reached; and where applicable, the specifics of the indicator used to measure success for this pathway",
             type = "info")
})

observeEvent(input[["pathway2statementinfo"]], {
  shinyalert("An Pathway Statement",
             "A SMART descriptive statement that highlights intermediate ecological or social changes needed to achieve the initiative's outcomes, based on a clear, evidence-based theory of change. \n
               The statement should also include: the anticipated end date by which the target will be reached; and where applicable, the specifics of the indicator used to measure success for this pathway",
             type = "info")
})

observeEvent(input[["milestonestatusinfo"]], {
  shinyalert("Milestone Status Assessments",
             "Each milestone is: (1) categorized into one of five assessment categories, and (2) accompanied by a brief written description or justification that provides a concise and constructively critical assessment, directly reflecting milestone progress.  If a specific management action is desired, then it can also be indicated in the brief written description. 
               
               Milestone status assessments are intended to enable teams to flag the need for support from leadership, where initiatives are experiencing issues, or coming across critical opportunities to scale or leverage the initiative.",
             type = "info")
})

observeEvent(input[["milestonestatuscategoriesinfo"]], {
  shinyalert("Status Assessment Categories",
             "<strong>Opportunity:</strong>  Critical opportunity to scale or leverage Initiative. <br>
                <strong>Progress:</strong>  On track or affected by minor issues that are being addressed by Initiative team. <br>
                <strong>Barrier:</strong>  Implementation is delayed by external factors (e.g., political will, partners engagement). <br>
                <strong>Support:</strong>  Initiative requires leadership support to address one or more issues. <br>
                <strong>Contingent:</strong>  Contingent on achievement of preceding milestone.",
             html = TRUE, type = "info")
})


# ---- character limit countdowns ----

output$out1statecharcount <- reactive({ paste0(300-nchar(input$outcome1statement), ' characters remaining.' )})
output$out1labelcharcount <- reactive({ paste0(45-nchar(input$out1indicatorlabel), ' characters remaining.' ) })
output$out1unitscharcount <- reactive({ paste0(50-nchar(input$out1indicatorunits), ' characters remaining.' ) })
output$out1subcat1charcount <- reactive({ paste0(20-nchar(input$out1subcat1), ' characters remaining.' ) })
output$out1subcat2charcount <- reactive({ paste0(20-nchar(input$out1subcat2), ' characters remaining.' ) })
output$out1subcat3charcount <- reactive({ paste0(20-nchar(input$out1subcat3), ' characters remaining.' ) })

output$out2statecharcount <- reactive({ paste0(300-nchar(input$outcome2statement), ' characters remaining.' ) })
output$out2labelcharcount <- reactive({ paste0(45-nchar(input$out2indicatorlabel), ' characters remaining.' ) })
output$out2unitscharcount <- reactive({ paste0(50-nchar(input$out2indicatorunits), ' characters remaining.' ) })
output$out2subcat1charcount <- reactive({ paste0(20-nchar(input$out2subcat1), ' characters remaining.' ) })
output$out2subcat2charcount <- reactive({ paste0(20-nchar(input$out2subcat2), ' characters remaining.' ) })
output$out2subcat3charcount <- reactive({ paste0(20-nchar(input$out2subcat3), ' characters remaining.' ) })

output$path1statecharcount <- reactive({ paste0(300-nchar(input$pathway1statement), ' characters remaining.' ) })
output$path1labelcharcount <- reactive({ paste0(45-nchar(input$path1indicatorlabel), ' characters remaining.' ) })
output$path1unitscharcount <- reactive({ paste0(50-nchar(input$path1indicatorunits), ' characters remaining.' ) })
output$path1subcat1charcount <- reactive({ paste0(20-nchar(input$path1subcat1), ' characters remaining.' ) })
output$path1subcat2charcount <- reactive({ paste0(20-nchar(input$path1subcat2), ' characters remaining.' ) })
output$path1subcat3charcount <- reactive({ paste0(20-nchar(input$path1subcat3), ' characters remaining.' ) })

output$path2statecharcount <- reactive({ paste0(300-nchar(input$pathway2statement), ' characters remaining.' ) })
output$path2labelcharcount <- reactive({ paste0(45-nchar(input$path2indicatorlabel), ' characters remaining.' ) })
output$path2unitscharcount <- reactive({ paste0(50-nchar(input$path2indicatorunits), ' characters remaining.' ) })
output$path2subcat1charcount <- reactive({ paste0(20-nchar(input$path2subcat1), ' characters remaining.' ) })
output$path2subcat2charcount <- reactive({ paste0(20-nchar(input$path2subcat2), ' characters remaining.' ) })
output$path2subcat3charcount <- reactive({ paste0(20-nchar(input$path2subcat3), ' characters remaining.' ) })


# ---- set character limits ----

shinyjs::runjs("$('#initiativestatement').attr('maxlength', 325)")
shinyjs::runjs("$('#initiativejust').attr('maxlength', 350)")

shinyjs::runjs("$('#outcome1statement').attr('maxlength', 300)")
shinyjs::runjs("$('#out1indicatorlabel').attr('maxlength', 45)")
shinyjs::runjs("$('#out1indicatorunits').attr('maxlength', 50)")
shinyjs::runjs("$('#out1subcat1').attr('maxlength', 20)")
shinyjs::runjs("$('#out1subcat2').attr('maxlength', 20)")
shinyjs::runjs("$('#out1subcat3').attr('maxlength', 20)")

shinyjs::runjs("$('#outcome2statement').attr('maxlength', 300)")
shinyjs::runjs("$('#out2indicatorlabel').attr('maxlength', 45)")
shinyjs::runjs("$('#out2indicatorunits').attr('maxlength', 50)")
shinyjs::runjs("$('#out2subcat1').attr('maxlength', 20)")
shinyjs::runjs("$('#out2subcat2').attr('maxlength', 20)")
shinyjs::runjs("$('#out2subcat3').attr('maxlength', 20)")

shinyjs::runjs("$('#pathway1statement').attr('maxlength', 300)")
shinyjs::runjs("$('#path1indicatorlabel').attr('maxlength', 45)")
shinyjs::runjs("$('#path1indicatorunits').attr('maxlength', 50)")
shinyjs::runjs("$('#path1subcat1').attr('maxlength', 20)")
shinyjs::runjs("$('#path1subcat2').attr('maxlength', 20)")
shinyjs::runjs("$('#path1subcat3').attr('maxlength', 20)")

shinyjs::runjs("$('#pathway2statement').attr('maxlength', 300)")
shinyjs::runjs("$('#path2indicatorlabel').attr('maxlength', 45)")
shinyjs::runjs("$('#path2indicatorunits').attr('maxlength', 50)")
shinyjs::runjs("$('#path2subcat1').attr('maxlength', 20)")
shinyjs::runjs("$('#path2subcat2').attr('maxlength', 20)")
shinyjs::runjs("$('#path2subcat3').attr('maxlength', 20)")


# ---- last updated reminders ----

output$initiativeassessdate <- reactive ({ 
  
  initkey <- unique(initiative_dim$initiativekey[initiative_dim$initiative==input$initiativeoptions])
  current <- initiative_dim$initiativejust[initiative_dim$initiativekey==initkey & 
                                             initiative_dim$timestamp==max(initiative_dim$timestamp[which(initiative_dim$initiativekey==initkey)])]
  
  initstatus <- 
    initiative_dim %>% 
    group_by(initiativejust, initiativekey) %>% 
    summarise(lastchanged = min(timestamp, na.rm = T)) 
  
  if(length(initkey)==1) {
    initstatus <- initstatus %>% filter(initiativekey==initkey & initiativejust==current) }
  
  latestyear <- ifelse(length(initkey)==1, substr(initstatus$lastchanged, 1, 4), "")
  latestmonth <- ifelse(length(initkey)==1, substr(initstatus$lastchanged, 5, 6), "")
  latestday <- ifelse(length(initkey)==1, substr(initstatus$lastchanged, 7, 8), "")
  
  latestdate <- ifelse(length(initkey)==1, paste(latestmonth, latestday, latestyear, sep="/"), "")

  paste0('INITIATIVE STATUS\nLAST UPDATED:\n', latestdate, sep=" ")
  })


output$financialdate <- reactive ({ 
  
  initkey <- unique(initiative_dim$initiativekey[initiative_dim$initiative==input$initiativeoptions])
  currentneeded <- initiative_dim$fundsneeded[initiative_dim$initiativekey==initkey & 
                                                initiative_dim$timestamp==max(initiative_dim$timestamp[which(initiative_dim$initiativekey==initkey)])]
  currentsecured <- initiative_dim$fundssecured[initiative_dim$initiativekey==initkey & 
                                                  initiative_dim$timestamp==max(initiative_dim$timestamp[which(initiative_dim$initiativekey==initkey)])]
  currentanticipated <- initiative_dim$fundsanticipated[initiative_dim$initiativekey==initkey & 
                                                          initiative_dim$timestamp==max(initiative_dim$timestamp[which(initiative_dim$initiativekey==initkey)])]
  
  initfundsneeded <- 
    initiative_dim %>% 
    group_by(fundsneeded, initiativekey) %>% 
    summarise(needed.lastchanged = min(timestamp, na.rm = T)) 
  
  initfundssecured <- 
    initiative_dim %>% 
    group_by(fundssecured, initiativekey) %>% 
    summarise(secured.lastchanged = min(timestamp, na.rm = T))

  initfundsanticipated <- 
    initiative_dim %>% 
    group_by(fundsanticipated, initiativekey) %>% 
    summarise(anticipated.lastchanged = min(timestamp, na.rm = T))

  
  if(length(initkey)==1) {
    initfundsneeded <- initfundsneeded %>% filter(initiativekey==initkey & fundsneeded==currentneeded)
    initfundssecured <- initfundssecured %>% filter(initiativekey==initkey & fundssecured==currentsecured)
    initfundsanticipated <- initfundsanticipated %>% filter(initiativekey==initkey & fundsanticipated==currentanticipated)
  }
  
  
  initfinance <- 
    left_join(initfundsneeded, initfundssecured, by = "initiativekey") %>%
    left_join(initfundsanticipated, by = "initiativekey") %>%
    mutate(lastchanged = max(needed.lastchanged, secured.lastchanged, anticipated.lastchanged))
  
  latestyear <- ifelse(length(initkey)==1, substr(initfinance$lastchanged, 1, 4), "")
  latestmonth <- ifelse(length(initkey)==1, substr(initfinance$lastchanged, 5, 6), "")
  latestday <- ifelse(length(initkey)==1, substr(initfinance$lastchanged, 7, 8), "")
  
  latestdate <- ifelse(length(initkey==1), paste(latestmonth, latestday, latestyear, sep="/"), "")
  
  paste0('FINANCIAL INFORMATION\nLAST UPDATED:\n', latestdate, sep="")
})

