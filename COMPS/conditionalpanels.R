
# code: functions for each outcome and pathway, rendering a ui for the number of subcategories selected

# outcome 1 conditional panels for trend data
outcome1Conditional <- 
  function(selectedinitiative, input, output) {
    output$out1numtrend <- renderUI({
      if (input$out1numtrend == "") return(NULL) else {
        if (input$out1numtrend == 1) return(
          tagList(tags$hr(),
                  numericInput(inputId = "out1subcat1year",
                               label = "Target Year",
                               value = max(init_indicator_fact$Year[init_indicator_fact$initiative==selectedinitiative &
                                                                  substr(init_indicator_fact$indicatorkey,7,7)=="1" & 
                                                                  substr(init_indicator_fact$indicatorkey,1,1)=="1" & 
                                                                  init_indicator_fact$timestamp==max(init_indicator_fact$timestamp[
                                                                    which(init_indicator_fact$initiative==selectedinitiative)])]),
                               width = 200),
                  numericInput(inputId = "out1subcat1target",
                               label = "Target Value",
                               value = init_indicator_dim$subcattarget[init_indicator_dim$initiative==selectedinitiative &
                                                                         substr(init_indicator_dim$indicatorkey,7,7)=="1" & 
                                                                         substr(init_indicator_dim$indicatorkey,1,1)=="1" & 
                                                                         init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                           which(init_indicator_dim$initiative==selectedinitiative)])],
                               width = 200),
                  tags$br(),
                  h5(tags$em("Current data:")),
                  DT::dataTableOutput(outputId = "out1subcat1data", width = 300),
                  actionButton(inputId = "addout1subcat1Data",
                               label = "Add Row"),
                  hidden(textInput("out1subcat1","")),
                  hidden(textInput("out1subcat2","")),
                  hidden(numericInput("out1subcat2year","","")),
                  hidden(numericInput("out1subcat2target","","")),
                  hidden(textInput("out1subcat3","")),
                  hidden(numericInput("out1subcat3year","","")),
                  hidden(numericInput("out1subcat3target","","")))) else {
                                 if(input$out1numtrend == 2) return(
                                   tagList(tags$hr(),
                                           h4(tags$b("TREND 1")),
                                           tags$br(),
                                           textInput(inputId = "out1subcat1",
                                                     label = "Label",
                                                     value = init_indicator_dim$subcat[init_indicator_dim$initiative==selectedinitiative &
                                                                                         substr(init_indicator_dim$indicatorkey,7,7)=="1" & 
                                                                                         substr(init_indicator_dim$indicatorkey,1,1)=="1" & 
                                                                                         init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                           which(init_indicator_dim$initiative==selectedinitiative)])]),
                                           numericInput(inputId = "out1subcat1year",
                                                        label = "Target Year",
                                                        value = max(init_indicator_fact$Year[init_indicator_fact$initiative==selectedinitiative &
                                                                                               substr(init_indicator_fact$indicatorkey,7,7)=="1" & 
                                                                                               substr(init_indicator_fact$indicatorkey,1,1)=="1" & 
                                                                                               init_indicator_fact$timestamp==max(init_indicator_fact$timestamp[
                                                                                                 which(init_indicator_fact$initiative==selectedinitiative)])]),
                                                        width = 200),
                                           numericInput(inputId = "out1subcat1target",
                                                        label = "Target Value",
                                                        value = init_indicator_dim$subcattarget[init_indicator_dim$initiative==selectedinitiative &
                                                                                                  substr(init_indicator_dim$indicatorkey,7,7)=="1" & 
                                                                                                  substr(init_indicator_dim$indicatorkey,1,1)=="1" & 
                                                                                                  init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                                    which(init_indicator_dim$initiative==selectedinitiative)])],
                                                        
                                                        width = 200),
                                           tags$br(),
                                           h5(tags$em("Current data:")),
                                           DT::dataTableOutput(outputId = "out1subcat1data", width = 300),
                                           actionButton(inputId = "addout1subcat1Data",
                                                        label = "Add Row"),
                                           tags$hr(),
                                           h4(tags$b("TREND 2")),
                                           tags$br(),
                                           textInput(inputId = "out1subcat2",
                                                     label = "Label",
                                                     value = init_indicator_dim$subcat[init_indicator_dim$initiative==selectedinitiative &
                                                                                         substr(init_indicator_dim$indicatorkey,7,7)=="2" & 
                                                                                         substr(init_indicator_dim$indicatorkey,1,1)=="1" & 
                                                                                         init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                           which(init_indicator_dim$initiative==selectedinitiative)])]),
                                           numericInput(inputId = "out1subcat2year",
                                                        label = "Target Year",
                                                        value = max(init_indicator_fact$Year[init_indicator_fact$initiative==selectedinitiative &
                                                                                               substr(init_indicator_fact$indicatorkey,7,7)=="2" & 
                                                                                               substr(init_indicator_fact$indicatorkey,1,1)=="1" & 
                                                                                               init_indicator_fact$timestamp==max(init_indicator_fact$timestamp[
                                                                                                 which(init_indicator_fact$initiative==selectedinitiative)])]),
                                                        width = 200),
                                           numericInput(inputId = "out1subcat2target",
                                                        label = "Target Value",
                                                        value = init_indicator_dim$subcattarget[init_indicator_dim$initiative==selectedinitiative &
                                                                                                  substr(init_indicator_dim$indicatorkey,7,7)=="2" & 
                                                                                                  substr(init_indicator_dim$indicatorkey,1,1)=="1" & 
                                                                                                  init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                                    which(init_indicator_dim$initiative==selectedinitiative)])],
                                                        width = 200),
                                           tags$br(),
                                           h5(tags$em("Current data:")),
                                           DT::dataTableOutput(outputId = "out1subcat2data", width = 300),
                                           actionButton(inputId = "addout1subcat2Data",
                                                        label = "Add Row"),
                                           hidden(textInput("out1subcat3","")),
                                           hidden(numericInput("out1subcat3year","","")),
                                           hidden(numericInput("out1subcat3target","","")))) else {
                                                          tagList(tags$hr(),
                                                                  h4(tags$b("TREND 1")),
                                                                  tags$br(),
                                                                  textInput(inputId = "out1subcat1",
                                                                            label = "Label",
                                                                            value = init_indicator_dim$subcat[init_indicator_dim$initiative==selectedinitiative &
                                                                                                                substr(init_indicator_dim$indicatorkey,7,7)=="1" & 
                                                                                                                substr(init_indicator_dim$indicatorkey,1,1)=="1" & 
                                                                                                                init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                                                  which(init_indicator_dim$initiative==selectedinitiative)])]),
                                                                  numericInput(inputId = "out1subcat1year",
                                                                               label = "Target Year",
                                                                               value = max(init_indicator_fact$Year[init_indicator_fact$initiative==selectedinitiative &
                                                                                                                      substr(init_indicator_fact$indicatorkey,7,7)=="1" & 
                                                                                                                      substr(init_indicator_fact$indicatorkey,1,1)=="1" & 
                                                                                                                      init_indicator_fact$timestamp==max(init_indicator_fact$timestamp[
                                                                                                                        which(init_indicator_fact$initiative==selectedinitiative)])]),
                                                                               width = 200),
                                                                  numericInput(inputId = "out1subcat1target",
                                                                               label = "Target Value",
                                                                               value = init_indicator_dim$subcattarget[init_indicator_dim$initiative==selectedinitiative &
                                                                                                                         substr(init_indicator_dim$indicatorkey,7,7)=="1" & 
                                                                                                                         substr(init_indicator_dim$indicatorkey,1,1)=="1" & 
                                                                                                                         init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                                                           which(init_indicator_dim$initiative==selectedinitiative)])], 
                                                                               width = 200),
                                                                  tags$br(),
                                                                  h5(tags$em("Current data:")),
                                                                  DT::dataTableOutput(outputId = "out1subcat1data", width = 300),
                                                                  actionButton(inputId = "addout1subcat1Data",
                                                                               label = "Add Row"),
                                                                  tags$hr(),
                                                                  h4(tags$b("TREND 2")),
                                                                  tags$br(),
                                                                  textInput(inputId = "out1subcat2",
                                                                            label = "Label",
                                                                            value = init_indicator_dim$subcat[init_indicator_dim$initiative==selectedinitiative &
                                                                                                                substr(init_indicator_dim$indicatorkey,7,7)=="2" & 
                                                                                                                substr(init_indicator_dim$indicatorkey,1,1)=="1" & 
                                                                                                                init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                                                  which(init_indicator_dim$initiative==selectedinitiative)])]),
                                                                  numericInput(inputId = "out1subcat2year",
                                                                               label = "Target Year",
                                                                               value = max(init_indicator_fact$Year[init_indicator_fact$initiative==selectedinitiative &
                                                                                                                      substr(init_indicator_fact$indicatorkey,7,7)=="2" & 
                                                                                                                      substr(init_indicator_fact$indicatorkey,1,1)=="1" & 
                                                                                                                      init_indicator_fact$timestamp==max(init_indicator_fact$timestamp[
                                                                                                                        which(init_indicator_fact$initiative==selectedinitiative)])]),
                                                                               width = 200),
                                                                  numericInput(inputId = "out1subcat2target",
                                                                               label = "Target Value",
                                                                               value = init_indicator_dim$subcattarget[init_indicator_dim$initiative==selectedinitiative &
                                                                                                                         substr(init_indicator_dim$indicatorkey,7,7)=="2" & 
                                                                                                                         substr(init_indicator_dim$indicatorkey,1,1)=="1" & 
                                                                                                                         init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                                                           which(init_indicator_dim$initiative==selectedinitiative)])],
                                                                               width = 200),
                                                                  tags$br(),
                                                                  h5(tags$em("Current data:")),
                                                                  DT::dataTableOutput(outputId = "out1subcat2data", width = 300),
                                                                  actionButton(inputId = "addout1subcat2Data",
                                                                               label = "Add Row"),
                                                                  tags$hr(),
                                                                  h4(tags$b("TREND 3")),
                                                                  tags$br(),
                                                                  textInput(inputId = "out1subcat3",
                                                                            label = "Label",
                                                                            value = init_indicator_dim$subcat[init_indicator_dim$initiative==selectedinitiative &
                                                                                                                substr(init_indicator_dim$indicatorkey,7,7)=="3" & 
                                                                                                                substr(init_indicator_dim$indicatorkey,1,1)=="1" & 
                                                                                                                init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                                                  which(init_indicator_dim$initiative==selectedinitiative)])]),
                                                                  numericInput(inputId = "out1subcat3year",
                                                                               label = "Target Year",
                                                                               value = max(init_indicator_fact$Year[init_indicator_fact$initiative==selectedinitiative &
                                                                                                                      substr(init_indicator_fact$indicatorkey,7,7)=="3" & 
                                                                                                                      substr(init_indicator_fact$indicatorkey,1,1)=="1" & 
                                                                                                                      init_indicator_fact$timestamp==max(init_indicator_fact$timestamp[
                                                                                                                        which(init_indicator_fact$initiative==selectedinitiative)])]),
                                                                               width = 200),
                                                                  numericInput(inputId = "out1subcat3target",
                                                                               label = "Target Value",
                                                                               value = init_indicator_dim$subcattarget[init_indicator_dim$initiative==selectedinitiative &
                                                                                                                         substr(init_indicator_dim$indicatorkey,7,7)=="3" & 
                                                                                                                         substr(init_indicator_dim$indicatorkey,1,1)=="1" & 
                                                                                                                         init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                                                           which(init_indicator_dim$initiative==selectedinitiative)])],
                                                                               width = 200),
                                                                  tags$br(),
                                                                  h5(tags$em("Current data:")),
                                                                  DT::dataTableOutput(outputId = "out1subcat3data", width = 300),
                                                                  actionButton(inputId = "addout1subcat3Data",
                                                                               label = "Add Row"))
                                             
                                                        }
                               }
      }
    })
  }



# outcome 2 conditional panels for trend data
outcome2Conditional <-
  function(selectedinitiative, input, output) {
    output$out2numtrend <- renderUI({
      if (input$out2numtrend == "") return(NULL) else {
        if (input$out2numtrend == 1) return(
          tagList(tags$hr(),
                  numericInput(inputId = "out2subcat1year",
                               label = "Target Year",
                               value = max(init_indicator_fact$Year[init_indicator_fact$initiative==selectedinitiative &
                                                                      substr(init_indicator_fact$indicatorkey,7,7)=="4" &
                                                                      substr(init_indicator_fact$indicatorkey,1,1)=="1" &
                                                                      init_indicator_fact$timestamp==max(init_indicator_fact$timestamp[
                                                                        which(init_indicator_fact$initiative==selectedinitiative)])])),
                  numericInput(inputId = "out2subcat1target",
                               label = "Target Value",
                               value = init_indicator_dim$subcattarget[init_indicator_dim$initiative==selectedinitiative &
                                                                         substr(init_indicator_dim$indicatorkey,7,7)=="4" &
                                                                         substr(init_indicator_dim$indicatorkey,1,1)=="1" &
                                                                         init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                           which(init_indicator_dim$initiative==selectedinitiative)])]),
                  DT::dataTableOutput(outputId = "out2subcat1data"),
                  hidden(textInput("out2subcat1","")),
                  hidden(textInput("out2subcat2","")),
                  hidden(numericInput("out2subcat2year","","")),
                  hidden(numericInput("out2subcat2target","","")),
                  hidden(textInput("out2subcat3","")),
                  hidden(numericInput("out2subcat3year","","")),
                  hidden(numericInput("out2subcat3target","","")))) else {
                    if(input$out2numtrend == 2) return(
                      tagList(tags$hr(),
                              h5(tags$b("Trend 1")),
                              textInput(inputId = "out2subcat1",
                                        label = "Label",
                                        value = init_indicator_dim$subcat[init_indicator_dim$initiative==selectedinitiative &
                                                                            substr(init_indicator_dim$indicatorkey,7,7)=="4" &
                                                                            substr(init_indicator_dim$indicatorkey,1,1)=="1" &
                                                                            init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                              which(init_indicator_dim$initiative==selectedinitiative)])]),
                              numericInput(inputId = "out2subcat1year",
                                           label = "Target Year",
                                           value = max(init_indicator_fact$Year[init_indicator_fact$initiative==selectedinitiative &
                                                                                  substr(init_indicator_fact$indicatorkey,7,7)=="4" &
                                                                                  substr(init_indicator_fact$indicatorkey,1,1)=="1" &
                                                                                  init_indicator_fact$timestamp==max(init_indicator_fact$timestamp[
                                                                                    which(init_indicator_fact$initiative==selectedinitiative)])])),
                              numericInput(inputId = "out2subcat1target",
                                           label = "Target Value",
                                           value = init_indicator_dim$subcattarget[init_indicator_dim$initiative==selectedinitiative &
                                                                                     substr(init_indicator_dim$indicatorkey,7,7)=="4" &
                                                                                     substr(init_indicator_dim$indicatorkey,1,1)=="1" &
                                                                                     init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                       which(init_indicator_dim$initiative==selectedinitiative)])]),
                              DT::dataTableOutput(outputId = "out2subcat1data"),
                              tags$hr(),
                              h5(tags$b("Trend 2")),
                              textInput(inputId = "out2subcat2",
                                        label = "Label",
                                        value = init_indicator_dim$subcat[init_indicator_dim$initiative==selectedinitiative &
                                                                            substr(init_indicator_dim$indicatorkey,7,7)=="5" &
                                                                            substr(init_indicator_dim$indicatorkey,1,1)=="1" &
                                                                            init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                              which(init_indicator_dim$initiative==selectedinitiative)])]),
                              numericInput(inputId = "out2subcat2year",
                                           label = "Target Year",
                                           value = max(init_indicator_fact$Year[init_indicator_fact$initiative==selectedinitiative &
                                                                                  substr(init_indicator_fact$indicatorkey,7,7)=="5" &
                                                                                  substr(init_indicator_fact$indicatorkey,1,1)=="1" &
                                                                                  init_indicator_fact$timestamp==max(init_indicator_fact$timestamp[
                                                                                    which(init_indicator_fact$initiative==selectedinitiative)])])),
                              numericInput(inputId = "out2subcat2target",
                                           label = "Target Value",
                                           value = init_indicator_dim$subcattarget[init_indicator_dim$initiative==selectedinitiative &
                                                                                     substr(init_indicator_dim$indicatorkey,7,7)=="5" &
                                                                                     substr(init_indicator_dim$indicatorkey,1,1)=="1" &
                                                                                     init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                       which(init_indicator_dim$initiative==selectedinitiative)])]),
                              hidden(textInput("out2subcat3","")),
                              hidden(numericInput("out2subcat3year","","")),
                              hidden(numericInput("out2subcat3target","","")))) else {
                                                                                         tagList(tags$hr(),
                                                                                                 h5(tags$b("Trend 1")),
                                                                                                 textInput(inputId = "out2subcat1",
                                                                                                           label = "Label",
                                                                                                           value = init_indicator_dim$subcat[init_indicator_dim$initiative==selectedinitiative &
                                                                                                                                               substr(init_indicator_dim$indicatorkey,7,7)=="4" &
                                                                                                                                               substr(init_indicator_dim$indicatorkey,1,1)=="1" &
                                                                                                                                               init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                                                                                 which(init_indicator_dim$initiative==selectedinitiative)])]),
                                                                                                 numericInput(inputId = "out2subcat1year",
                                                                                                              label = "Target Year",
                                                                                                              value = max(init_indicator_fact$Year[init_indicator_fact$initiative==selectedinitiative &
                                                                                                                                                     substr(init_indicator_fact$indicatorkey,7,7)=="4" &
                                                                                                                                                     substr(init_indicator_fact$indicatorkey,1,1)=="1" &
                                                                                                                                                     init_indicator_fact$timestamp==max(init_indicator_fact$timestamp[
                                                                                                                                                       which(init_indicator_fact$initiative==selectedinitiative)])])),
                                                                                                 numericInput(inputId = "out2subcat1target",
                                                                                                              label = "Target Value",
                                                                                                              value = init_indicator_dim$subcattarget[init_indicator_dim$initiative==selectedinitiative &
                                                                                                                                                        substr(init_indicator_dim$indicatorkey,7,7)=="4" &
                                                                                                                                                        substr(init_indicator_dim$indicatorkey,1,1)=="1" &
                                                                                                                                                        init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                                                                                          which(init_indicator_dim$initiative==selectedinitiative)])]),
                                                                                                 DT::dataTableOutput(outputId = "out2subcat1data"),
                                                                                                 tags$hr(),
                                                                                                 h5(tags$b("Trend 2")),
                                                                                                 textInput(inputId = "out2subcat2",
                                                                                                           label = "Label",
                                                                                                           value = init_indicator_dim$subcat[init_indicator_dim$initiative==selectedinitiative &
                                                                                                                                               substr(init_indicator_dim$indicatorkey,7,7)=="5" &
                                                                                                                                               substr(init_indicator_dim$indicatorkey,1,1)=="1" &
                                                                                                                                               init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                                                                                 which(init_indicator_dim$initiative==selectedinitiative)])]),
                                                                                                 numericInput(inputId = "out2subcat2year",
                                                                                                              label = "Target Year",
                                                                                                              value = max(init_indicator_fact$Year[init_indicator_fact$initiative==selectedinitiative &
                                                                                                                                                     substr(init_indicator_fact$indicatorkey,7,7)=="5" &
                                                                                                                                                     substr(init_indicator_fact$indicatorkey,1,1)=="1" &
                                                                                                                                                     init_indicator_fact$timestamp==max(init_indicator_fact$timestamp[
                                                                                                                                                       which(init_indicator_fact$initiative==selectedinitiative)])])),
                                                                                                 numericInput(inputId = "out2subcat2target",
                                                                                                              label = "Target Value",
                                                                                                              value = init_indicator_dim$subcattarget[init_indicator_dim$initiative==selectedinitiative &
                                                                                                                                                        substr(init_indicator_dim$indicatorkey,7,7)=="5" &
                                                                                                                                                        substr(init_indicator_dim$indicatorkey,1,1)=="1" &
                                                                                                                                                        init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                                                                                          which(init_indicator_dim$initiative==selectedinitiative)])]),
                                                                                                 tags$hr(),
                                                                                                 h5(tags$b("Trend 3")),
                                                                                                 textInput(inputId = "out2subcat3",
                                                                                                           label = "Label",
                                                                                                           value = init_indicator_dim$subcat[init_indicator_dim$initiative==selectedinitiative &
                                                                                                                                               substr(init_indicator_dim$indicatorkey,7,7)=="6" &
                                                                                                                                               substr(init_indicator_dim$indicatorkey,1,1)=="1" &
                                                                                                                                               init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                                                                                 which(init_indicator_dim$initiative==selectedinitiative)])]),
                                                                                                 numericInput(inputId = "out2subcat3year",
                                                                                                              label = "Target Year",
                                                                                                              value = max(init_indicator_fact$Year[init_indicator_fact$initiative==selectedinitiative &
                                                                                                                                                     substr(init_indicator_fact$indicatorkey,7,7)=="6" &
                                                                                                                                                     substr(init_indicator_fact$indicatorkey,1,1)=="1" &
                                                                                                                                                     init_indicator_fact$timestamp==max(init_indicator_fact$timestamp[
                                                                                                                                                       which(init_indicator_fact$initiative==selectedinitiative)])])),
                                                                                                 numericInput(inputId = "out2subcat3target",
                                                                                                              label = "Target Value",
                                                                                                              value = init_indicator_dim$subcattarget[init_indicator_dim$initiative==selectedinitiative &
                                                                                                                                                        substr(init_indicator_dim$indicatorkey,7,7)=="6" &
                                                                                                                                                        substr(init_indicator_dim$indicatorkey,1,1)=="1" &
                                                                                                                                                        init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                                                                                          which(init_indicator_dim$initiative==selectedinitiative)])]))
                                                                                       }
                  }
      }
    })
  }

# pathway 1 conditional panels for trend data
pathway1Conditional <-
  function(selectedinitiative, input, output) {
    output$path1numtrend <- renderUI({
      if (input$path1numtrend == "") return(NULL) else {
        if (input$path1numtrend == 1) return(
          tagList(tags$hr(),
                  numericInput(inputId = "path1subcat1year",
                               label = "Target Year",
                               value = max(init_indicator_fact$Year[init_indicator_fact$initiative==selectedinitiative &
                                                                      substr(init_indicator_fact$indicatorkey,7,7)=="1" &
                                                                      substr(init_indicator_fact$indicatorkey,1,1)=="2" &
                                                                      init_indicator_fact$timestamp==max(init_indicator_fact$timestamp[
                                                                        which(init_indicator_fact$initiative==selectedinitiative)])])),
                  numericInput(inputId = "path1subcat1target",
                               label = "Target Value",
                               value = init_indicator_dim$subcattarget[init_indicator_dim$initiative==selectedinitiative &
                                                                         substr(init_indicator_dim$indicatorkey,7,7)=="1" &
                                                                         substr(init_indicator_dim$indicatorkey,1,1)=="2" &
                                                                         init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                           which(init_indicator_dim$initiative==selectedinitiative)])]),
                  DT::dataTableOutput(outputId = "path1subcat1data"),
                  hidden(textInput("path1subcat1","")),
                  hidden(textInput("path1subcat2","")),
                  hidden(numericInput("path1subcat2year","","")),
                  hidden(numericInput("path1subcat2target","","")),
                  hidden(textInput("path1subcat3","")),
                  hidden(numericInput("path1subcat3year","","")),
                  hidden(numericInput("path1subcat3target","","")))) else {
                    if(input$path1numtrend == 2) return(
                      tagList(tags$hr(),
                              h5(tags$b("Trend 1")),
                              textInput(inputId = "path1subcat1",
                                        label = "Label",
                                        value = init_indicator_dim$subcat[init_indicator_dim$initiative==selectedinitiative &
                                                                            substr(init_indicator_dim$indicatorkey,7,7)=="1" &
                                                                            substr(init_indicator_dim$indicatorkey,1,1)=="2" &
                                                                            init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                              which(init_indicator_dim$initiative==selectedinitiative)])]),
                              numericInput(inputId = "path1subcat1year",
                                           label = "Target Year",
                                           value = max(init_indicator_fact$Year[init_indicator_fact$initiative==selectedinitiative &
                                                                                  substr(init_indicator_fact$indicatorkey,7,7)=="1" &
                                                                                  substr(init_indicator_fact$indicatorkey,1,1)=="2" &
                                                                                  init_indicator_fact$timestamp==max(init_indicator_fact$timestamp[
                                                                                    which(init_indicator_fact$initiative==selectedinitiative)])])),
                              numericInput(inputId = "path1subcat1target",
                                           label = "Target Value",
                                           value = init_indicator_dim$subcattarget[init_indicator_dim$initiative==selectedinitiative &
                                                                                     substr(init_indicator_dim$indicatorkey,7,7)=="1" &
                                                                                     substr(init_indicator_dim$indicatorkey,1,1)=="2" &
                                                                                     init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                       which(init_indicator_dim$initiative==selectedinitiative)])]),
                              DT::dataTableOutput(outputId = "path1subcat1data"),
                              tags$hr(),
                              h5(tags$b("Trend 2")),
                              textInput(inputId = "path1subcat2",
                                        label = "Label",
                                        value = init_indicator_dim$subcat[init_indicator_dim$initiative==selectedinitiative &
                                                                            substr(init_indicator_dim$indicatorkey,7,7)=="2" &
                                                                            substr(init_indicator_dim$indicatorkey,1,1)=="2" &
                                                                            init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                              which(init_indicator_dim$initiative==selectedinitiative)])]),
                              numericInput(inputId = "path1subcat2year",
                                           label = "Target Year",
                                           value = max(init_indicator_fact$Year[init_indicator_fact$initiative==selectedinitiative &
                                                                                  substr(init_indicator_fact$indicatorkey,7,7)=="2" &
                                                                                  substr(init_indicator_fact$indicatorkey,1,1)=="2" &
                                                                                  init_indicator_fact$timestamp==max(init_indicator_fact$timestamp[
                                                                                    which(init_indicator_fact$initiative==selectedinitiative)])])),
                              numericInput(inputId = "path1subcat2target",
                                           label = "Target Value",
                                           value = init_indicator_dim$subcattarget[init_indicator_dim$initiative==selectedinitiative &
                                                                                     substr(init_indicator_dim$indicatorkey,7,7)=="2" &
                                                                                     substr(init_indicator_dim$indicatorkey,1,1)=="2" &
                                                                                     init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                       which(init_indicator_dim$initiative==selectedinitiative)])]),
                              hidden(textInput("path1subcat3","")),
                              hidden(numericInput("path1subcat3year","","")),
                              hidden(numericInput("path1subcat3target","","")))) else {
                                                                                         tagList(tags$hr(),
                                                                                                 h5(tags$b("Trend 1")),
                                                                                                 textInput(inputId = "path1subcat1",
                                                                                                           label = "Label",
                                                                                                           value = init_indicator_dim$subcat[init_indicator_dim$initiative==selectedinitiative &
                                                                                                                                               substr(init_indicator_dim$indicatorkey,7,7)=="1" &
                                                                                                                                               substr(init_indicator_dim$indicatorkey,1,1)=="2" &
                                                                                                                                               init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                                                                                 which(init_indicator_dim$initiative==selectedinitiative)])]),
                                                                                                 numericInput(inputId = "path1subcat1year",
                                                                                                              label = "Target Year",
                                                                                                              value = max(init_indicator_fact$Year[init_indicator_fact$initiative==selectedinitiative &
                                                                                                                                                     substr(init_indicator_fact$indicatorkey,7,7)=="1" &
                                                                                                                                                     substr(init_indicator_fact$indicatorkey,1,1)=="2" &
                                                                                                                                                     init_indicator_fact$timestamp==max(init_indicator_fact$timestamp[
                                                                                                                                                       which(init_indicator_fact$initiative==selectedinitiative)])])),
                                                                                                 numericInput(inputId = "path1subcat1target",
                                                                                                              label = "Target Value",
                                                                                                              value = init_indicator_dim$subcattarget[init_indicator_dim$initiative==selectedinitiative &
                                                                                                                                                        substr(init_indicator_dim$indicatorkey,7,7)=="1" &
                                                                                                                                                        substr(init_indicator_dim$indicatorkey,1,1)=="2" &
                                                                                                                                                        init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                                                                                          which(init_indicator_dim$initiative==selectedinitiative)])]),
                                                                                                 DT::dataTableOutput(outputId = "path1subcat1data"),
                                                                                                 tags$hr(),
                                                                                                 h5(tags$b("Trend 2")),
                                                                                                 textInput(inputId = "path1subcat2",
                                                                                                           label = "Label",
                                                                                                           value = init_indicator_dim$subcat[init_indicator_dim$initiative==selectedinitiative &
                                                                                                                                               substr(init_indicator_dim$indicatorkey,7,7)=="2" &
                                                                                                                                               substr(init_indicator_dim$indicatorkey,1,1)=="2" &
                                                                                                                                               init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                                                                                 which(init_indicator_dim$initiative==selectedinitiative)])]),
                                                                                                 numericInput(inputId = "path1subcat2year",
                                                                                                              label = "Target Year",
                                                                                                              value = max(init_indicator_fact$Year[init_indicator_fact$initiative==selectedinitiative &
                                                                                                                                                     substr(init_indicator_fact$indicatorkey,7,7)=="2" &
                                                                                                                                                     substr(init_indicator_fact$indicatorkey,1,1)=="2" &
                                                                                                                                                     init_indicator_fact$timestamp==max(init_indicator_fact$timestamp[
                                                                                                                                                       which(init_indicator_fact$initiative==selectedinitiative)])])),
                                                                                                 numericInput(inputId = "path1subcat2target",
                                                                                                              label = "Target Value",
                                                                                                              value = init_indicator_dim$subcattarget[init_indicator_dim$initiative==selectedinitiative &
                                                                                                                                                        substr(init_indicator_dim$indicatorkey,7,7)=="2" &
                                                                                                                                                        substr(init_indicator_dim$indicatorkey,1,1)=="2" &
                                                                                                                                                        init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                                                                                          which(init_indicator_dim$initiative==selectedinitiative)])]),
                                                                                                 tags$hr(),
                                                                                                 h5(tags$b("Trend 3")),
                                                                                                 textInput(inputId = "path1subcat3",
                                                                                                           label = "Label",
                                                                                                           value = init_indicator_dim$subcat[init_indicator_dim$initiative==selectedinitiative &
                                                                                                                                               substr(init_indicator_dim$indicatorkey,7,7)=="3" &
                                                                                                                                               substr(init_indicator_dim$indicatorkey,1,1)=="2" &
                                                                                                                                               init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                                                                                 which(init_indicator_dim$initiative==selectedinitiative)])]),
                                                                                                 numericInput(inputId = "path1subcat3year",
                                                                                                              label = "Target Year",
                                                                                                              value = max(init_indicator_fact$Year[init_indicator_fact$initiative==selectedinitiative &
                                                                                                                                                     substr(init_indicator_fact$indicatorkey,7,7)=="3" &
                                                                                                                                                     substr(init_indicator_fact$indicatorkey,1,1)=="2" &
                                                                                                                                                     init_indicator_fact$timestamp==max(init_indicator_fact$timestamp[
                                                                                                                                                       which(init_indicator_fact$initiative==selectedinitiative)])])),
                                                                                                 numericInput(inputId = "path1subcat3target",
                                                                                                              label = "Target Value",
                                                                                                              value = init_indicator_dim$subcattarget[init_indicator_dim$initiative==selectedinitiative &
                                                                                                                                                        substr(init_indicator_dim$indicatorkey,7,7)=="3" &
                                                                                                                                                        substr(init_indicator_dim$indicatorkey,1,1)=="2" &
                                                                                                                                                        init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                                                                                          which(init_indicator_dim$initiative==selectedinitiative)])]))
                                                                                       }
                  }
      }
    })
  }


# pathway 2 conditional panels for trend data
pathway2Conditional <-
  function(selectedinitiative, input, output) {
    output$path2numtrend <- renderUI({
      if (input$path2numtrend == "") return(NULL) else {
        if (input$path2numtrend == 1) return(
          tagList(tags$hr(),
                  numericInput(inputId = "path2subcat1year",
                               label = "Target Year",
                               value = max(init_indicator_fact$Year[init_indicator_fact$initiative==selectedinitiative &
                                                                      substr(init_indicator_fact$indicatorkey,7,7)=="4" &
                                                                      substr(init_indicator_fact$indicatorkey,1,1)=="2" &
                                                                      init_indicator_fact$timestamp==max(init_indicator_fact$timestamp[
                                                                        which(init_indicator_fact$initiative==selectedinitiative)])])),
                  numericInput(inputId = "path2subcat1target",
                               label = "Target Value",
                               value = init_indicator_dim$subcattarget[init_indicator_dim$initiative==selectedinitiative &
                                                                         substr(init_indicator_dim$indicatorkey,7,7)=="4" &
                                                                         substr(init_indicator_dim$indicatorkey,1,1)=="2" &
                                                                         init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                           which(init_indicator_dim$initiative==selectedinitiative)])]),
                  DT::dataTableOutput(outputId = "path2subcat1data"),
                  hidden(textInput("path2subcat1","")),
                  hidden(textInput("path2subcat2","")),
                  hidden(numericInput("path2subcat2year","","")),
                  hidden(numericInput("path2subcat2target","","")),
                  hidden(textInput("path2subcat3","")),
                  hidden(numericInput("path2subcat3year","","")),
                  hidden(numericInput("path2subcat3target","","")))) else {
                    if(input$path2numtrend == 2) return(
                      tagList(tags$hr(),
                              h5(tags$b("Trend 1")),
                              textInput(inputId = "path2subcat1",
                                        label = "Label",
                                        value = init_indicator_dim$subcat[init_indicator_dim$initiative==selectedinitiative &
                                                                            substr(init_indicator_dim$indicatorkey,7,7)=="4" &
                                                                            substr(init_indicator_dim$indicatorkey,1,1)=="2" &
                                                                            init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                              which(init_indicator_dim$initiative==selectedinitiative)])]),
                              numericInput(inputId = "path2subcat1year",
                                           label = "Target Year",
                                           value = max(init_indicator_fact$Year[init_indicator_fact$initiative==selectedinitiative &
                                                                                  substr(init_indicator_fact$indicatorkey,7,7)=="4" &
                                                                                  substr(init_indicator_fact$indicatorkey,1,1)=="2" &
                                                                                  init_indicator_fact$timestamp==max(init_indicator_fact$timestamp[
                                                                                    which(init_indicator_fact$initiative==selectedinitiative)])])),
                              numericInput(inputId = "path2subcat1target",
                                           label = "Target Value",
                                           value = init_indicator_dim$subcattarget[init_indicator_dim$initiative==selectedinitiative &
                                                                                     substr(init_indicator_dim$indicatorkey,7,7)=="4" &
                                                                                     substr(init_indicator_dim$indicatorkey,1,1)=="2" &
                                                                                     init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                       which(init_indicator_dim$initiative==selectedinitiative)])]),
                              DT::dataTableOutput(outputId = "path2subcat1data"),
                              tags$hr(),
                              h5(tags$b("Trend 2")),
                              textInput(inputId = "path2subcat2",
                                        label = "Label",
                                        value = init_indicator_dim$subcat[init_indicator_dim$initiative==selectedinitiative &
                                                                            substr(init_indicator_dim$indicatorkey,7,7)=="5" &
                                                                            substr(init_indicator_dim$indicatorkey,1,1)=="2" &
                                                                            init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                              which(init_indicator_dim$initiative==selectedinitiative)])]),
                              numericInput(inputId = "path2subcat2year",
                                           label = "Target Year",
                                           value = max(init_indicator_fact$Year[init_indicator_fact$initiative==selectedinitiative &
                                                                                  substr(init_indicator_fact$indicatorkey,7,7)=="5" &
                                                                                  substr(init_indicator_fact$indicatorkey,1,1)=="2" &
                                                                                  init_indicator_fact$timestamp==max(init_indicator_fact$timestamp[
                                                                                    which(init_indicator_fact$initiative==selectedinitiative)])])),
                              numericInput(inputId = "path2subcat2target",
                                           label = "Target Value",
                                           value = init_indicator_dim$subcattarget[init_indicator_dim$initiative==selectedinitiative &
                                                                                     substr(init_indicator_dim$indicatorkey,7,7)=="5" &
                                                                                     substr(init_indicator_dim$indicatorkey,1,1)=="2" &
                                                                                     init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                       which(init_indicator_dim$initiative==selectedinitiative)])]),
                              hidden(textInput("path2subcat3","")),
                              hidden(numericInput("path2subcat3year","","")),
                              hidden(numericInput("path2subcat3target","","")))) else {
                                                                                         tagList(tags$hr(),
                                                                                                 h5(tags$b("Trend 1")),
                                                                                                 textInput(inputId = "path2subcat1",
                                                                                                           label = "Label",
                                                                                                           value = init_indicator_dim$subcat[init_indicator_dim$initiative==selectedinitiative &
                                                                                                                                               substr(init_indicator_dim$indicatorkey,7,7)=="4" &
                                                                                                                                               substr(init_indicator_dim$indicatorkey,1,1)=="2" &
                                                                                                                                               init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                                                                                 which(init_indicator_dim$initiative==selectedinitiative)])]),
                                                                                                 numericInput(inputId = "path2subcat1year",
                                                                                                              label = "Target Year",
                                                                                                              value = max(init_indicator_fact$Year[init_indicator_fact$initiative==selectedinitiative &
                                                                                                                                                     substr(init_indicator_fact$indicatorkey,7,7)=="4" &
                                                                                                                                                     substr(init_indicator_fact$indicatorkey,1,1)=="2" &
                                                                                                                                                     init_indicator_fact$timestamp==max(init_indicator_fact$timestamp[
                                                                                                                                                       which(init_indicator_fact$initiative==selectedinitiative)])])),
                                                                                                 numericInput(inputId = "path2subcat1target",
                                                                                                              label = "Target Value",
                                                                                                              value = init_indicator_dim$subcattarget[init_indicator_dim$initiative==selectedinitiative &
                                                                                                                                                        substr(init_indicator_dim$indicatorkey,7,7)=="4" &
                                                                                                                                                        substr(init_indicator_dim$indicatorkey,1,1)=="2" &
                                                                                                                                                        init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                                                                                          which(init_indicator_dim$initiative==selectedinitiative)])]),
                                                                                                 DT::dataTableOutput(outputId = "path2subcat1data"),
                                                                                                 tags$hr(),
                                                                                                 h5(tags$b("Trend 2")),
                                                                                                 textInput(inputId = "path2subcat2",
                                                                                                           label = "Label",
                                                                                                           value = init_indicator_dim$subcat[init_indicator_dim$initiative==selectedinitiative &
                                                                                                                                               substr(init_indicator_dim$indicatorkey,7,7)=="5" &
                                                                                                                                               substr(init_indicator_dim$indicatorkey,1,1)=="2" &
                                                                                                                                               init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                                                                                 which(init_indicator_dim$initiative==selectedinitiative)])]),
                                                                                                 numericInput(inputId = "path2subcat2year",
                                                                                                              label = "Target Year",
                                                                                                              value = max(init_indicator_fact$Year[init_indicator_fact$initiative==selectedinitiative &
                                                                                                                                                     substr(init_indicator_fact$indicatorkey,7,7)=="5" &
                                                                                                                                                     substr(init_indicator_fact$indicatorkey,1,1)=="2" &
                                                                                                                                                     init_indicator_fact$timestamp==max(init_indicator_fact$timestamp[
                                                                                                                                                       which(init_indicator_fact$initiative==selectedinitiative)])])),
                                                                                                 numericInput(inputId = "path2subcat2target",
                                                                                                              label = "Target Value",
                                                                                                              value = init_indicator_dim$subcattarget[init_indicator_dim$initiative==selectedinitiative &
                                                                                                                                                        substr(init_indicator_dim$indicatorkey,7,7)=="5" &
                                                                                                                                                        substr(init_indicator_dim$indicatorkey,1,1)=="2" &
                                                                                                                                                        init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                                                                                          which(init_indicator_dim$initiative==selectedinitiative)])]),
                                                                                                 tags$hr(),
                                                                                                 h5(tags$b("Trend 3")),
                                                                                                 textInput(inputId = "path2subcat3",
                                                                                                           label = "Label",
                                                                                                           value = init_indicator_dim$subcat[init_indicator_dim$initiative==selectedinitiative &
                                                                                                                                               substr(init_indicator_dim$indicatorkey,7,7)=="6" &
                                                                                                                                               substr(init_indicator_dim$indicatorkey,1,1)=="2" &
                                                                                                                                               init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                                                                                 which(init_indicator_dim$initiative==selectedinitiative)])]),
                                                                                                 numericInput(inputId = "path2subcat3year",
                                                                                                              label = "Target Year",
                                                                                                              value = max(init_indicator_fact$Year[init_indicator_fact$initiative==selectedinitiative &
                                                                                                                                                     substr(init_indicator_fact$indicatorkey,7,7)=="6" &
                                                                                                                                                     substr(init_indicator_fact$indicatorkey,1,1)=="2" &
                                                                                                                                                     init_indicator_fact$timestamp==max(init_indicator_fact$timestamp[
                                                                                                                                                       which(init_indicator_fact$initiative==selectedinitiative)])])),
                                                                                                 numericInput(inputId = "path2subcat3target",
                                                                                                              label = "Target Value",
                                                                                                              value = init_indicator_dim$subcattarget[init_indicator_dim$initiative==selectedinitiative &
                                                                                                                                                        substr(init_indicator_dim$indicatorkey,7,7)=="6" &
                                                                                                                                                        substr(init_indicator_dim$indicatorkey,1,1)=="2" &
                                                                                                                                                        init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                                                                                          which(init_indicator_dim$initiative==selectedinitiative)])]))
                                                                                       }
                  }
      }
    })
  }