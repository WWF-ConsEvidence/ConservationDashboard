
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
                                                                    which(init_indicator_fact$initiative==selectedinitiative)])])),
                  numericInput(inputId = "out1subcat1target",
                               label = "Target Value",
                               value = init_indicator_dim$subcattarget[init_indicator_dim$initiative==selectedinitiative &
                                                                         init_indicator_dim$displayorder==1 &
                                                                         substr(init_indicator_dim$indicatorkey,7,7)=="1" & 
                                                                         init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                           which(init_indicator_dim$initiative==selectedinitiative)])]),
                  DT::dataTableOutput(outputId = "out1subcat1data"))) else {
                                 if(input$out1numtrend == 2) return(
                                   tagList(tags$hr(),
                                           h5(tags$b("Trend 1")),
                                           textInput(inputId = "out1subcat1",
                                                     label = "Label",
                                                     value = init_indicator_dim$subcat[init_indicator_dim$initiative==selectedinitiative &
                                                                                         init_indicator_dim$displayorder==1 &
                                                                                         substr(init_indicator_dim$indicatorkey,7,7)=="1" & 
                                                                                         init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                           which(init_indicator_dim$initiative==selectedinitiative)])]),
                                           numericInput(inputId = "out1subcat1year",
                                                        label = "Target Year",
                                                        value = max(init_indicator_fact$Year[init_indicator_fact$initiative==selectedinitiative &
                                                                                               substr(init_indicator_fact$indicatorkey,7,7)=="1" & 
                                                                                               substr(init_indicator_fact$indicatorkey,1,1)=="1" & 
                                                                                               init_indicator_fact$timestamp==max(init_indicator_fact$timestamp[
                                                                                                 which(init_indicator_fact$initiative==selectedinitiative)])])),
                                           numericInput(inputId = "out1subcat1target",
                                                        label = "Target Value",
                                                        value = init_indicator_dim$subcattarget[init_indicator_dim$initiative==selectedinitiative &
                                                                                                  init_indicator_dim$displayorder==1 &
                                                                                                  substr(init_indicator_dim$indicatorkey,7,7)=="1" & 
                                                                                                  init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                                    which(init_indicator_dim$initiative==selectedinitiative)])]),
                                           DT::dataTableOutput(outputId = "out1subcat1data"),
                                           tags$hr(),
                                           h5(tags$b("Trend 2")),
                                           textInput(inputId = "out1subcat2",
                                                     label = "Label",
                                                     value = init_indicator_dim$subcat[init_indicator_dim$initiative==selectedinitiative &
                                                                                         init_indicator_dim$displayorder==1 &
                                                                                         substr(init_indicator_dim$indicatorkey,7,7)=="2" & 
                                                                                         init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                           which(init_indicator_dim$initiative==selectedinitiative)])]),
                                           numericInput(inputId = "out1subcat2year",
                                                        label = "Target Year",
                                                        value = max(init_indicator_fact$Year[init_indicator_fact$initiative==selectedinitiative &
                                                                                               substr(init_indicator_fact$indicatorkey,7,7)=="2" & 
                                                                                               substr(init_indicator_fact$indicatorkey,1,1)=="1" & 
                                                                                               init_indicator_fact$timestamp==max(init_indicator_fact$timestamp[
                                                                                                 which(init_indicator_fact$initiative==selectedinitiative)])])),
                                           numericInput(inputId = "out1subcat2target",
                                                        label = "Target Value",
                                                        value = init_indicator_dim$subcattarget[init_indicator_dim$initiative==selectedinitiative &
                                                                                                  init_indicator_dim$displayorder==1 &
                                                                                                  substr(init_indicator_dim$indicatorkey,7,7)=="2" & 
                                                                                                  init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                                    which(init_indicator_dim$initiative==selectedinitiative)])]))) else {
                                                          tagList(tags$hr(),
                                                                  h5(tags$b("Trend 1")),
                                                                  textInput(inputId = "out1subcat1",
                                                                            label = "Label",
                                                                            value = init_indicator_dim$subcat[init_indicator_dim$initiative==selectedinitiative &
                                                                                                                init_indicator_dim$displayorder==1 &
                                                                                                                substr(init_indicator_dim$indicatorkey,7,7)=="1" & 
                                                                                                                init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                                                  which(init_indicator_dim$initiative==selectedinitiative)])]),
                                                                  numericInput(inputId = "out1subcat1year",
                                                                               label = "Target Year",
                                                                               value = max(init_indicator_fact$Year[init_indicator_fact$initiative==selectedinitiative &
                                                                                                                      substr(init_indicator_fact$indicatorkey,7,7)=="1" & 
                                                                                                                      substr(init_indicator_fact$indicatorkey,1,1)=="1" & 
                                                                                                                      init_indicator_fact$timestamp==max(init_indicator_fact$timestamp[
                                                                                                                        which(init_indicator_fact$initiative==selectedinitiative)])])),
                                                                  numericInput(inputId = "out1subcat1target",
                                                                               label = "Target Value",
                                                                               value = init_indicator_dim$subcattarget[init_indicator_dim$initiative==selectedinitiative &
                                                                                                                         init_indicator_dim$displayorder==1 &
                                                                                                                         substr(init_indicator_dim$indicatorkey,7,7)=="1" & 
                                                                                                                         init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                                                           which(init_indicator_dim$initiative==selectedinitiative)])]),
                                                                  DT::dataTableOutput(outputId = "out1subcat1data"),
                                                                  tags$hr(),
                                                                  h5(tags$b("Trend 2")),
                                                                  textInput(inputId = "out1subcat2",
                                                                            label = "Label",
                                                                            value = init_indicator_dim$subcat[init_indicator_dim$initiative==selectedinitiative &
                                                                                                                init_indicator_dim$displayorder==1 &
                                                                                                                substr(init_indicator_dim$indicatorkey,7,7)=="2" & 
                                                                                                                init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                                                  which(init_indicator_dim$initiative==selectedinitiative)])]),
                                                                  numericInput(inputId = "out1subcat2year",
                                                                               label = "Target Year",
                                                                               value = max(init_indicator_fact$Year[init_indicator_fact$initiative==selectedinitiative &
                                                                                                                      substr(init_indicator_fact$indicatorkey,7,7)=="2" & 
                                                                                                                      substr(init_indicator_fact$indicatorkey,1,1)=="1" & 
                                                                                                                      init_indicator_fact$timestamp==max(init_indicator_fact$timestamp[
                                                                                                                        which(init_indicator_fact$initiative==selectedinitiative)])])),
                                                                  numericInput(inputId = "out1subcat2target",
                                                                               label = "Target Value",
                                                                               value = init_indicator_dim$subcattarget[init_indicator_dim$initiative==selectedinitiative &
                                                                                                                         init_indicator_dim$displayorder==1 &
                                                                                                                         substr(init_indicator_dim$indicatorkey,7,7)=="2" & 
                                                                                                                         init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                                                           which(init_indicator_dim$initiative==selectedinitiative)])]),
                                                                  tags$hr(),
                                                                  h5(tags$b("Trend 3")),
                                                                  textInput(inputId = "out1subcat3",
                                                                            label = "Label",
                                                                            value = init_indicator_dim$subcat[init_indicator_dim$initiative==selectedinitiative &
                                                                                                                init_indicator_dim$displayorder==1 &
                                                                                                                substr(init_indicator_dim$indicatorkey,7,7)=="3" & 
                                                                                                                init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                                                  which(init_indicator_dim$initiative==selectedinitiative)])]),
                                                                  numericInput(inputId = "out1subcat3year",
                                                                               label = "Target Year",
                                                                               value = max(init_indicator_fact$Year[init_indicator_fact$initiative==selectedinitiative &
                                                                                                                      substr(init_indicator_fact$indicatorkey,7,7)=="3" & 
                                                                                                                      substr(init_indicator_fact$indicatorkey,1,1)=="1" & 
                                                                                                                      init_indicator_fact$timestamp==max(init_indicator_fact$timestamp[
                                                                                                                        which(init_indicator_fact$initiative==selectedinitiative)])])),
                                                                  numericInput(inputId = "out1subcat3target",
                                                                               label = "Target Value",
                                                                               value = init_indicator_dim$subcattarget[init_indicator_dim$initiative==selectedinitiative &
                                                                                                                         init_indicator_dim$displayorder==1 &
                                                                                                                         substr(init_indicator_dim$indicatorkey,7,7)=="3" & 
                                                                                                                         init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[
                                                                                                                           which(init_indicator_dim$initiative==selectedinitiative)])]))
                                                        }
                               }
      }
    })
  }