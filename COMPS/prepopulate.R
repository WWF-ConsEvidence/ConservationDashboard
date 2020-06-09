
# code: function for initiative reporting app to pre-populate text boxes based on selected initiatives


# ---- define function ----

prePopulate <- function(selectedinitiative, session) {
    
    # initiative info
    updateTextInput(session,
                    inputId = "initiative",
                    value = selectedinitiative)
    
    updateTextInput(session,
                    inputId = "initiativelead",
                    value = initiative_dim$initiativelead[initiative_dim$initiative==selectedinitiative & 
                                                            initiative_dim$timestamp==max(initiative_dim$timestamp[which(initiative_dim$initiative==selectedinitiative)])])
    
    updateTextInput(session,
                    inputId = "email",
                    value = initiative_dim$email[initiative_dim$initiative==selectedinitiative & 
                                                   initiative_dim$timestamp==max(initiative_dim$timestamp[which(initiative_dim$initiative==selectedinitiative)])])
    
    updateTextInput(session,
                    inputId = "initiativestart",
                    value = initiative_dim$initiativestart[initiative_dim$initiative==selectedinitiative & 
                                                             initiative_dim$timestamp==max(initiative_dim$timestamp[which(initiative_dim$initiative==selectedinitiative)])])
    
    updateTextInput(session,
                    inputId = "initiativeend",
                    value = initiative_dim$initiativeend[initiative_dim$initiative==selectedinitiative & 
                                                           initiative_dim$timestamp==max(initiative_dim$timestamp[which(initiative_dim$initiative==selectedinitiative)])])
    
    updateTextInput(session,
                    inputId = "initiativestatement",
                    value = initiative_dim$initiativestatement[initiative_dim$initiative==selectedinitiative & 
                                                                 initiative_dim$timestamp==max(initiative_dim$timestamp[which(initiative_dim$initiative==selectedinitiative)])])
    
    updateNumericInput(session,
                       inputId = "fundsneeded",
                       value = initiative_dim$fundsneeded[initiative_dim$initiative==selectedinitiative & 
                                                                    initiative_dim$timestamp==max(initiative_dim$timestamp[which(initiative_dim$initiative==selectedinitiative)])])
    
    updateNumericInput(session,
                       inputId = "fundssecured",
                       value = initiative_dim$fundssecured[initiative_dim$initiative==selectedinitiative & 
                                                            initiative_dim$timestamp==max(initiative_dim$timestamp[which(initiative_dim$initiative==selectedinitiative)])])
    
    updateNumericInput(session,
                       inputId = "fundsanticipated",
                       value = initiative_dim$fundsanticipated[initiative_dim$initiative==selectedinitiative & 
                                                            initiative_dim$timestamp==max(initiative_dim$timestamp[which(initiative_dim$initiative==selectedinitiative)])])
    
    # outcome 1 info
    updateTextInput(session,
                    inputId = "outcome1statement",
                    value = init_indicator_dim$statement[init_indicator_dim$initiative==selectedinitiative &
                                                           init_indicator_dim$displayorder==1 &
                                                         substr(init_indicator_dim$indicatorkey,7,7)=="1" & 
                                                           init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[which(init_indicator_dim$initiative==selectedinitiative)])])
    
    updateTextInput(session,
                    inputId ="out1indicator",
                    value = init_indicator_dim$indicatordescription[init_indicator_dim$initiative==selectedinitiative &
                                                                      init_indicator_dim$displayorder==1 &
                                                                    substr(init_indicator_dim$indicatorkey,7,7)=="1" & 
                                                                      init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[which(init_indicator_dim$initiative==selectedinitiative)])])
    
    updateTextInput(session,
                    inputId = "out1indicatorlabel", 
                    value = init_indicator_dim$indicatorlabel[init_indicator_dim$initiative==selectedinitiative &
                                                                init_indicator_dim$displayorder==1 &
                                                              substr(init_indicator_dim$indicatorkey,7,7)=="1" & 
                                                                init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[which(init_indicator_dim$initiative==selectedinitiative)])])
    
    updateTextInput(session,
                    inputId = "out1indicatorunits",
                    value = init_indicator_dim$indicatorunits[init_indicator_dim$initiative==selectedinitiative &
                                                                init_indicator_dim$displayorder==1 &
                                                              substr(init_indicator_dim$indicatorkey,7,7)=="1" & 
                                                                init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[which(init_indicator_dim$initiative==selectedinitiative)])])
    
    updateTextInput(session,
                    inputId = "out1indicatorsource",
                    value = init_indicator_dim$indicatorsource[init_indicator_dim$initiative==selectedinitiative &
                                                                 init_indicator_dim$displayorder==1 &
                                                               substr(init_indicator_dim$indicatorkey,7,7)=="1" & 
                                                                 init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[which(init_indicator_dim$initiative==selectedinitiative)])])
    
    updateSelectInput(session,
                      inputId = "out1numtrend",
                      selected = ifelse(length(init_indicator_dim$subcat[init_indicator_dim$initiative==selectedinitiative & 
                                                                           init_indicator_dim$displayorder==1 &
                                                                           (!is.na(init_indicator_dim$subcat) & init_indicator_dim$subcat!="") & 
                                                                           init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[which(init_indicator_dim$initiative==selectedinitiative)])])==0,1,
                                        length(init_indicator_dim$subcat[init_indicator_dim$initiative==selectedinitiative & 
                                                                           init_indicator_dim$displayorder==1 &
                                                                           (!is.na(init_indicator_dim$subcat) & init_indicator_dim$subcat!="") & 
                                                                           init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[which(init_indicator_dim$initiative==selectedinitiative)])])))

    
    # outcome 2 info
    updateTextInput(session,
                    inputId = "outcome2statement",
                    value = init_indicator_dim$statement[init_indicator_dim$initiative==selectedinitiative &
                                                           init_indicator_dim$displayorder==2 &
                                                         substr(init_indicator_dim$indicatorkey,7,7)=="4" & 
                                                           init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[which(init_indicator_dim$initiative==selectedinitiative)])])
    
    updateTextInput(session,
                    inputId ="out2indicator",
                    value = init_indicator_dim$indicatordescription[init_indicator_dim$initiative==selectedinitiative &
                                                                      init_indicator_dim$displayorder==2 &
                                                                    substr(init_indicator_dim$indicatorkey,7,7)=="4" & 
                                                                      init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[which(init_indicator_dim$initiative==selectedinitiative)])])
    
    updateTextInput(session,
                    inputId = "out2indicatorlabel", 
                    value = init_indicator_dim$indicatorlabel[init_indicator_dim$initiative==selectedinitiative &
                                                                init_indicator_dim$displayorder==2 &
                                                              substr(init_indicator_dim$indicatorkey,7,7)=="4" & 
                                                                init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[which(init_indicator_dim$initiative==selectedinitiative)])])
    
    updateTextInput(session,
                    inputId = "out2indicatorunits",
                    value = init_indicator_dim$indicatorunits[init_indicator_dim$initiative==selectedinitiative &
                                                                init_indicator_dim$displayorder==2 &
                                                              substr(init_indicator_dim$indicatorkey,7,7)=="4" & 
                                                                init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[which(init_indicator_dim$initiative==selectedinitiative)])])
    
    updateTextInput(session,
                    inputId = "out2indicatorsource",
                    value = init_indicator_dim$indicatorsource[init_indicator_dim$initiative==selectedinitiative &
                                                                 init_indicator_dim$displayorder==2 &
                                                               substr(init_indicator_dim$indicatorkey,7,7)=="4" & 
                                                                 init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[which(init_indicator_dim$initiative==selectedinitiative)])])
    
    updateSelectInput(session,
                      inputId = "out2numtrend",
                      selected = ifelse(length(init_indicator_dim$subcat[init_indicator_dim$initiative==selectedinitiative & 
                                                                    init_indicator_dim$displayorder==2 &
                                                                    (!is.na(init_indicator_dim$subcat) & init_indicator_dim$subcat!="") & 
                                                                    init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[which(init_indicator_dim$initiative==selectedinitiative)])])==0,1,
                                        length(init_indicator_dim$subcat[init_indicator_dim$initiative==selectedinitiative & 
                                                                           init_indicator_dim$displayorder==2 &
                                                                           (!is.na(init_indicator_dim$subcat) & init_indicator_dim$subcat!="") & 
                                                                           init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[which(init_indicator_dim$initiative==selectedinitiative)])])))
    
    
    # pathway 1 info
    updateTextInput(session,
                    inputId = "pathway1statement",
                    value = init_indicator_dim$statement[init_indicator_dim$initiative==selectedinitiative &
                                                           init_indicator_dim$displayorder==3 &
                                                           substr(init_indicator_dim$indicatorkey,7,7)=="1" & 
                                                           init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[which(init_indicator_dim$initiative==selectedinitiative)])])
    
    updateTextInput(session,
                    inputId ="path1indicator",
                    value = init_indicator_dim$indicatordescription[init_indicator_dim$initiative==selectedinitiative &
                                                                      init_indicator_dim$displayorder==3 &
                                                                      substr(init_indicator_dim$indicatorkey,7,7)=="1" & 
                                                                      init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[which(init_indicator_dim$initiative==selectedinitiative)])])
    
    updateTextInput(session,
                    inputId = "path1indicatorlabel", 
                    value = init_indicator_dim$indicatorlabel[init_indicator_dim$initiative==selectedinitiative &
                                                                init_indicator_dim$displayorder==3 &
                                                                substr(init_indicator_dim$indicatorkey,7,7)=="1" & 
                                                                init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[which(init_indicator_dim$initiative==selectedinitiative)])])
    
    updateTextInput(session,
                    inputId = "path1indicatorunits",
                    value = init_indicator_dim$indicatorunits[init_indicator_dim$initiative==selectedinitiative &
                                                                init_indicator_dim$displayorder==3 &
                                                                substr(init_indicator_dim$indicatorkey,7,7)=="1" & 
                                                                init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[which(init_indicator_dim$initiative==selectedinitiative)])])
    
    updateTextInput(session,
                    inputId = "path1indicatorsource",
                    value = init_indicator_dim$indicatorsource[init_indicator_dim$initiative==selectedinitiative &
                                                                 init_indicator_dim$displayorder==3 &
                                                                 substr(init_indicator_dim$indicatorkey,7,7)=="1" & 
                                                                 init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[which(init_indicator_dim$initiative==selectedinitiative)])])
    
    updateSelectInput(session,
                      inputId = "path1numtrend",
                      selected = ifelse(length(init_indicator_dim$subcat[init_indicator_dim$initiative==selectedinitiative & 
                                                                           init_indicator_dim$displayorder==3 &
                                                                           (!is.na(init_indicator_dim$subcat) & init_indicator_dim$subcat!="") & 
                                                                           init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[which(init_indicator_dim$initiative==selectedinitiative)])])==0,1,
                                        length(init_indicator_dim$subcat[init_indicator_dim$initiative==selectedinitiative & 
                                                                           init_indicator_dim$displayorder==3 &
                                                                           (!is.na(init_indicator_dim$subcat) & init_indicator_dim$subcat!="") & 
                                                                           init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[which(init_indicator_dim$initiative==selectedinitiative)])])))
    
    
    # pathway 2 info
    updateTextInput(session,
                    inputId = "pathway2statement",
                    value = init_indicator_dim$statement[init_indicator_dim$initiative==selectedinitiative &
                                                           init_indicator_dim$displayorder==4 &
                                                           substr(init_indicator_dim$indicatorkey,7,7)=="4" & 
                                                           init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[which(init_indicator_dim$initiative==selectedinitiative)])])
    
    updateTextInput(session,
                    inputId ="path2indicator",
                    value = init_indicator_dim$indicatordescription[init_indicator_dim$initiative==selectedinitiative &
                                                                      init_indicator_dim$displayorder==4 &
                                                                      substr(init_indicator_dim$indicatorkey,7,7)=="4" & 
                                                                      init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[which(init_indicator_dim$initiative==selectedinitiative)])])
    
    updateTextInput(session,
                    inputId = "path2indicatorlabel", 
                    value = init_indicator_dim$indicatorlabel[init_indicator_dim$initiative==selectedinitiative &
                                                                init_indicator_dim$displayorder==4 &
                                                                substr(init_indicator_dim$indicatorkey,7,7)=="4" & 
                                                                init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[which(init_indicator_dim$initiative==selectedinitiative)])])
    
    updateTextInput(session,
                    inputId = "path2indicatorunits",
                    value = init_indicator_dim$indicatorunits[init_indicator_dim$initiative==selectedinitiative &
                                                                init_indicator_dim$displayorder==4 &
                                                                substr(init_indicator_dim$indicatorkey,7,7)=="4" & 
                                                                init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[which(init_indicator_dim$initiative==selectedinitiative)])])
    
    updateTextInput(session,
                    inputId = "path2indicatorsource",
                    value = init_indicator_dim$indicatorsource[init_indicator_dim$initiative==selectedinitiative &
                                                                 init_indicator_dim$displayorder==4 &
                                                                 substr(init_indicator_dim$indicatorkey,7,7)=="4" & 
                                                                 init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[which(init_indicator_dim$initiative==selectedinitiative)])])
    
    updateSelectInput(session,
                      inputId = "path2numtrend",
                      selected = ifelse(length(init_indicator_dim$subcat[init_indicator_dim$initiative==selectedinitiative & 
                                                                           init_indicator_dim$displayorder==4 &
                                                                           (!is.na(init_indicator_dim$subcat) & init_indicator_dim$subcat!="") & 
                                                                           init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[which(init_indicator_dim$initiative==selectedinitiative)])])==0,1,
                                        length(init_indicator_dim$subcat[init_indicator_dim$initiative==selectedinitiative & 
                                                                           init_indicator_dim$displayorder==4 &
                                                                           (!is.na(init_indicator_dim$subcat) & init_indicator_dim$subcat!="") & 
                                                                           init_indicator_dim$timestamp==max(init_indicator_dim$timestamp[which(init_indicator_dim$initiative==selectedinitiative)])])))
}