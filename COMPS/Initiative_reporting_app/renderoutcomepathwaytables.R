
# code: render outcome and pathway data tables


# ---- render tables ----

observe({ 
  
  # OUTCOME 1: 
  # subcat 1 datatable 
  output$out1subcat1data <- DT::renderDataTable({
    datatable(out1subcat1(), escape = F, editable = T, selection = 'none', 
              options = list(dom = 't', pageLength = 50),
              rownames = F)
  }, server = T)
  
  # subcat 2 datatable
  output$out1subcat2data <- DT::renderDataTable({
    datatable(out1subcat2(), escape = F, editable = T, selection = 'none', 
              options = list(dom = 't', pageLength = 50),
              rownames = F)
  }, server = T)
  
  # subcat 3 datatable 
  output$out1subcat3data <- DT::renderDataTable({
    datatable(out1subcat3(), escape = F, editable = T, selection = 'none', 
              options = list(dom = 't', pageLength = 50),
              rownames = F)
  }, server = T)
  
  
  # OUTCOME 2: 
  # subcat 1 datatable 
  output$out2subcat1data <- DT::renderDataTable({
    datatable(out2subcat1(), escape = F, editable = T, selection = 'none', 
              options = list(dom = 't', pageLength = 50),
              rownames = F)
  }, server = T)
  
  # subcat 2 datatable
  output$out2subcat2data <- DT::renderDataTable({
    datatable(out2subcat2(), escape = F, editable = T, selection = 'none', 
              options = list(dom = 't', pageLength = 50),
              rownames = F)
  }, server = T)
  
  # subcat 3 datatable 
  output$out2subcat3data <- DT::renderDataTable({
    datatable(out2subcat3(), escape = F, editable = T, selection = 'none', 
              options = list(dom = 't', pageLength = 50),
              rownames = F)
  }, server = T)
  
  
  # PATHWAY 1: 
  # subcat 1 datatable 
  output$path1subcat1data <- DT::renderDataTable({
    datatable(path1subcat1(), escape = F, editable = T, selection = 'none', 
              options = list(dom = 't', pageLength = 50),
              rownames = F)
  }, server = T)
  
  # subcat 2 datatable
  output$path1subcat2data <- DT::renderDataTable({
    datatable(path1subcat2(), escape = F, editable = T, selection = 'none', 
              options = list(dom = 't', pageLength = 50),
              rownames = F)
  }, server = T)
  
  # subcat 3 datatable 
  output$path1subcat3data <- DT::renderDataTable({
    datatable(path1subcat3(), escape = F, editable = T, selection = 'none', 
              options = list(dom = 't', pageLength = 50),
              rownames = F)
  }, server = T)
  
  
  # PATHWAY 2: 
  # subcat 1 datatable 
  output$path2subcat1data <- DT::renderDataTable({
    datatable(path2subcat1(), escape = F, editable = T, selection = 'none', 
              options = list(dom = 't', pageLength = 50),
              rownames = F)
  }, server = T)
  
  # subcat 2 datatable
  output$path2subcat2data <- DT::renderDataTable({
    datatable(path2subcat2(), escape = F, editable = T, selection = 'none', 
              options = list(dom = 't', pageLength = 50),
              rownames = F)
  }, server = T)
  
  # subcat 3 datatable 
  output$path2subcat3data <- DT::renderDataTable({
    datatable(path2subcat3(), escape = F, editable = T, selection = 'none', 
              options = list(dom = 't', pageLength = 50),
              rownames = F)
  }, server = T)
  
})


# ---- update reactive df when cell edited for each outcome and pathway subcategory table ----

# OUTCOME 1 save edits:
observeEvent(input[["out1subcat1data_cell_edit"]], {
  
  cell <- input[["out1subcat1data_cell_edit"]]
  newdf <- out1subcat1()
  newdf[cell$row, cell$col+1] <- cell$value
  out1subcat1(newdf)
  
})

observeEvent(input[["out1subcat2data_cell_edit"]], {
  
  cell <- input[["out1subcat2data_cell_edit"]]
  newdf <- out1subcat2()
  newdf[cell$row, cell$col+1] <- cell$value
  out1subcat2(newdf)
  
})

observeEvent(input[["out1subcat3data_cell_edit"]], {
  
  cell <- input[["out1subcat3data_cell_edit"]]
  newdf <- out1subcat3()
  newdf[cell$row, cell$col+1] <- cell$value
  out1subcat3(newdf)
  
})

# OUTCOME 2 save edits:
observeEvent(input[["out2subcat1data_cell_edit"]], {
  
  cell <- input[["out2subcat1data_cell_edit"]]
  newdf <- out2subcat1()
  newdf[cell$row, cell$col+1] <- cell$value
  out2subcat1(newdf)
  
})

observeEvent(input[["out2subcat2data_cell_edit"]], {
  
  cell <- input[["out2subcat2data_cell_edit"]]
  newdf <- out2subcat2()
  newdf[cell$row, cell$col+1] <- cell$value
  out2subcat2(newdf)
  
})

observeEvent(input[["out2subcat3data_cell_edit"]], {
  
  cell <- input[["out2subcat3data_cell_edit"]]
  newdf <- out2subcat3()
  newdf[cell$row, cell$col+1] <- cell$value
  out2subcat3(newdf)
  
})

# PATHWAY 1 save edits:
observeEvent(input[["path1subcat1data_cell_edit"]], {
  
  cell <- input[["path1subcat1data_cell_edit"]]
  newdf <- path1subcat1()
  newdf[cell$row, cell$col+1] <- cell$value
  path1subcat1(newdf)
  
})

observeEvent(input[["path1subcat2data_cell_edit"]], {
  
  cell <- input[["path1subcat2data_cell_edit"]]
  newdf <- path1subcat2()
  newdf[cell$row, cell$col+1] <- cell$value
  path1subcat2(newdf)
  
})

observeEvent(input[["path1subcat3data_cell_edit"]], {
  
  cell <- input[["path1subcat3data_cell_edit"]]
  newdf <- path1subcat3()
  newdf[cell$row, cell$col+1] <- cell$value
  path1subcat3(newdf)
  
})

# PATHWAY 2 save edits:
observeEvent(input[["path2subcat1data_cell_edit"]], {
  
  cell <- input[["path2subcat1data_cell_edit"]]
  newdf <- path2subcat1()
  newdf[cell$row, cell$col+1] <- cell$value
  path2subcat1(newdf)
  
})

observeEvent(input[["path2subcat2data_cell_edit"]], {
  
  cell <- input[["path2subcat2data_cell_edit"]]
  newdf <- path2subcat2()
  newdf[cell$row, cell$col+1] <- cell$value
  path2subcat2(newdf)
  
})

observeEvent(input[["path2subcat3data_cell_edit"]], {
  
  cell <- input[["path2subcat3data_cell_edit"]]
  newdf <- path2subcat3()
  newdf[cell$row, cell$col+1] <- cell$value
  path2subcat3(newdf)
  
})


# ---- each time addData is pressed, add data to reactive df ----

# OUTCOME 1 add row:
observeEvent(input[["addout1subcat1Data"]], {
  
  oldout1subcat1table <- out1subcat1()
  outcomesubcatrow <- data.frame(Year = 9999,
                                 Value = NA)
  out1subcat1(rbind.data.frame(oldout1subcat1table,outcomesubcatrow))
  
})

observeEvent(input[["addout1subcat2Data"]], {
  
  oldout1subcat2table <- out1subcat2()
  outcomesubcatrow <- data.frame(Year = 9999,
                                 Value = NA)
  out1subcat2(rbind.data.frame(oldout1subcat2table,outcomesubcatrow))
  
})

observeEvent(input[["addout1subcat3Data"]], {
  
  oldout1subcat3table <- out1subcat3()
  outcomesubcatrow <- data.frame(Year = 9999,
                                 Value = NA)
  out1subcat3(rbind.data.frame(oldout1subcat3table,outcomesubcatrow))
  
})

# OUTCOME 2 add row:
observeEvent(input[["addout2subcat1Data"]], {
  
  oldout2subcat1table <- out2subcat1()
  outcomesubcatrow <- data.frame(Year = 9999,
                                 Value = NA)
  out2subcat1(rbind.data.frame(oldout2subcat1table,outcomesubcatrow))
  
})

observeEvent(input[["addout2subcat2Data"]], {
  
  oldout2subcat2table <- out2subcat2()
  outcomesubcatrow <- data.frame(Year = 9999,
                                 Value = NA)
  out2subcat2(rbind.data.frame(oldout2subcat2table,outcomesubcatrow))
  
})

observeEvent(input[["addout2subcat3Data"]], {
  
  oldout2subcat3table <- out2subcat3()
  outcomesubcatrow <- data.frame(Year = 9999,
                                 Value = NA)
  out2subcat3(rbind.data.frame(oldout2subcat3table,outcomesubcatrow))
  
})

# PATHWAY 1 add row:
observeEvent(input[["addpath1subcat1Data"]], {
  
  oldpath1subcat1table <- path1subcat1()
  outcomesubcatrow <- data.frame(Year = 9999,
                                 Value = NA)
  path1subcat1(rbind.data.frame(oldpath1subcat1table,outcomesubcatrow))
  
})

observeEvent(input[["addpath1subcat2Data"]], {
  
  oldpath1subcat2table <- path1subcat2()
  outcomesubcatrow <- data.frame(Year = 9999,
                                 Value = NA)
  path1subcat2(rbind.data.frame(oldpath1subcat2table,outcomesubcatrow))
  
})

observeEvent(input[["addpath1subcat3Data"]], {
  
  oldpath1subcat3table <- path1subcat3()
  outcomesubcatrow <- data.frame(Year = 9999,
                                 Value = NA)
  path1subcat3(rbind.data.frame(oldpath1subcat3table,outcomesubcatrow))
  
})

# PATHWAY 2 add row:
observeEvent(input[["addpath2subcat1Data"]], {
  
  oldpath2subcat1table <- path2subcat1()
  outcomesubcatrow <- data.frame(Year = 9999,
                                 Value = NA)
  path2subcat1(rbind.data.frame(oldpath2subcat1table,outcomesubcatrow))
  
})

observeEvent(input[["addpath2subcat2Data"]], {
  
  oldpath2subcat2table <- path2subcat2()
  outcomesubcatrow <- data.frame(Year = 9999,
                                 Value = NA)
  path2subcat2(rbind.data.frame(oldpath2subcat2table,outcomesubcatrow))
  
})

observeEvent(input[["addpath2subcat3Data"]], {
  
  oldpath2subcat3table <- path2subcat3()
  outcomesubcatrow <- data.frame(Year = 9999,
                                 Value = NA)
  path2subcat3(rbind.data.frame(oldpath2subcat3table,outcomesubcatrow))
  
})