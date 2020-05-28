#
# code:  WWF COMPS Initiative Report Shiny Data Entry App
#
# author: Louise Glew, louise.glew@wwf.org; Kelly Claborn, clabornkelly@gmail.com
# created: March 2020
# modified: 



source('COMPS/global.R',local=F)


# ---- Define UI for initiative reporting app ----

ui <- fluidPage(
  
  # Application title & intro
  titlePanel(tags$b("WWF Initiative Report")),
  
  tags$div(
    tags$br(),
    tags$h5("Welcome to the new platform for WWF-US initiative reporting.  To have your report pre-populated with your most recently completed report, please select your Goal team and Initiative name from the drop-down menus below."),
    tags$h5(tags$b("For initiative reporting support, please contact louise.glew@wwf.org or kelly.claborn@wwf.org")),
    tags$hr(
      tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
    tags$br()
  ),
  
  # drop-down menus for goal and initiative selection
  tags$div(
    tags$h4(tags$em("Select your initiative here:")),
    selectInput(inputId = "goal",
                label = "Goal Team",
                choices = c("","Climate","Food","Forests","Freshwater","Oceans","Wildlife"),
                selectize = T),
    selectInput(inputId = "initiativeoptions",
                label = "Initiative Name",
                choices = "",
                selectize = T),
    actionButton(inputId = "populatereport",
                 label = "Pre-populate Report"),
    tags$br(), 
    tags$br(),
    tags$hr(tags$style(HTML("hr {border-top: 1px solid #000000;}")))
  ),
  
  # Sidebar to provide more instructions
  sidebarLayout(
    sidebarPanel(
      h5("General instructions here..."),
      br(),
      h5("More info..."),
      width=2),
    
    # Define tabs
    mainPanel(
      tabsetPanel(
        type="tabs",
        
        tabPanel(
          tags$b("Initiative Information"),
          textInput(inputId = "initiative",
                    label = "Initiative Name"),
          textInput(inputId = "initiativelead",
                    label = "Name of Initiative Lead"),
          textInput(inputId = "email",
                    label = "Initiative Lead Email"),
          textInput(inputId = "initiativestart",
                    label = "Initiative Start Year",
                    placeholder = "Year (YYYY)"),
          textInput(inputId = "initiativeend",
                    label = "Initiative End Year",
                    placeholder = "Year (YYYY)"),
          textInput(inputId = "initiativestatement",
                    label = "Initiative Statement"),
          h5("Guidance on initiative statement"),
          selectInput("initiativestatus", "Initative Status Assessment Category", c("","Opportunity","Progress","Barrier","Support","Contingent")),
          #popify(selectInput("Overall.status", "Initative Status Assessment Category", c("","Opportunity","Progress","Barrier","Support","Contingent")), "Status Assessment Categories",
          #       "<b> Opportunity: </b> Critical opportunity to scale or leverage Initiative. <br/>
          #      <b> Progress: </b> On track or affected by minor issues that are being addressed by Initiative team. <br/>
          #      <b> Barrier: </b> Implementation is delayed by external factors (e.g., political will, partners engagement). <br/>
          #      <b> Support: </b> Initiative requires leadership support to address one or more issues. <br/>
          #      <b> Contingent: </b> Contingent on achievement of preceding milestone.", placement = "bottom", trigger = "hover"),
          textInput("initiativejust", "Initiative Status Assessment Justification"),
          h5 ("Guidance on initiative status justification")
        ),
        
        tabPanel(
          tags$b("Outcomes"),
          column(6,
                 tags$br(),
                 tags$b("OUTCOME 1"),
                 tags$br(),
                 textAreaInput(inputId = "outcome1statement",
                               label = "Outcome Statement",
                               placeholder = "Maximum of XX characters",
                               width = "100%",
                               height = "100px"),
                 h5("<INSERT INSTRUCTIONS ON OUTCOME STATEMENTS>"),
                 textInput(inputId ="out1indicator", 
                           label = "Indicator Description", 
                           placeholder = "A description of your indicator"),
                 textInput(inputId = "out1indicatorlabel", 
                           label = "Indicator Label"),
                 textInput(inputId = "out1indicatorunits", 
                           label= "Indicator Units"),
                 textInput(inputId = "out1indicatorsource",
                           label = "Indicator Data Source"),
                 selectInput(inputId = "out1numtrend",
                               label = "How many trend lines are there?",
                               choices = c("",1,2,3)),
                 uiOutput("out1numtrend")
                 ),
          column(6,
                 tags$br(),
                 tags$b("OUTCOME 2"),
                 tags$br(),
                 textAreaInput(inputId = "outcome2statement",
                               label = "Outcome Statement",
                               placeholder = "Maximum of XX characters",
                               width = "100%",
                               height = "100px"),
                 h5("<INSERT INSTRUCTIONS ON OUTCOME STATEMENTS>"),
                 textInput(inputId ="out2indicator", 
                           label = "Indicator Description", 
                           placeholder = "A description of your indicator"),
                 textInput(inputId = "out2indicatorlabel", 
                           label = "Indicator Label"),
                 textInput(inputId = "out2indicatorunits", 
                           label= "Indicator Units"),
                 textInput(inputId = "out2indicatorsource",
                           label = "Indicator Data Source"),
                 h5(tags$em("You may display up to three trend lines per outcome. Please indicate the labels for disaggregation (if any) 
                            you would like to display for this outcome in the boxes marked 'Trend 1', 'Trend 2', and 'Trend 3' below. 
                            If no aggregation is desired, please fill in the appropriate data for your indicator in the column for 'Trend 1' in the table below.")),
                 textInput(inputId = "out2subcat1",
                           label = "Trend 1 Label",
                           placeholder = "Leave blank if no disaggregation"),
                 textInput(inputId = "out2subcat2",
                           label = "Trend 2 Label",
                           placeholder = "Leave blank if no disaggregation"),
                 textInput(inputId = "out2subcat3",
                           label = "Trend 3 Label",
                           placeholder = "Leave blank if no disaggregation")
          )),
        
        tabPanel(
          tags$b("Pathways"),
          h5("PATHWAY 1"),
          h5("PATHWAY 2")
        ),
        
        tabPanel(
          tags$b("Milestones"),
          DT::dataTableOutput(outputId = "milestonedataoutput"),
          actionButton(inputId = "addMilestoneData",
                       label = "Add Row")
        ),
        
        tabPanel(
          tags$b("Financial Information"),
          h5(tags$em("What is the total cost budget for FY21-FY23?")),
          numericInput(inputId = "fundsneeded",
                       label = "Total Cost Budget",
                       value = NULL),
          numericInput(inputId = "fundssecured",
                       label = "Total Funds Secured",
                       value = NULL),
          numericInput(inputId = "fundsanticipated",
                       label = "Total Funds Anticipated", 
                       value = NULL)
        )
        
      )#tabset
    ) #main panel
  ), # side bar
  
  
  tags$div(
    tags$br(),
    tags$p("Please click 'Save' before ending your session -- however, do not click save until you are ready to leave the webpage!  You will be able to return to your work next time by selecting your initiative at the top and 'Pre-populating' the report."),
    column(2,actionButton(inputId = "save",
                          label = "Save",
                          icon('save'))),
    column(2,textOutput(outputId = "confirmsave"),
           tags$head(tags$style("#text1{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"))
           ),
    tags$br()
  )
  
 ) # fluid page



# ---- Define server logic ----

server <- function(input, output, session) {

  initiative_dim <<- import('COMPS/responses/FY21_initiative_dim.csv', colClasses = c(initiativekey = "character")) 
  init_indicator_dim <<- import('COMPS/responses/FY21_init_indicator_dim.csv')
  init_indicator_fact <<- import('COMPS/responses/FY21_init_indicator_fact.csv')
  milestones <<- import('COMPS/responses/FY21_milestones.csv', colClasses = c(target = "character")) %>% 
    mutate(milestonestatus = factor(milestonestatus, levels = c("","Opportunity","Progress","Barrier","Support","Contingent"), ordered = T))
  

  # update initiative selection options based on selected goal
  observe ({ 
    
    initiativeoptions <- initiative_dim %>% filter(goal==input$goal) %>% 
                             group_by(initiativekey) %>% summarise(initiative=initiative[timestamp==max(timestamp)]) 
    
    updateSelectInput(session, 
                      inputId = "initiativeoptions",
                      choices = c("",sort(initiativeoptions$initiative)),
                      selected = "")

  })

  # pre-populate text boxes based on selected initiatives
  observeEvent(input$populatereport, {
    
    source('COMPS/prepopulate.R',local=F)
    
    selectedinitiative <- input$initiativeoptions
    prePopulate(selectedinitiative, session)
    
  })

  # source conditional trend line panel info (to either pre-populate or leave blank)
  observeEvent(input$out1numtrend, {
    source('COMPS/conditionalpanels.R',local=F)
    
    selectedinitiative <- input$initiativeoptions
    outcome1Conditional(selectedinitiative, input, output)
    
  })
  
  # render data tables for outcome data 
  observe({
    
    selectedinitiative <- input$initiativeoptions
    initiativekey <- as.character(initiative_dim$initiativekey[initiative_dim$initiative==input$initiativeoptions])
    
    outcomedata <- reactiveValues()
    outcomedata$out1subcat1 <- data.frame(Year=init_indicator_fact$Year[init_indicator_fact$indicatorkey==paste("1",initiativekey,"01",sep="") &
                                                                          init_indicator_fact$Year != max(init_indicator_fact$Year[
                                                                            init_indicator_fact$indicatorkey==paste("1",initiativekey,"01",sep="")]) &
                                                                          init_indicator_fact$timestamp==max(init_indicator_fact$timestamp[
                                                                            which(init_indicator_fact$initiative==selectedinitiative)])],
                                          Value=init_indicator_fact$Value[init_indicator_fact$indicatorkey==paste("1",initiativekey,"01",sep="") &
                                                                            init_indicator_fact$Year != max(init_indicator_fact$Year[
                                                                              init_indicator_fact$indicatorkey==paste("1",initiativekey,"01",sep="")]) &
                                                                            init_indicator_fact$timestamp==max(init_indicator_fact$timestamp[
                                                                              which(init_indicator_fact$initiative==selectedinitiative)])])
    
    
    output$out1subcat1data <- renderDataTable({
      datatable(outcomedata$out1subcat1,
                editable = T, 
                options = list(paging = FALSE,
                               searching = FALSE, 
                               info = FALSE),
                rownames= FALSE)
    }, server=T)
    
  })
  

  # render data tables for milestones
  observe({
      
    data <- reactiveValues()  
    data <- milestones[which(milestones$initiative==input$initiativeoptions),c("milestone","target","milestonejust")]

    
    for (i in 1:nrow(data)) {
      data$status[i] <- as.character(selectInput(inputId = "milestonestatus", 
                                                 label = "", 
                                                 choices = sort(unique(milestones$milestonestatus)), width = "100px", 
                                                 selected = milestones$milestonestatus[milestones$initiative==input$initiativeoptions][i]))
    }
    
    for (i in 1:nrow(data)) {
      data$display[i] <- as.character(selectInput(inputId = "milestoneactive", 
                                                 label = "", 
                                                 choices = sort(unique(milestones$milestoneactive)), width = "100px", 
                                                 selected = milestones$milestoneactive[milestones$initiative==input$initiativeoptions][i]))
    }
    
    data <- data %>% .[,c("milestone","target","status","milestonejust","display")]
    colnames(data) <- c("Milestone","Target","Status","Status.Justification","Display.On.FY20.Dashboard")
    
    output$milestonedataoutput = DT::renderDataTable(
      data, escape = F, editable = T, selection = 'none', server = T,
      options = list(dom = 't', paging = F, ordering = F),
      rownames = F, 
      callback = JS("table.rows().every(function(i, tab, row) {
        var $this = $(this.node());
        $this.attr('id', this.data()[0]);
        $this.addClass('shiny-input-container');
      });
      Shiny.unbindAll(table.table().node());
      Shiny.bindAll(table.table().node());")
    )
    
    # milestonerow <- 
    #   data %>% 
    #   slice(1) %>% 
    #   # transpose the first row of data into two columns
    #   gather(key = "column_name", value = "value") %>%
    #   # replace all values with ""
    #   mutate(value = "") %>%
    #   # reshape the data from long to wide
    #   spread(column_name, value) %>%
    #   # rearrange the column order to match that of data
    #   select(colnames(data))
    # 
    # # store a proxy of milestone table 
    # proxy <- dataTableProxy(outputId = "milestonedataoutput")
    # 
    # # each time addData is pressed, add data to proxy
    # observeEvent(input$addMilestoneData, {
    #   proxy %>% 
    #     addRow(milestonerow)
    # })
    # 
  })
  

  
  
  # whenever a field is filled, aggregate all form data
  formData1 <- reactive({
    initiative_dim_data <- data.frame(t(as.matrix(sapply(initiative_dim_fields, function(x) input[[x]])))) %>%
      mutate(securedanticipatedsum = input$fundssecured+input$fundsanticipated,
             initiativekey = as.character(initiative_dim$initiativekey[initiative_dim$initiative==input$initiative]),
             displayorder = initiative_dim$displayorder[initiative_dim$initiative==input$initiative],
             globalinitiative = initiative_dim$globalinitiative[initiative_dim$initiative==input$initiative],
             usinitiative = initiative_dim$usinitiative[initiative_dim$initiative==input$initiative])
    initiative_dim_data
  })
  
  formData2 <- reactive({
    init_indicator_dim_data <- rbind.data.frame(t(as.matrix(sapply(out1_subcat1_indicator_dim_fields, function(x) input[[x]], USE.NAMES = F))),
                                                t(as.matrix(sapply(out1_subcat2_indicator_dim_fields, function(x) input[[x]], USE.NAMES = F))),
                                                t(as.matrix(sapply(out1_subcat3_indicator_dim_fields, function(x) input[[x]], USE.NAMES = F))),
                                                t(as.matrix(sapply(out2_subcat1_indicator_dim_fields, function(x) input[[x]], USE.NAMES = F))),
                                                t(as.matrix(sapply(out2_subcat2_indicator_dim_fields, function(x) input[[x]], USE.NAMES = F))),
                                                t(as.matrix(sapply(out2_subcat3_indicator_dim_fields, function(x) input[[x]], USE.NAMES = F))),
                                                t(as.matrix(sapply(path1_subcat1_indicator_dim_fields, function(x) input[[x]], USE.NAMES = F))),
                                                t(as.matrix(sapply(path1_subcat2_indicator_dim_fields, function(x) input[[x]], USE.NAMES = F))),
                                                t(as.matrix(sapply(path1_subcat3_indicator_dim_fields, function(x) input[[x]], USE.NAMES = F))),
                                                t(as.matrix(sapply(path2_subcat1_indicator_dim_fields, function(x) input[[x]], USE.NAMES = F))),
                                                t(as.matrix(sapply(path2_subcat2_indicator_dim_fields, function(x) input[[x]], USE.NAMES = F))),
                                                t(as.matrix(sapply(path2_subcat3_indicator_dim_fields, function(x) input[[x]], USE.NAMES = F)))) %>%
      mutate(indicatortype = c(rep("Outcome",6),rep("Pathway",6)),
             displayorder = c(rep(1,3),rep(2,3),rep(3,3),rep(4,3)),
             indicatorkey = as.numeric(paste(c(rep(1,6),rep(2,6),initiative_dim$initiativekey[initiative_dim$initiative==input$initiative],"0",c(seq(1:6),seq(1:6))))))
    colnames(init_indicator_dim_data) <- init_indicator_dim_fields
    init_indicator_dim_data
  })
  
  
  # define'saveData' function
  saveData <- function(data1,data2) {
    data1 <- rbind(initiative_dim, 
                   data1 %>% mutate(timestamp = format(Sys.time(), format="%Y%m%d%H%M")))
    data2 <- rbind(init_indicator_dim,
                   data2 %>% mutate(timestamp = format(Sys.time(), format="%Y%m%d%H%M")))
    
    # Write the files to the local system
    write.csv(
      x = data1,
      file = "COMPS/responses/FY21_initiative_dim.csv", 
      row.names = FALSE, quote = TRUE
    )
    
    write.csv(
      x = data2,
      file = "COMPS/responses/FY21_init_indicator_dim.csv",
      row.names = FALSE, quote = TRUE
    )
    
  }
  
  # ***TO DO: BEFORE SAVING, HAVE USERS HIT A 'REFRESH' BUTTON TO ENSURE THEY ARE WRITING THEIR DATA TO NEWEST VERSION OF FILE***
  # ***New window when saved?***
  
  # When the Save button is clicked, save the form data
  observeEvent(input$save, {
    saveData(formData1(),formData2())
    output$confirmsave <- renderText({ "Saved!" })
    initiative_dim <<- import('COMPS/responses/FY21_initiative_dim.csv', colClasses = c(initiativekey = "character"))
    init_indicator_dim <<- import('COMPS/responses/FY21_init_indicator_dim.csv')
  })

}

# ---- Run the application ----

shinyApp(ui = ui, server = server)




# For hover over of categories (look at shinyBS Tooltip)
# Tricky things -- character limits (might need to jump to Java)
  # add outcome disaggregation
  # add milestone editing functionality
  # timestamp & submit button 
#Milestone -editing functionality (finalize). 
#Font types
#Dropdown menus to select Goal Team and Initiative
#Directions/manual for initiative teams



