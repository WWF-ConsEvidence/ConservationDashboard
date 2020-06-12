#
# code:  WWF COMPS Initiative Report Shiny Data Entry App
#
# author: Louise Glew, louise.glew@wwf.org; Kelly Claborn, clabornkelly@gmail.com
# created: March 2020
# modified: 



source('COMPS/global.R',local=F)


# ---- Define UI for initiative reporting app ----

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  useShinyjs(),
  useShinyalert(),
  
  # Application title & intro
  titlePanel(h1(HTML("<strong>WWF-US FY20 Initiative Report</strong>"))),
  
  tags$div(
    tags$br(),
    tags$table(style = "width: 1000",
               tags$tr(tags$td(style = "width: 50%",
                               align = "left",
                               h4(HTML("<strong>Welcome to the new platform for WWF-US initiative reporting!</strong>   <br> <br> 
                                       <em>To have your report pre-populated with your most recently completed report, please select your Goal team and Initiative name from the drop-down menus below.</em>"))),
                       tags$td(style = "width: 50%",
                               align = "right",
                               p(HTML("<br> <br> <strong>For initiative reporting support, please contact <a href = 'mailto: louise.glew@wwf.org'>Louise Glew</a> or 
                                      <a href = 'mailto: kelly.claborn@wwf.org'>Kelly Claborn</a></strong>"))))),
    tags$hr(
      tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
    tags$br()
  ),
  
  # drop-down menus for goal and initiative selection
  tags$div(
    column(3, 
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
      tags$br()),
    column(9, img(src=Dashboard_intro, height = 600, width = 1400, align = "center")),
    h2("..."),
    tags$head(tags$style('h2 {color:white;}')),
    tags$hr(tags$style(HTML("hr {border-top: 1px solid #000000;}")))
  ),

  
  # Sidebar to provide more instructions
  sidebarLayout(
    sidebarPanel(
      h3(HTML("<strong>Completing this report:</strong>")),
      h4("After pre-populating your report, click through each tab below to provide updated information where applicable."),
      h5(HTML("<p class = 'text-info'>More specifically, be sure to: <br> <br> 
                 (1) update the initiative status assessment on the “Initiative Information” tab; <br> <br> 
                 (2) update the milestones to be displayed on the FY20 Dashboard on the “Milestones” tab; <br> <br> 
                 (3) update the financial information on the “Financial Information” tab;  and <br> <br>
                 (4) add any new data points for the Outcomes and Pathways.")),
         br(),
      width=2),
    
    # Define tabs
    mainPanel(
      tabsetPanel( 
        id = "tabs",
        type="tabs",
        
        tabPanel( value = "1",
          tags$b("Initiative Information"),
          tags$br(),
          tags$b(h3("INITIATIVE INFORMATION")),
          tags$br(),
          textInput(inputId = "initiative",
                    label = "Initiative Name",
                    width = 600),
          textInput(inputId = "initiativelead",
                    label = "Name of Initiative Lead",
                    width = 600),
          textInput(inputId = "email",
                    label = "Initiative Lead Email",
                    width = 600),
          textInput(inputId = "initiativestart",
                    label = "Initiative Start Year",
                    placeholder = "Year (YYYY)",
                    width = 200),
          textInput(inputId = "initiativeend",
                    label = "Initiative End Year",
                    placeholder = "Year (YYYY)",
                    width = 200),
          tags$table(style = "width: 625",
                     tags$tr(tags$td(align = "left",
                                     textAreaInput(inputId = "initiativestatement",
                                                   label = "Initiative Statement",
                                                   width = 600,
                                                   height = "100px")),
                             tags$td(style = "width: 5%",
                                     align = "center",
                                     actionButton(inputId = "initiativestatementinfo",
                                                  label = "",
                                                  icon('question'),
                                                  class = "btn-info")))),
          tags$hr(),
          tags$b(h4("Overall Initiative Status Assessment")),
          tags$br(),
          selectInput(inputId = "initiativestatus", 
                      label = "Status Assessment Category", 
                      choices = c("","Opportunity","Progress","Barrier","Support","Contingent"),
                      width = 200),
          textAreaInput(inputId = "initiativejust", 
                        label = "Status Assessment Justification",
                        width = 600,
                        height = "100px"),
          tags$table(style = "width: 700",
                     tags$tr(tags$td(style = "width: 7%",
                                     align = "center",
                                     actionButton(inputId = "initiativestatusinfo", label = "", icon('question'))),
                             tags$td(style = "width: 45%",
                                     align = "left",
                                     h5("How do I determine my initiative's status?")),
                             tags$td(style = "width: 7%",
                                     align = "center",
                                     actionButton(inputId = "initiativestatuscategoriesinfo", label = "", icon('question'))),
                             tags$td(style = "width: 46%",
                                     align = "left",
                                     h5("What are the status assessment categories?"))))
        ),
        
        tabPanel( value = "2",
          tags$b("Outcome 1"),
          column(8,
                 tags$br(),
                 tags$b(h3("OUTCOME 1")),
                 tags$br(),
                 tags$table(style = "width: 625",
                            tags$tr(tags$td(align = "left",
                                            textAreaInput(inputId = "outcome1statement",
                                                          label = "Outcome Statement",
                                                          placeholder = "Maximum of XX characters",
                                                          width = 600,
                                                          height = "100px")),
                                    tags$td(style = "width: 5%",
                                            align = "center",
                                            actionButton(inputId = "outcomestatementinfo",
                                                         label = "",
                                                         icon('question'),
                                                         class = "btn-info")))),
                 textAreaInput(inputId ="out1indicator", 
                           label = "Indicator Description", 
                           placeholder = "A description of your indicator",
                           width = 600,
                           height = "100px"),
                 textInput(inputId = "out1indicatorlabel", 
                           label = "Indicator Label"),
                 textInput(inputId = "out1indicatorunits", 
                           label= "Indicator Units"),
                 textAreaInput(inputId = "out1indicatorsource",
                           label = "Indicator Data Source",
                           width = 600,
                           height = "100px"),
                 selectInput(inputId = "out1numtrend",
                               label = "How many trend lines are there?",
                               choices = c("",1,2,3)),
                 uiOutput("out1numtrend")
                 ),
          column(4,
                 tags$br(),
                 tags$br(),
                 tags$br(),
                 h5("INSERT ANNOTATED JPEG SHOWING COMPONENTS OF OUTCOME 1 INFO ON DASHBOARD"))
          ),
        
        tabPanel( value = "3",
          tags$b("Outcome 2"),
          column(8,
                 tags$br(),
                 tags$b("OUTCOME 2"),
                 tags$br(),
                 textAreaInput(inputId = "outcome2statement",
                               label = "Outcome Statement",
                               placeholder = "Maximum of XX characters",
                               width = 600,
                               height = "100px"),
                 h5("<INSERT INSTRUCTIONS ON OUTCOME STATEMENTS>"),
                 textAreaInput(inputId ="out2indicator", 
                           label = "Indicator Description", 
                           placeholder = "A description of your indicator",
                           width = 600,
                           height = "100px"),
                 textInput(inputId = "out2indicatorlabel", 
                           label = "Indicator Label"),
                 textInput(inputId = "out2indicatorunits", 
                           label= "Indicator Units"),
                 textAreaInput(inputId = "out2indicatorsource",
                           label = "Indicator Data Source",
                           width = 600,
                           height = "100px"),
                 selectInput(inputId = "out2numtrend",
                             label = "How many trend lines are there?",
                             choices = c("",1,2,3)),
                 uiOutput("out2numtrend")
          ),
          column(4,
                 tags$br(),
                 h5("INSERT ANNOTATED JPEG SHOWING COMPONENTS OF OUTCOME 2 INFO ON DASHBOARD"))
        ),
        
        tabPanel( value = "4",
          tags$b("Pathway 1"),
          column(8,
                 tags$br(),
                 tags$b("PATHWAY 1"),
                 tags$br(),
                 textAreaInput(inputId = "pathway1statement",
                               label = "Pathway Statement",
                               placeholder = "Maximum of XX characters",
                               width = 600,
                               height = "100px"),
                 h5("<INSERT INSTRUCTIONS ON PATHWAY STATEMENTS>"),
                 textAreaInput(inputId ="path1indicator", 
                           label = "Indicator Description", 
                           placeholder = "A description of your indicator",
                           width = 600,
                           height = "100px"),
                 textInput(inputId = "path1indicatorlabel", 
                           label = "Indicator Label"),
                 textInput(inputId = "path1indicatorunits", 
                           label= "Indicator Units"),
                 textAreaInput(inputId = "path1indicatorsource",
                           label = "Indicator Data Source",
                           width = 600,
                           height = "100px"),
                 selectInput(inputId = "path1numtrend",
                             label = "How many trend lines are there?",
                             choices = c("",1,2,3)),
                 uiOutput("path1numtrend")
          ),
          column(4,
                 tags$br(),
                 h5("INSERT ANNOTATED JPEG SHOWING COMPONENTS OF PATHWAY 1 INFO ON DASHBOARD"))
        ),
        
        tabPanel( value = "5",
          tags$b("Pathway 2"),
          column(8,
                 tags$br(),
                 tags$b("PATHWAY 2"),
                 tags$br(),
                 textAreaInput(inputId = "pathway2statement",
                               label = "Pathway Statement",
                               placeholder = "Maximum of XX characters",
                               width = 600,
                               height = "100px"),
                 h5("<INSERT INSTRUCTIONS ON PATHWAY STATEMENTS>"),
                 textAreaInput(inputId ="path2indicator", 
                           label = "Indicator Description", 
                           placeholder = "A description of your indicator",
                           width = 600,
                           height = "100px"),
                 textInput(inputId = "path2indicatorlabel", 
                           label = "Indicator Label"),
                 textInput(inputId = "path2indicatorunits", 
                           label= "Indicator Units"),
                 textAreaInput(inputId = "path2indicatorsource",
                           label = "Indicator Data Source",
                           width = 600,
                           height = "100px"),
                 selectInput(inputId = "path2numtrend",
                             label = "How many trend lines are there?",
                             choices = c("",1,2,3)),
                 uiOutput("path2numtrend")
          ),
          column(4,
                 tags$br(),
                 h5("INSERT ANNOTATED JPEG SHOWING COMPONENTS OF PATHWAY 2 INFO ON DASHBOARD"))
        ),
        
        tabPanel( value = "6",
          tags$b("Milestones"),
          DT::DTOutput("milestonedataoutput"),
          actionButton(inputId = "addMilestoneData",
                       label = "Add Row")
        ),
        
        tabPanel( value = "7",
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
    tags$hr(tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
    tags$br(),
    tags$b(h4("Please click 'Save' before ending your session.")),
    tags$p("NOTE: do not click save until you are ready to leave the webpage! \nYou will be able to return to your work next time by selecting your initiative at the top and 'Pre-populating' the report."),
    tags$br(),
    column(2,actionButton(inputId = "save",
                          label = "Save",
                          icon('save'),
                          width = 100,
                          class = "btn-success")),
    tags$br(),
    h2("...")
  )
  
 ) # fluid page


# +++++++++++++++++++++++++++++++++
# 
# ---- Define server logic ----
# 
# +++++++++++++++++++++++++++++++++


server <- function(input, output, session) {

  initiative_dim <<- import('COMPS/responses/FY21_initiative_dim.csv') 
  init_indicator_dim <<- import('COMPS/responses/FY21_init_indicator_dim.csv')
  init_indicator_fact <<- import('COMPS/responses/FY21_init_indicator_fact.csv')
  milestones <<- import('COMPS/responses/FY21_milestones.csv', colClasses = c(target = "character")) %>% 
    mutate(milestonestatus = factor(milestonestatus, levels = c("","Opportunity","Progress","Barrier","Support","Contingent"), ordered = T))
  
  source('COMPS/prepopulate.R',local=F)
  source('COMPS/conditionalpanels.R',local=F)
  source('COMPS/outcomepathwaydata.R',local=F)
  
  
  # update initiative selection options based on selected goal
  observe({ 
    
    initiativeoptions <- initiative_dim %>% filter(goal==input$goal) %>% 
      group_by(initiativekey) %>% summarise(initiative=initiative[timestamp==max(timestamp)]) 
    
    updateSelectInput(session, 
                      inputId = "initiativeoptions",
                      choices = c("",sort(initiativeoptions$initiative)),
                      selected = "")
    
  })
  
  
  # pre-populate text boxes based on selected initiative
  observeEvent(input[["populatereport"]], {
    
    selectedinitiative <- input$initiativeoptions
    
    # pre-populate initiative info, outcomes, pathways, financial
    prePopulate(selectedinitiative, session)
    
    # render conditional panels for outcome and pathway subcategories
    outcome2Conditional(selectedinitiative, input, output)
    pathway1Conditional(selectedinitiative, input, output)
    pathway2Conditional(selectedinitiative, input, output)
    outcome1Conditional(selectedinitiative, input, output)
    
    outputOptions(output, "out1numtrend", suspendWhenHidden = FALSE)
    outputOptions(output, "out2numtrend", suspendWhenHidden = FALSE)
    outputOptions(output, "path1numtrend", suspendWhenHidden = FALSE)
    outputOptions(output, "path2numtrend", suspendWhenHidden = FALSE)
    
    # pre-populate outcome and pathway data tables
    defineOutPathData(selectedinitiative)
    out1subcat1(out1subcat1initfact)
    out1subcat2(out1subcat2initfact)
    out1subcat3(out1subcat3initfact)
    
    # pre-populate milestone table
    # filtered_milestones <- milestonesPrePopulate(selectedinitiative, input, output)
    filtered_milestones <- data.frame(milestones[which(milestones$initiative==selectedinitiative),c("milestone","target","milestonestatus","milestonejust","milestoneactive")])
    colnames(filtered_milestones) <- c("Milestone","Target","Status","Status.Justification","Display.On.FY20.Dashboard")
    milestonedata(filtered_milestones)
    
  })

  # ---- MILESTONES ----
  
  # render reactive milestone df and table output
  milestonedata <- reactiveVal()
  
  observe({ 
    
    output$milestonedataoutput <- DT::renderDataTable({
      datatable(milestonedata(), extensions = 'Buttons', escape = F, editable = T, selection = 'none', 
                options = list(dom = 'Bt', pageLength = 50),
                rownames = F)
    }, server = T)
    
  })
  
  # update reactive df when cell edited
  observeEvent(input[["milestonedataoutput_cell_edit"]], {

    cell <- input[["milestonedataoutput_cell_edit"]]
    newdf <- milestonedata()
    newdf[cell$row, cell$col+1] <- cell$value
    milestonedata(newdf)

  })

  # each time addData is pressed, add data to reactive df
  observeEvent(input[["addMilestoneData"]], {
    
    oldmilestonetable <- milestonedata()

    milestonerow <- data.frame(Milestone="[CLICK HERE TO INSERT NEW MILESTONE TEXT]",
                               Target="[MM/YYYY]",
                               Status="[Opportunity, Progress, Barrier, Support, Contingent]",
                               Status.Justification="[CLICK HERE TO INSERT STATUS JUSTIFICATION]",
                               Display.On.FY20.Dashboard="[Yes/No]")

    milestonedata(rbind.data.frame(oldmilestonetable,milestonerow))
    
})
  

  # ---- OUTCOMES & PATHWAYS ----
  
  # render reactive dataframes and output tables for outcome and pathway subcategories  
  out1subcat1 <- reactiveVal()
  out1subcat2 <- reactiveVal()
  out1subcat3 <- reactiveVal()
    

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
    
  })
  
  # update reactive df when cell edited for each outcome and pathway subcategory table
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
    
  # each time addData is pressed, add data to reactive df
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


  # ---- POP UPS AND GUIDANCE ----
  
  observeEvent(input[["initiativestatementinfo"]], {
    shinyalert("The Initiative Statement",
               "This statement should identify the overall intention of the initative, including the anticipated end date, desired change seen in the world, and the actions taken to achieve that change.",
               type = "info")
  })
  
  observeEvent(input[["initiativestatusinfo"]], {
    shinyalert("Completing a Status Assessment",
               "There is no formula to 'add up' to the assessment category. Rather, think of this as an opportunity to signal the overall status 
               of your initiative to senior leadership.",
               type = "info")
  })
  
  observeEvent(input[["initiativestatuscategoriesinfo"]], {
    shinyalert("Status Assessment Categories",
               "Opportunity: Critical opportunity to scale or leverage Initiative. \n
                Progress: On track or affected by minor issues that are being addressed by Initiative team. \n
                Barrier: Implementation is delayed by external factors (e.g., political will, partners engagement). \n
                Support: Initiative requires leadership support to address one or more issues. \n
                Contingent: Contingent on achievement of preceding milestone.",
               type = "info")
  })
  
  observeEvent(input[["outcomestatementinfo"]], {
    shinyalert("An Outcome Statement","It should be 'SMART', and identify 3 things: (1) the end date by which this outcome should be achieved, 
               (2) the indicator to measure success, and (3) the desired end state (or value) for this outcome.",
               type = "info")
  })
    

  # ---- SAVING INPUT DATA ----
  
  # whenever a field is filled, aggregate all form data
  formData1 <- reactive({
    initiative_dim_data <- data.frame(t(as.matrix(sapply(initiative_dim_fields, function(x) input[[x]])))) %>%
      mutate(securedanticipatedsum = input$fundssecured+input$fundsanticipated,
             initiativekey = as.character(initiative_dim$initiativekey[initiative_dim$initiative==input$initiativeoptions]),
             displayorder = initiative_dim$displayorder[initiative_dim$initiative==input$initiativeoptions],
             globalinitiative = initiative_dim$globalinitiative[initiative_dim$initiative==input$initiativeoptions],
             usinitiative = initiative_dim$usinitiative[initiative_dim$initiative==input$initiativeoptions])
    initiative_dim_data
  })
  
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
             indicatorkey = as.numeric(paste0(c(rep(1,6),rep(2,6)),substr(initiative_dim$initiativekey[initiative_dim$initiative==input$initiativeoptions],2,5),"0",c(seq(1:6),seq(1:6)))))
    colnames(init_indicator_dim_data) <- init_indicator_dim_fields
    init_indicator_dim_data
  })
  
  formData3 <- reactive({
    newdf <- milestonedata()
    finalmilestonedat <- data.frame(milestonekey = NA,
                                    goal = rep(input$goal, nrow(milestonedata())),
                                    initiative = rep(input$initiative, nrow(milestonedata())),
                                    milestone = newdf$Milestone,
                                    target = newdf$Target,
                                    milestonestatus = newdf$Status,
                                    milestonejust = newdf$Status.Justification,
                                    milestonecreation = NA,
                                    milestonestart = NA,
                                    milestoneend = NA,
                                    milestoneactive = newdf$Display.On.FY20.Dashboard)
  })

  
  # define 'saveData' function
  saveData <- function(data1,data2,data3) {
    
    initiative_dim <<- import('COMPS/responses/FY21_initiative_dim.csv')
    init_indicator_dim <<- import('COMPS/responses/FY21_init_indicator_dim.csv')
    init_indicator_fact <<- import('COMPS/responses/FY21_init_indicator_fact.csv')
    milestones <<- import('COMPS/responses/FY21_milestones.csv', colClasses = c(target = "character")) %>% 
      mutate(milestonestatus = factor(milestonestatus, levels = c("","Opportunity","Progress","Barrier","Support","Contingent"), ordered = T))
    
    data1 <- rbind(initiative_dim, 
                   data1 %>% mutate(timestamp = format(Sys.time(), format="%Y%m%d%H%M")))
    data2 <- rbind(init_indicator_dim,
                   data2 %>% mutate(timestamp = format(Sys.time(), format="%Y%m%d%H%M")))
    
    # data3 <- rbind(milestones, data3)
    
    # Write the files to the local system
    write.csv(
      x = data1,
      file = "COMPS/responses/FY21_initiative_dim_TEST.csv", 
      row.names = FALSE, quote = TRUE
    )
    
    write.csv(
      x = data2,
      file = "COMPS/responses/FY21_init_indicator_dim_TEST.csv",
      row.names = FALSE, quote = TRUE
    )
    
    write.csv(
      x = data3,
      file = "COMPS/responses/FY21_milestones_TEST.csv",
      row.names = FALSE, quote = TRUE
    )

  }

  
  # ***TO DO: BEFORE SAVING, HAVE USERS HIT A 'REFRESH' BUTTON TO ENSURE THEY ARE WRITING THEIR DATA TO NEWEST VERSION OF FILE***
  

  # when the Save button is clicked, save the form data & provide confirmation pop-up
  observeEvent(input[["save"]], {

    saveData(formData1(),formData2(),formData3())
    shinyalert("Saved!","You may now close your window.", type = "success")

  })

}

# ---- Run the application ----

shinyApp(ui = ui, server = server)



# STILL TO FIX
# Verify that 'refresh' works, so that most recent data is being used to save to, even if another user has saved in the interim of your session
# Update milestone dates between csv and importing/exporting from R (format gets funky)
# Directions / guidance for teams (character limits, explanations, etc)
# Format input form, and include jpegs with more information


