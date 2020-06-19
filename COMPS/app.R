#
# code:  WWF COMPS Initiative Report Shiny Data Entry App
#
# author: Louise Glew, louise.glew@wwf.org; Kelly Claborn, clabornkelly@gmail.com
# created: March 2020
# modified: June 2020, Kelly Claborn


source('global.R',local=F)


# 
# +++++++++++++++++++++++++++++++++
# 
# ---- Defining ui for initiative reporting app ----
# 
# +++++++++++++++++++++++++++++++++
# 


ui <- fluidPage(
  theme = shinytheme("sandstone"),
  useShinyjs(),
  useShinyalert(),
  
  # add 'loading' bar for whenever app is busy
  tags$head(tags$style(type="text/css", "
             #loadmessage {
               position: fixed;
               top: 0px;
               left: 0px;
               width: 100%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background-color: #93C54B;
               z-index: 105;
             }")),
  
  # Application title & intro
  titlePanel(h1(HTML("<strong>WWF-US FY20 Initiative Report</strong>")),
             windowTitle = "WWF-US FY20 Initiative Report"),
  
  tags$div(tags$br(),
    tags$table(style = "width: 100%",
               tags$tr(tags$td(style = "width: 50%",
                               align = "left",
                               h4(HTML("<strong>Welcome to the new platform for WWF-US initiative reporting!</strong>   <br> <br> 
                                       To have your report pre-populated with your most recent submission, please select your Goal team and Initiative name from the drop-down menus below."))),
                       tags$td(style = "width: 50%",
                               align = "right",
                               p(HTML("<br> <br> <strong>For initiative reporting support, please contact <a href = 'mailto: louise.glew@wwf.org'>Louise Glew</a> or 
                                      <a href = 'mailto: kelly.claborn@wwf.org'>Kelly Claborn</a></strong>"))))),
    tags$hr(tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
    tags$br()),
  
  # drop-down menus for goal and initiative selection
  tags$div(column(3, 
      tags$h4(tags$b("SELECT YOUR INITIATIVE HERE:")),
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
    column(9, img(src=Dashboard_intro, height = "45%", width = "100%", align = "center")),
    h2("..."),
    tags$head(tags$style('h2 {color:white;}')),
    tags$hr(tags$style(HTML("hr {border-top: 1px solid #000000;}")))),

  
  # Sidebar to provide more instructions
  sidebarLayout(sidebarPanel(h4(HTML("<strong>COMPLETING THIS REPORT:</strong>")),
      h4("After pre-populating your report, click through each tab to provide updated information where applicable."),
      tags$br(),
      h5(HTML("<p class = 'text-success'><strong>More specifically, be sure to: </strong> <br> <br> 
                 (1) update the overall initiative status assessment on the “Initiative Information” tab <br> <br>
                 (2) add any new data points for the Outcomes and Pathways <br> <br>
                 (3) update the milestones to be displayed on the FY20 Dashboard on the “Milestones” tab <br> <br>
                 (4) update the financial information on the “Financial Information” tab.")),
      tags$br(),
      width = 2),
    
      
    # Define tabs
    mainPanel(div(tabsetPanel(id = "tabs",
        type="tabs",
        
        tabPanel( value = "1",
          tags$b("Initiative Information"),
          tags$br(),
          tags$b(h3("INITIATIVE INFORMATION")),
          tags$br(),
          textInput(inputId = "initiative",
                    label = "Initiative Name",
                    width = "40%"),
          textInput(inputId = "initiativelead",
                    label = "Name of Initiative Lead",
                    width = "40%"),
          textInput(inputId = "email",
                    label = "Initiative Lead Email",
                    width = "40%"),
          textInput(inputId = "initiativestart",
                    label = "Initiative Start Year",
                    placeholder = "Year (YYYY)",
                    width = "20%"),
          textInput(inputId = "initiativeend",
                    label = "Initiative End Year",
                    placeholder = "Year (YYYY)",
                    width = "20%"),
          tags$table(style = "width: 60%", 
                     tags$tr(style = "width: 100%",
                             tags$td(style = "width: 95%",
                                     align = "left",
                                     textAreaInput(inputId = "initiativestatement",
                                                   label = "Initiative Statement",
                                                   height = "100px")  %>%
                                       shiny::tagAppendAttributes(style = 'width: 100%;')),
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
                      width = "20%"),
          textAreaInput(inputId = "initiativejust", 
                        label = "Status Assessment Justification",
                        height = "100px")  %>%
            shiny::tagAppendAttributes(style = 'width: 57%;'),
          tags$table(style = "width: 75%",
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
                                     h5("What are the status assessment categories?"))))),
        
        tabPanel( value = "2",
          tags$b("Outcome 1"),
          column(8,
                 tags$br(),
                 tags$b(h3("OUTCOME 1")),
                 tags$br(),
                 tags$table(style = "width: 100%",
                            tags$tr(tags$td(style = "width: 85%",
                                            align = "left",
                                            textAreaInput(inputId = "outcome1statement",
                                                          label = "Outcome Statement",
                                                          placeholder = "Maximum of 300 characters",
                                                          height = "100px") %>%
                                              shiny::tagAppendAttributes(style = 'width: 100%;')),
                                    tags$td(style = "width: 10%",
                                            align = "center",
                                            textOutput("out1statecharcount")),
                                    tags$td(style = "width: 5%",
                                            align = "right",
                                            actionButton(inputId = "outcome1statementinfo",
                                                         label = "",
                                                         icon('question'),
                                                         class = "btn-info")))),
                 textAreaInput(inputId ="out1indicator", 
                           label = "Indicator Description", 
                           placeholder = "A description of your indicator",
                           height = "100px") %>%
                   shiny::tagAppendAttributes(style = 'width: 85%;'),
                 tags$table(style = "width: 75%",
                            tags$tr(tags$td(style = "width: 90%",
                                            align = "left",
                                            textInput(inputId = "out1indicatorlabel", 
                                                          label = "Indicator Label") %>%
                                              shiny::tagAppendAttributes(style = 'width: 100%;')),
                                    tags$td(style = "width: 10%",
                                            align = "center",
                                            textOutput("out1labelcharcount")))),
                 tags$table(style = "width: 75%",
                            tags$tr(tags$td(style = "width: 90%",
                                            align = "left",
                                            textInput(inputId = "out1indicatorunits", 
                                                      label = "Indicator Units") %>%
                                              shiny::tagAppendAttributes(style = 'width: 100%;')),
                                    tags$td(style = "width: 10%",
                                            align = "center",
                                            textOutput("out1unitscharcount")))),
                 textAreaInput(inputId = "out1indicatorsource",
                           label = "Indicator Data Source",
                           height = "100px") %>%
                   shiny::tagAppendAttributes(style = 'width: 85%;'),
                 selectInput(inputId = "out1numtrend",
                               label = "How many trend lines are there?",
                               choices = c("",1,2,3)),
                 uiOutput("out1numtrend")),
          
          column(4,
                 tags$br(),
                 tags$br(),
                 tags$br(),
                 tags$table(style = "width: 100%",
                            tags$tr(tags$td(img(src=Outcome1_detailintro, height = "35%", width = "100%", align = "center"))),
                            tags$tr(tags$td(h2("..."))),
                            tags$tr(tags$td(img(src=Outcome1_labels, height = "65%", width = "100%", align = "center")))),
                 uiOutput("out1trendpic"))),
        
        tabPanel( value = "3",
          tags$b("Outcome 2"),
          column(8,
                 tags$br(),
                 tags$b(h3("OUTCOME 2")),
                 tags$br(),
                 tags$table(style = "width: 100%",
                            tags$tr(tags$td(style = "width: 85%",
                                            align = "left",
                                            textAreaInput(inputId = "outcome2statement",
                                                          label = "Outcome Statement",
                                                          placeholder = "Maximum of 300 characters",
                                                          height = "100px") %>%
                                              shiny::tagAppendAttributes(style = 'width: 100%;')),
                                    tags$td(style = "width: 10%",
                                            align = "center",
                                            textOutput("out2statecharcount")),
                                    tags$td(style = "width: 5%",
                                            align = "right",
                                            actionButton(inputId = "outcome2statementinfo",
                                                         label = "",
                                                         icon('question'),
                                                         class = "btn-info")))),
                 textAreaInput(inputId ="out2indicator", 
                               label = "Indicator Description", 
                               placeholder = "A description of your indicator",
                               height = "100px") %>%
                   shiny::tagAppendAttributes(style = 'width: 85%;'),
                 tags$table(style = "width: 75%",
                            tags$tr(tags$td(style = "width: 90%",
                                            align = "left",
                                            textInput(inputId = "out2indicatorlabel", 
                                                      label = "Indicator Label") %>%
                                              shiny::tagAppendAttributes(style = 'width: 100%;')),
                                    tags$td(style = "width: 10%",
                                            align = "center",
                                            textOutput("out2labelcharcount")))),
                 tags$table(style = "width: 75%",
                            tags$tr(tags$td(style = "width: 90%",
                                            align = "left",
                                            textInput(inputId = "out2indicatorunits", 
                                                      label = "Indicator Units") %>%
                                              shiny::tagAppendAttributes(style = 'width: 100%;')),
                                    tags$td(style = "width: 10%",
                                            align = "center",
                                            textOutput("out2unitscharcount")))),
                 textAreaInput(inputId = "out2indicatorsource",
                               label = "Indicator Data Source",
                               height = "100px") %>%
                   shiny::tagAppendAttributes(style = 'width: 85%;'),
                 selectInput(inputId = "out2numtrend",
                             label = "How many trend lines are there?",
                             choices = c("",1,2,3)),
                 uiOutput("out2numtrend")),
          
          column(4,
                 tags$br(),
                 tags$br(),
                 tags$br(),
                 tags$table(style = "width: 100%",
                            tags$tr(tags$td(img(src=Outcome2_detailintro, height = "35%", width = "100%", align = "center"))),
                            tags$tr(tags$td(h2("..."))),
                            tags$tr(tags$td(img(src=Outcome2_labels, height = "65%", width = "100%", align = "center")))),
                 uiOutput("out2trendpic"))),
        
        tabPanel( value = "4",
          tags$b("Pathway 1"),
          column(8,
                 tags$br(),
                 tags$b(h3("PATHWAY 1")),
                 tags$br(),
                 tags$table(style = "width: 100%",
                            tags$tr(tags$td(style = "width: 85%",
                                            align = "left",
                                            textAreaInput(inputId = "pathway1statement",
                                                          label = "Pathway Statement",
                                                          placeholder = "Maximum of 300 characters",
                                                          height = "100px") %>%
                                              shiny::tagAppendAttributes(style = 'width: 100%;')),
                                    tags$td(style = "width: 10%",
                                            align = "center",
                                            textOutput("path1statecharcount")),
                                    tags$td(style = "width: 5%",
                                            align = "right",
                                            actionButton(inputId = "pathway1statementinfo",
                                                         label = "",
                                                         icon('question'),
                                                         class = "btn-info")))),
                 textAreaInput(inputId ="path1indicator", 
                               label = "Indicator Description", 
                               placeholder = "A description of your indicator",
                               height = "100px") %>%
                   shiny::tagAppendAttributes(style = 'width: 85%;'),
                 tags$table(style = "width: 75%",
                            tags$tr(tags$td(style = "width: 90%",
                                            align = "left",
                                            textInput(inputId = "path1indicatorlabel", 
                                                      label = "Indicator Label") %>%
                                              shiny::tagAppendAttributes(style = 'width: 100%;')),
                                    tags$td(style = "width: 10%",
                                            align = "center",
                                            textOutput("path1labelcharcount")))),
                 tags$table(style = "width: 75%",
                            tags$tr(tags$td(style = "width: 90%",
                                            align = "left",
                                            textInput(inputId = "path1indicatorunits", 
                                                      label = "Indicator Units") %>%
                                              shiny::tagAppendAttributes(style = 'width: 100%;')),
                                    tags$td(style = "width: 10%",
                                            align = "center",
                                            textOutput("path1unitscharcount")))),
                 textAreaInput(inputId = "path1indicatorsource",
                               label = "Indicator Data Source",
                               height = "100px") %>%
                   shiny::tagAppendAttributes(style = 'width: 85%;'),
                 selectInput(inputId = "path1numtrend",
                             label = "How many trend lines are there?",
                             choices = c("",1,2,3)),
                 uiOutput("path1numtrend")),
          
          column(4,
                 tags$br(),
                 tags$br(),
                 tags$br(),
                 tags$table(style = "width: 100%",
                            tags$tr(tags$td(img(src=Pathway1_detailintro, height = "35%", width = "100%", align = "center"))),
                            tags$tr(tags$td(h2("..."))),
                            tags$tr(tags$td(img(src=Pathway1_labels, height = "65%", width = "100%", align = "center")))),
                 uiOutput("path1trendpic"))),
        
        tabPanel( value = "5",
          tags$b("Pathway 2"),
          column(8,
                 tags$br(),
                 tags$b(h3("PATHWAY 2")),
                 tags$br(),
                 tags$table(style = "width: 100%",
                            tags$tr(tags$td(style = "width: 85%",
                                            align = "left",
                                            textAreaInput(inputId = "pathway2statement",
                                                          label = "Pathway Statement",
                                                          placeholder = "Maximum of 300 characters",
                                                          height = "100px") %>%
                                              shiny::tagAppendAttributes(style = 'width: 100%;')),
                                    tags$td(style = "width: 10%",
                                            align = "center",
                                            textOutput("path2statecharcount")),
                                    tags$td(style = "width: 5%",
                                            align = "right",
                                            actionButton(inputId = "pathway2statementinfo",
                                                         label = "",
                                                         icon('question'),
                                                         class = "btn-info")))),
                 textAreaInput(inputId ="path2indicator", 
                               label = "Indicator Description", 
                               placeholder = "A description of your indicator",
                               height = "100px") %>%
                   shiny::tagAppendAttributes(style = 'width: 85%;'),
                 tags$table(style = "width: 75%",
                            tags$tr(tags$td(style = "width: 90%",
                                            align = "left",
                                            textInput(inputId = "path2indicatorlabel", 
                                                      label = "Indicator Label") %>%
                                              shiny::tagAppendAttributes(style = 'width: 100%;')),
                                    tags$td(style = "width: 10%",
                                            align = "center",
                                            textOutput("path2labelcharcount")))),
                 tags$table(style = "width: 75%",
                            tags$tr(tags$td(style = "width: 90%",
                                            align = "left",
                                            textInput(inputId = "path2indicatorunits", 
                                                      label = "Indicator Units") %>%
                                              shiny::tagAppendAttributes(style = 'width: 100%;')),
                                    tags$td(style = "width: 10%",
                                            align = "center",
                                            textOutput("path2unitscharcount")))),
                 textAreaInput(inputId = "path2indicatorsource",
                               label = "Indicator Data Source",
                               height = "100px") %>%
                   shiny::tagAppendAttributes(style = 'width: 85%;'),
                 selectInput(inputId = "path2numtrend",
                             label = "How many trend lines are there?",
                             choices = c("",1,2,3)),
                 uiOutput("path2numtrend")),
          
          column(4,
                 tags$br(),
                 tags$br(),
                 tags$br(),
                 tags$table(style = "width: 100%",
                            tags$tr(tags$td(img(src=Pathway2_detailintro, height = "35%", width = "100%", align = "center"))),
                            tags$tr(tags$td(h2("..."))),
                            tags$tr(tags$td(img(src=Pathway2_labels, height = "65%", width = "100%", align = "center")))),
                 uiOutput("path2trendpic"))),
        
        tabPanel( value = "6",
          tags$b("Milestones"),
          tags$b(h3("MILESTONES")),
          tags$br(),
          h5("Milestones are statements that articulate an initiative’s intended progress towards achieving conservation outcomes, based on the initiative’s theory of change.  
             A milestone is outcome-oriented, representing an actionable achievement that is necessary in order to achieve the initiative’s pathway(s) and outcome(s)."),
          tags$table(style = "width: 75%",
                     tags$tr(tags$td(style = "width: 7%",
                                     align = "center",
                                     actionButton(inputId = "milestonestatusinfo", label = "", icon('question'))),
                             tags$td(style = "width: 45%",
                                     align = "left",
                                     h5("What is a milestone status assessment?")),
                             tags$td(style = "width: 7%",
                                     align = "center",
                                     actionButton(inputId = "milestonestatuscategoriesinfo", label = "", icon('question'))),
                             tags$td(style = "width: 46%",
                                     align = "left",
                                     h5("What are the status assessment categories?")))),
          tags$hr(),
          tags$br(),
          h5(HTML("<strong> In the table below, please: <br> 
                  (1) add any new milestones by clicking 'Add Row', <br>
                  (2) identify which milestones (up to 6) will be included on this year's Dashboard by writing 'Yes' or 'No', and <br>
                  (3) update the status/justification for each milestone that is still current (i.e., the target date has not yet passed).</strong>")),
          tags$br(),
          DT::DTOutput("milestonedataoutput"),
          actionButton(inputId = "addMilestoneData",
                       label = "Add Row")),
        
        tabPanel( value = "7",
          tags$b("Financial Information"),
          tags$b(h3("FINANCIAL INFORMATION")),
          tags$br(),
          h4("What is the total cost budget for FY21-FY23?"),
          tags$br(),
          numericInput(inputId = "fundsneeded",
                       label = "Total Cost Budget",
                       value = NULL),
          numericInput(inputId = "fundssecured",
                       label = "Total Funds Secured",
                       value = NULL),
          numericInput(inputId = "fundsanticipated",
                       label = "Total Funds Anticipated", 
                       value = NULL))), class = "span7"))),
  
  
  # Save button at bottom of app page
  tags$div(tags$br(),
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
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$div("Loading...",id="loadmessage")),
    h2("...")))


# 
# +++++++++++++++++++++++++++++++++
# 
# ---- Define server logic ----
# 
# +++++++++++++++++++++++++++++++++
# 


server <- function(input, output, session) {
  
  initiative_dim <<- drop_read_csv('GitHub/ConservationDashboard/COMPS/responses/FY21_initiative_dim.csv', dtoken = token)
  init_indicator_dim <<- drop_read_csv('GitHub/ConservationDashboard/COMPS/responses/FY21_init_indicator_dim.csv', dtoken = token)
  init_indicator_fact <<- drop_read_csv('GitHub/ConservationDashboard/COMPS/responses/FY21_init_indicator_fact.csv', dtoken = token)
  milestones <<- drop_read_csv('GitHub/ConservationDashboard/COMPS/responses/FY21_milestones.csv', dtoken = token) %>% 
    mutate(milestonestatus = factor(milestonestatus, levels = c("","Opportunity","Progress","Barrier","Support","Contingent"), ordered = T),
           target = gsub("m","",target))
  
  # update initiative selection options based on selected goal
  observe({ 
    
    initiativeoptions <- initiative_dim %>% filter(goal==input$goal) %>% 
      group_by(initiativekey) %>% summarise(initiative=initiative[timestamp==max(timestamp)]) 
    
    updateSelectInput(session, 
                      inputId = "initiativeoptions",
                      choices = c("",sort(initiativeoptions$initiative)),
                      selected = "")
    
  })
  
  # ---- PRE-POPULATE DATA ----
  
  # source functions for pre-populating & establish reactive values for per-session unique identifiers
  source('prepopulate.R', local = F)
  source('conditionalpanels.R', local = F)

  rv <- reactiveValues(initiativekey = NA, out1subcat1key = NA, out1subcat2key = NA, out1subcat3key = NA, out2subcat1key = NA, out2subcat2key = NA, out2subcat3key = NA, 
                       path1subcat1key = NA, path1subcat2key = NA, path1subcat3key = NA, path2subcat1key = NA, path2subcat2key = NA, path2subcat3key = NA)
  
  
  # pre-populate text boxes based on selected initiative
  observeEvent(input[["populatereport"]], {
    
    initiative_dim <<- drop_read_csv('GitHub/ConservationDashboard/COMPS/responses/FY21_initiative_dim.csv', dtoken = token)
    init_indicator_dim <<- drop_read_csv('GitHub/ConservationDashboard/COMPS/responses/FY21_init_indicator_dim.csv', dtoken = token)
    init_indicator_fact <<- drop_read_csv('GitHub/ConservationDashboard/COMPS/responses/FY21_init_indicator_fact.csv', dtoken = token)
    milestones <<- drop_read_csv('GitHub/ConservationDashboard/COMPS/responses/FY21_milestones.csv', dtoken = token) %>% 
      mutate(milestonestatus = factor(milestonestatus, levels = c("","Opportunity","Progress","Barrier","Support","Contingent"), ordered = T),
             target = gsub("m","",target))
    
    selectedinitiative <- input$initiativeoptions
    
    # pre-populate initiative info, outcomes, pathways, financial
    prePopulate(selectedinitiative, session)
    
    # render conditional panels for outcome and pathway subcategories
    outcome1Conditional(selectedinitiative, input, output)
    outcome2Conditional(selectedinitiative, input, output)
    pathway1Conditional(selectedinitiative, input, output)
    pathway2Conditional(selectedinitiative, input, output)
    
    outputOptions(output, "out1numtrend", suspendWhenHidden = FALSE)
    outputOptions(output, "out2numtrend", suspendWhenHidden = FALSE)
    outputOptions(output, "path1numtrend", suspendWhenHidden = FALSE)
    outputOptions(output, "path2numtrend", suspendWhenHidden = FALSE)
    
    # pre-populate outcome and pathway data tables
    source('outcomepathwaydata.R', local = T)
    
    out1subcat1(out1subcat1initfact)
    out1subcat2(out1subcat2initfact)
    out1subcat3(out1subcat3initfact)
    out2subcat1(out2subcat1initfact)
    out2subcat2(out2subcat2initfact)
    out2subcat3(out2subcat3initfact)
    path1subcat1(path1subcat1initfact)
    path1subcat2(path1subcat2initfact)
    path1subcat3(path1subcat3initfact)
    path2subcat1(path2subcat1initfact)
    path2subcat2(path2subcat2initfact)
    path2subcat3(path2subcat3initfact)
    
    # pre-populate milestone table
    filtered_milestones <- 
      milestones %>% filter(initiative==selectedinitiative & timestamp==max(timestamp[initiative==selectedinitiative], na.rm=T)) %>%
                 select(milestone,target,milestonestatus,milestonejust,milestoneactive)
    
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
  
  out2subcat1 <- reactiveVal()
  out2subcat2 <- reactiveVal()
  out2subcat3 <- reactiveVal()
  
  path1subcat1 <- reactiveVal()
  path1subcat2 <- reactiveVal()
  path1subcat3 <- reactiveVal()
  
  path2subcat1 <- reactiveVal()
  path2subcat2 <- reactiveVal()
  path2subcat3 <- reactiveVal()
  
  source('renderoutcomepathwaytables.R', local = T)

  
  # ---- POP UPS AND GUIDANCE ----
  
  source('guidance.R', local = T)
  
  
  # ---- SAVING INPUT DATA ----
  
  # formulate all inputs and datatables into dataframes 
  source('formdata.R', local = T)
  
  
  # define 'saveData' function
  saveData <- function(data1,data2,data3,data4) {

    data1 <- rbind(initiative_dim,
                   data1 %>% mutate(timestamp = format(Sys.time(), format="%Y%m%d%H%M")))

    data2 <- rbind(init_indicator_dim,
                   data2 %>% mutate(timestamp = format(Sys.time(), format="%Y%m%d%H%M")))

    data3 <- rbind(milestones,
                   data3 %>% mutate(timestamp = format(Sys.time(), format="%Y%m%d%H%M")))

    data4 <- rbind(init_indicator_fact,
                   data4 %>% mutate(timestamp = format(Sys.time(), format="%Y%m%d%H%M")))
    
    
    # Write the files to the local system
    write.csv(x = data1,
      file = "responses/FY21_initiative_dim.csv", 
      row.names = FALSE, quote = TRUE)
    drop_upload("responses/FY21_initiative_dim.csv", path = "GitHub/ConservationDashboard/COMPS/responses/", dtoken = token)
  
    write.csv(x = data2,
      file = "responses/FY21_init_indicator_dim.csv",
      row.names = FALSE, quote = TRUE)
    drop_upload("responses/FY21_init_indicator_dim.csv", path = "GitHub/ConservationDashboard/COMPS/responses/", dtoken = token)

    write.csv(x = data3,
      file = "responses/FY21_milestones.csv",
      row.names = FALSE, quote = TRUE)
    drop_upload("responses/FY21_milestones.csv", path = "GitHub/ConservationDashboard/COMPS/responses/", dtoken = token)

    write.csv(x = data4,
      file = "responses/FY21_init_indicator_fact.csv",
      row.names = FALSE, quote = TRUE)
    drop_upload("responses/FY21_init_indicator_fact.csv", path = "GitHub/ConservationDashboard/COMPS/responses/", dtoken = token)
    
  }

  
  # when the Save button is clicked, save the form data & provide confirmation pop-up
  observeEvent(input[["save"]], {
    
    initiative_dim <<- drop_read_csv('GitHub/ConservationDashboard/COMPS/responses/FY21_initiative_dim.csv', dtoken = token)
    init_indicator_dim <<- drop_read_csv('GitHub/ConservationDashboard/COMPS/responses/FY21_init_indicator_dim.csv', dtoken = token)
    init_indicator_fact <<- drop_read_csv('GitHub/ConservationDashboard/COMPS/responses/FY21_init_indicator_fact.csv', dtoken = token)
    milestones <<- drop_read_csv('GitHub/ConservationDashboard/COMPS/responses/FY21_milestones.csv', dtoken = token)
    
    saveData(formData1(),formData2(),formData3(),formData4())
    shinyalert("Saved!","You may now close your window.", type = "success")

    
  })

}


# ---- Run the application ----

shinyApp(ui = ui, server = server)




