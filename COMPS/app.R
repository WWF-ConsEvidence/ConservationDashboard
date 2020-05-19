#
# code:  WWF COMPS Initiative Report Shiny Data Entry App
#
# author: Louise Glew, louise.glew@wwf.org
# created: March 2020
# modified: 


# ---- load libraries ----

# - libraries
pacman::p_load(rio, bit64, dplyr, shiny, shinyBS, DT)


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
                    label = "Initiative Start Date",
                    placeholder = "Year (YYYY)"),
          textInput(inputId = "initiativeend",
                    label = "Initiative End Date",
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
          textInput ("initiativejust", "Initiative Status Assessment Justification"),
          h5 ("Guidance on initiative status justification")
        ),
        
        tabPanel(
          tags$b("Outcomes"),
          column(6,
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
                 textInput(inputId = "out1indicatorlabelabbr", 
                           label = "Abbreviated Indicator Label"),
                 textInput(inputId = "out1indicatorunits", 
                           label= "Indicator Units"),
                 textInput(inputId = "out1indicatorsource",
                           label = "Indicator Data Source"),
                 h5(tags$em("You may display up to three trend lines per outcome. Please indicate the labels for disaggregation (if any) 
                            you would like to display for this outcome in the boxes marked 'Trend 1', 'Trend 2', and 'Trend 3' below. 
                            If no aggregation is desired, please fill in the appropriate data for your indicator in the column for 'Trend 1' in the table below.")),
                 # selectInput(inputId = "out1numtrend",
                 #             label = "How many trend lines would you like to display for this indicator?",
                 #             choices = c("",1,2,3),
                 #             selectize = T),
                 textInput(inputId = "out1subcat1",
                           label = "Trend 1 Label",
                           placeholder = "Leave blank if no disaggregation"),
                 textInput(inputId = "out1subcat2",
                           label = "Trend 2 Label",
                           placeholder = "Leave blank if no disaggregation"),
                 textInput(inputId = "out1subcat3",
                           label = "Trend 3 Label",
                           placeholder = "Leave blank if no disaggregation"),
                 DT::dataTableOutput(outputId = "outcome1data")
                 ),
          column(6,
                 tags$b("OUTCOME 2"),
                 tags$br())
        ),
        
        tabPanel(
          tags$b("Pathways"),
          h5("PATHWAY 1"),
          h5("PATHWAY 2")
        ),
        
        tabPanel(
          tags$b("Milestones"),
          datatable(milestones, editable=TRUE)
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
  
  # pre-populate data
  initiative_dim <<- import('COMPS/responses/FY21_initiative_dim_TEST.csv', colClasses = c(initiativekey = "character"))
  init_indicator_dim <<- import('2_Oct2019_US/2_FlatDataFiles/ConsDB_Input_2019/May_2020_updates/fy20_initiative_indicators_dim_2020_0409.xlsx')
  init_indicator_fact <<- import('2_Oct2019_US/2_FlatDataFiles/ConsDB_Input_2019/May_2020_updates/fy20_initiative_indicators_fact_2020_0409.xlsx')
  
  
  initiative_dim_data <- import('COMPS/responses/FY21_initiative_dim.csv')
  init_indicator_dim_data <- import('COMPS/responses/FY21_init_indicator_dim.csv')

  
  # update initiative selection options based on selected goal
  observe({
    
    updateSelectInput(session, 
                      inputId = "initiativeoptions",
                      choices = c("",sort(initiative_dim$initiative[which(initiative_dim$goal==input$goal)])),
                      selected = "")

  })

  # pre-populate text boxes based on selected initiatives
  observeEvent(input$populatereport, {
    selectedinitiative <- input$initiativeoptions
    
    updateTextInput(session,
                    inputId = "initiative",
                    value = selectedinitiative)
    
    updateTextInput(session,
                    inputId = "initiativelead",
                    value = initiative_dim$initiativelead[initiative_dim$initiative==selectedinitiative])
    
    updateTextInput(session,
                    inputId = "email",
                    value = initiative_dim$email[initiative_dim$initiative==selectedinitiative])
    
    updateTextInput(session,
                    inputId = "initiativestart",
                    value = initiative_dim$initiativestart[initiative_dim$initiative==selectedinitiative])
    
    updateTextInput(session,
                    inputId = "initiativeend",
                    value = initiative_dim$initiativeend[initiative_dim$initiative==selectedinitiative])
    
    updateTextInput(session,
                    inputId = "initiativestatement",
                    value = initiative_dim$initiativestatement[initiative_dim$initiative==selectedinitiative])
    
    updateTextInput(session,
                    inputId = "outcome1statement",
                    value = init_indicator_dim$Statement[init_indicator_dim$Initiative==selectedinitiative][1])
    
    updateTextInput(session,
                    inputId ="out1indicator",
                    value = init_indicator_dim$Indicator.name[init_indicator_dim$Initiative==selectedinitiative][1])
    
    updateTextInput(session,
                    inputId = "out1indicatorlabel", 
                    value = init_indicator_dim$Indicator.label[init_indicator_dim$Initiative==selectedinitiative][1])
    
    updateTextInput(session,
                    inputId = "out1indicatorlabelabbr",
                    value = init_indicator_dim$Indicator.label.abbr[init_indicator_dim$Initiative==selectedinitiative][1])
    
    # updateTextInput(session,
    #                 inputId = "out1indicatorunits",
    #                 value = )
    # 
    # updateTextInput(session,
    #                 inputId = "out1indicatorsource",
    #                 value = )
    # 
    # updateTextInput(session,
    #                 inputId = "out1subcat1",
    #                 value = )
    
  })
 
  # # populate outcome and pathway data tables for selected initiative
  # outcome1data <- data.frame(init_indicator_fact)
  # 
  # output$outcome1data <- DT::renderDataTable({ 
  #  outcome1data
  #  })
 
  # - define fields to be saved from form
  initiative_dim_fields <- c("goal","initiative","initiativelead","email","initiativestart","initiativeend",
                             "initiativestatement","initiativestatus","initiativejust","fundsneeded","fundssecured","fundsanticipated")
  init_indicator_dim_fields <- c("goal","initiative","statement","indicatordescription","indicatorlabel","indicatorlabelabbr","indicatorunits","indicatorsource",
                                 "indicatortype","displayorder")
  out1_indicator_dim_fields <- c("goal","initiative","outcome1statement","out1indicator","out1indicatorlabel","out1indicatorlabelabbr",
                                 "out1indicatorunits","out1indicatorsource","out1subcat1","out1subcat2","out1subcat3")
  out2_indicator_dim_fields <- c("goal","initiative","outcome2statement","out2indicator","out2indicatorlabel","out2indicatorlabelabbr",
                                 "out2indicatorunits","out2indicatorsource","out2subcat1","out2subcat2","out2subcat3")
  path1_indicator_dim_fields <- c("goal","initiative","pathway1statement","path1indicator","path1indicatorlabel")
  path2_indicator_dim_fields <- c("goal","initiative","pathway2statement","path2indicator","path2indicatorlabel")
  
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
    init_indicator_dim_data <- rbind.data.frame(t(as.matrix(sapply(out1_indicator_dim_fields, function(x) input[[x]], USE.NAMES = F))),
                                                t(as.matrix(sapply(out2_indicator_dim_fields, function(x) input[[x]], USE.NAMES = F))),
                                                t(as.matrix(sapply(path1_indicator_dim_fields, function(x) input[[x]], USE.NAMES = F))),
                                                t(as.matrix(sapply(path2_indicator_dim_fields, function(x) input[[x]], USE.NAMES = F)))) %>%
      mutate(indicatortype = c("Outcome","Outcome","Pathway","Pathway"),
             displayorder = 1:4)
    colnames(init_indicator_dim_data) <- init_indicator_dim_fields
    init_indicator_dim_data
  })
  
  
  # define'saveData' function
  saveData <- function(data1,data2) {
    data1 <- rbind(initiative_dim, 
                   data1 %>% mutate(timestamp = format(Sys.time(), format="%Y%m%d%H%M")))
    # data2 <- rbind(init_indicator_dim,data2)
    
    # Write the files to the local system
    write.csv(
      x = data1,
      file = "COMPS/responses/FY21_initiative_dim_TEST.csv", 
      row.names = FALSE, quote = TRUE
    )
    
    # write.csv(
    #   x = data2,
    #   file = "COMPS/responses/FY21_init_indicator_dim.csv", 
    #   row.names = FALSE, quote = TRUE
    # )
    
  }
  
  # When the Save button is clicked, save the form data
  observeEvent(input$save, {
    saveData(formData1(),formData2())
    output$confirmsave <- renderText({ "Saved!" })
    initiative_dim <<- import('COMPS/responses/FY21_initiative_dim_TEST.csv', colClasses = c(initiativekey = "character"))
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





# milestones <- data.frame("milestone"=c("A","B","C","D"),
#                         "date" =c(11/2019, 12/2020, 06/2021, 03/2020),
#                         "status"= c("Barrier","Barrier","Progress","Support"),
#                         "justification"=c("xyz", "abc", "def", "ghi"))
