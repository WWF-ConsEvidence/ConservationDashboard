#
# code:  WWF COMPS Initiative Report Shiny Data Entry App
#
# author: Louise Glew, louise.glew@wwf.org
# created: March 2020
# modified: 


# ---- load libraries, data, define fields ----

# - libraries
pacman::p_load(rio, dplyr, shiny, shinyBS, DT)

# - pre-populate data
initiative_dim <- import('C:/Users/claborn-intern/Dropbox (MPAMystery)/GitHub/ConservationDashboard/2_Oct2019_US/2_FlatDataFiles/ConsDB_Input_2019/May_2020_updates/fy20_initiative_reporting_dim_2020_0409.xlsx')
init_indicator_dim <- import('C:/Users/claborn-intern/Dropbox (MPAMystery)/GitHub/ConservationDashboard/2_Oct2019_US/2_FlatDataFiles/ConsDB_Input_2019/May_2020_updates/fy20_initiative_indicators_dim_2020_0409.xlsx')

# - define fields to be saved from form
initiative_dim_fields <- c("initiative","initiativelead","email")
init_indicator_dim_fields <- c("initiative","outcome1statement")



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
                choices = c("",sort(unique(initiative_dim$Practice))), 
                selectize = T),
    selectInput(inputId = "initiativeoptions",
                label = "Initiative Name",
                choices = c("", sort(unique(initiative_dim$Initiative))), 
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

        # Show a plot of the generated distribution
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
              selectInput("Overall.status", "Initative Status Assessment Category", c("","Opportunity","Progress","Barrier","Support","Contingent")),
              #popify(selectInput("Overall.status", "Initative Status Assessment Category", c("","Opportunity","Progress","Barrier","Support","Contingent")), "Status Assessment Categories",
              #       "<b> Opportunity: </b> Critical opportunity to scale or leverage Initiative. <br/> 
              #      <b> Progress: </b> On track or affected by minor issues that are being addressed by Initiative team. <br/> 
              #      <b> Barrier: </b> Implementation is delayed by external factors (e.g., political will, partners engagement). <br/> 
              #      <b> Support: </b> Initiative requires leadership support to address one or more issues. <br/> 
              #      <b> Contingent: </b> Contingent on achievement of preceding milestone.", placement = "bottom", trigger = "hover"),
              textInput ("Overall.just", "Initiative Status Assessment Justification"),
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
                     textInput("Indicator.name", "Indicator Name"),
                     textInput("Indicator.label", "Indicator Label"),
                     textInput("Indicator.label.abbr", "Abbreviated Indicator Label"),
                     textInput("Indicator.description", "Indicator Description"),
                     textInput("Units", "Indicator Units"),
                     textInput("Source","Indicator Data Source"),
                     selectInput(inputId = "out1numtrend",
                                 label = "How many trend lines would you like to display for this indicator?",
                                 choices = c("",1,2,3),
                                 selectize = T)),
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
              tags$b("Financial Information"))
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
  
  initiative_dim_data <- import('responses/fy21_initiative_dim.csv')
  init_indicator_dim_data <- import('responses/fy21_init_indicator_dim.csv')
  
  observe({ 
    
    initiativeoptions <- c("",sort(initiative_dim$Initiative[which(initiative_dim$Practice==input$goal)]))
    
    updateSelectInput(session, 
                      inputId = "initiativeoptions",
                      label = "Initiative Name",
                      choices = initiativeoptions,
                      selected = "")

  })
 
  observeEvent(input$populatereport, {
    
    selectedinitiative <- input$initiativeoptions
    
    updateTextInput(session,
                    inputId = "initiative",
                    value = selectedinitiative)
    
    updateTextInput(session,
                    inputId = "initiativelead",
                    value = initiative_dim$Initiative.lead[initiative_dim$Initiative==selectedinitiative])
    
    updateTextInput(session,
                    inputId = "email",
                    value = initiative_dim$Email[initiative_dim$Initiative==selectedinitiative])
    
    updateTextInput(session,
                    inputId = "initiativestart",
                    value = initiative_dim$Initiative.start[initiative_dim$Initiative==selectedinitiative])
    
    updateTextInput(session,
                    inputId = "initiativeend",
                    value = initiative_dim$Initiative.end[initiative_dim$Initiative==selectedinitiative])
    
    updateTextInput(session,
                    inputId = "outcome1statement",
                    value = init_indicator_dim$Statement[init_indicator_dim$Initiative==selectedinitiative][1])
  })
  
  # Whenever a field is filled, aggregate all form data
  formData1 <- reactive({
    initiative_dim_data <- sapply(initiative_dim_fields, function(x) input[[x]])
    initiative_dim_data
  })
  
  formData2 <- reactive({
    init_indicator_dim_data <- sapply(init_indicator_dim_fields, function(x) input[[x]])
    init_indicator_dim_data
  })
  
  
  # Define'saveData' function
  saveData <- function(data1,data2) {
    data1 <- rbind(initiative_dim_data,t(data1))
    data2 <- rbind(init_indicator_dim_data,t(data2))
    
    # Write the file to the local system
    write.csv(
      x = data1,
      file = "responses/FY21_initiative_dim.csv", 
      row.names = FALSE, quote = TRUE
    )
    
    # Write the file to the local system
    write.csv(
      x = data2,
      file = "responses/FY21_init_indicator_dim.csv", 
      row.names = FALSE, quote = TRUE
    )
    
  }
  
  # When the Save button is clicked, save the form data
  observeEvent(input$save, {
    saveData(formData1(),formData2())
    output$confirmsave <- renderText({ "Saved!" })
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





#milestones <- data.frame("milestone"=c("A","B","C","D"),
#                         "date" =c(11/2019, 12/2020, 06/2021, 03/2020),
#                         "status"= c("Barrier","Barrier","Progress","Support"),
#                         "justification"=c("xyz", "abc", "def", "ghi"))
