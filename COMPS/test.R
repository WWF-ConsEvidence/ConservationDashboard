shinyApp(
  ui = fluidPage(
    titlePanel("WWF US Initiative Report"),
    div(
      id = "form",
      tabsetPanel(type="tabs",
                  tabPanel("Initiatve Information", 
                           textInput("Initiative", "Initiative Name", ""),
                           textInput("Initiative.lead", "Name of Initiative Lead"),
                           textInput("Initiative.email", "Initiative Lead Email"),
                           textInput("Initiative.start", "Initiative Start Date"),
                           textInput("Initiative.end", "Initiative End Date"),
                           textInput("Initiative.statement", "Initiative Statement"),
                           h5("Guidance on initiative statement")
                           selectInput("Overall.status", "Initative Status Assessment Category", c("","Opportunity","Progress","Barrier","Support","Contingent")),
                           textInput ("Overall.just", "Initiative Status Assessment Justification"),
                           h5 ("Guidance on initiative status justification")
                           )
                  tabPanel("Outcome1",
                           XX)
                  tabPanel("Pathways", XX)
                  tabPanel("Milestones",XX)
      )
                  
                 
  
      checkboxInput("used_shiny", "I've built a Shiny app in R before", FALSE),
      sliderInput("r_num_years", "Number of years using R", 0, 25, 2, ticks = FALSE),
      selectInput("os_type", "Operating system used most frequently",
                  c("",  "Windows", "Mac", "Linux")),
      actionButton("submit", "Submit", class = "btn-primary")
    )
  ),
  server = function(input, output, session) {
  }
)



