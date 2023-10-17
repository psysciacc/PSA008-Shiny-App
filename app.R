# Written by Erin M. Buchanan


# Libraries ---------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(dplyr)
library(qualtRics)
library(DT)

source("tabs.R")

# Get Study Data ----------------------------------------------------------

# erin's test copy
survey_id <- "SV_cRMMbFKrVWfF6oS"

the_study <- fetch_survey(survey_id, 
                          # use this later for filtering out nonsense 
                          #start_date = "2018-10-01",
                          #end_date = "2018-10-31",
                          #include_questions = variables_needed, 
                          convert = FALSE,
                          label = FALSE)

show_DF <- the_study %>% 
  select(RecordedDate, Progress, ParticipantCode, totalmoney, currency)

lab_DF <- show_DF %>% 
  filter(Progress > 50) %>% 
  group_by(ParticipantCode) %>% 
  summarize(sample_size = n())


# UI ----------------------------------------------------------------------

ui <- dashboardPage(skin = 'purple',
              dashboardHeader(title = "PSA 008 Tracker"),
              dashboardSidebar(
                sidebarMenu(
                  menuItem(tags$b("Overall"), tabName = "overall_tab")
                )
              ),
              
              dashboardBody(
                
                ## add a custom css file
                tags$head(tags$style(HTML('
                .main-header .logo {
                  font-weight: bold;
                  font-size: 16px;
                }
                .box.box-solid.box-primary>.box-header {
                  color:#fff;
                  background:#666666
                }
                .box.box-solid.box-primary {
                  border-bottom-color:#666666;
                  border-left-color:#666666;
                  border-right-color:#666666;
                  border-top-color:#666666;
                }'))),
                
                ## show the tab items
                tabItems(
                  overall_tab
                ) # end tabItems
              ) # end dashboardBody
            ) # end dashboardPage


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$show_DF_table <- renderDT({
    
    datatable(show_DF, rownames = F,
              filter = "top",
              options = list(dom = 'tp'))
  })
  
  output$lab_DF_table <- renderDT({
    
    datatable(lab_DF, rownames = F,
              filter = "top",
              options = list(dom = 'tp'))
  })


}

# Run the application 
shinyApp(ui = ui, server = server)
