# Written by Erin M. Buchanan

# Libraries ---------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(dplyr)
library(qualtRics)
library(DT)

# source("api_key.R")
source("tabs.R")

# group assignments 
# check the money assignments is correct 
# 

# Get Study Data ----------------------------------------------------------

# new last changes copy
survey_id <- "SV_8k10IEYESlBLf5I"

the_study <- fetch_survey(survey_id, 
                          # use this later for filtering out nonsense 
                          #start_date = "2018-10-01",
                          #end_date = "2018-10-31",
                          #include_questions = variables_needed, 
                          convert = FALSE,
                          label = FALSE)

show_DF <- the_study %>% 
  filter(!(LabID == "SurveyGenerated")) %>% 
  filter(!(grepl("Test|test", LabID))) %>% 
  filter(!is.na(LabID)) %>% 
  select(LabID, country, RecordedDate, Progress, 
         ParticipantCode, currency, totalmoney) %>% 
  mutate(totalmoney = round(totalmoney, digits = 2)) %>% 
  mutate(ParticipantCode = as.character(ParticipantCode))

lab_DF <- show_DF %>% 
  # filter(Progress > 50) %>% 
  group_by(LabID) %>% 
  summarize(sample_size = n())

country_DF <- show_DF %>% 
  group_by(country) %>% 
  summarize(sample_size = n())

# UI ----------------------------------------------------------------------

ui <- dashboardPage(skin = 'green',
              dashboardHeader(title = "PSA 008 Tracker"),
              dashboardSidebar(
                sidebarMenu(
                  menuItem(tags$b("Overall"), tabName = "overall_tab"),
                  menuItem(tags$b("Countries"), tabName = "country_tab")
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
                  overall_tab,
                  country_tab
                ) # end tabItems
              ) # end dashboardBody
            ) # end dashboardPage


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$show_DF_table <- renderDT({
    
    datatable(show_DF, rownames = F,
              filter = "top",
              options = list(dom = 'tp', scrollX = TRUE))
  })
  
  output$lab_DF_table <- renderDT({
    
    datatable(lab_DF, rownames = F,
              filter = "top",
              options = list(dom = 'tp'))
  })
  
  output$country_DF_table <- renderDT({
    
    datatable(country_DF, rownames = F,
              filter = "top",
              options = list(dom = 'tp'))
  })


}

# Run the application 
shinyApp(ui = ui, server = server)
