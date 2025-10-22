# Written by Erin M. Buchanan

# Libraries ---------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(dplyr)
library(qualtRics)
library(DT)

library(sf)
library(leaflet)
library(RColorBrewer)

# source("api_key.R")
source("tabs.R")

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

the_study_2 <- fetch_survey("SV_6PZLNHEoQr9fQoK",
                            convert = FALSE,
                            label = FALSE)

if (nrow(the_study_2) > 0) {
  all_data <- the_study %>% 
    bind_rows(the_study_2) %>% 
    select(LabID, CountryLan, country, RecordedDate, Progress, 
           ParticipantCode, currency, totalmoney) %>% 
    mutate(totalmoney = round(totalmoney, digits = 2)) %>% 
    mutate(ParticipantCode = as.character(ParticipantCode)) 
  
} else {
  all_data <- the_study %>% 
    select(LabID, CountryLan, country, RecordedDate, Progress, 
           ParticipantCode, currency, totalmoney) %>% 
    mutate(totalmoney = round(totalmoney, digits = 2)) %>% 
    mutate(ParticipantCode = as.character(ParticipantCode)) 
  
}

# fix issue with french codes
all_data <- all_data %>% 
  mutate(country = ifelse(
    LabID == "4412", "Canada", ifelse(
      LabID == "4570", "Switzerland", ifelse(
        LabID == "4315", "Canada", ifelse(
          LabID == "3226", "India", ifelse(
            LabID == "4113", "Polska", ifelse(
              LabID == "3157", "Philippines", country
            )
          )
        )
      )
      
    )
  ),
  CountryLan = ifelse(
    LabID == "4315", "Canada_EN", ifelse(
      LabID == "1113", "Poland_Polish", CountryLan
    )
  )
  )


show_DF <- the_study %>% 
  filter(!(LabID == "SurveyGenerated")) %>% 
  filter(!(grepl("Test|test", LabID))) %>% 
  filter(!is.na(LabID)) %>% 
  select(LabID, CountryLan, country, RecordedDate, Progress, 
         ParticipantCode, currency, totalmoney) %>% 
  mutate(totalmoney = round(totalmoney, digits = 2)) %>% 
  mutate(ParticipantCode = as.character(ParticipantCode)) %>%
  mutate(country = if_else(CountryLan == "France_French", "France", 
                           if_else(CountryLan == "Benin_French", "Benin", country))) %>%
  select(-CountryLan)

lab_DF <- show_DF %>% 
  filter(Progress >= 95) %>%
  # filter(!is.na(totalmoney)) %>%
  mutate(LabID = as.numeric(gsub("[[:punct:]]", "", LabID))) %>% 
  mutate(LabID = as.character(LabID)) %>% 
  group_by(LabID) %>%
  summarize(sample_size = n(),
            StartDate = min(RecordedDate, na.rm = T), 
            EndDate = max(RecordedDate, na.rm = T),
            totalmoney = sum(totalmoney, na.rm = T),
            currency = names(which.max(table(currency))))

country_DF <- show_DF %>% 
  mutate(country = replace(country, country == 'the United States', 'United States')) %>%
  mutate(country = replace(country, country == 'the United Kingdom', 'United Kingdom')) %>%
  mutate(country = replace(country, country == 'The Netherlands', 'Netherlands')) %>%
  mutate(country = replace(country, country == 'Nederland', 'Netherlands')) %>% 
  mutate(country = replace(country, country == 'España', 'Spain')) %>%   
  mutate(country = replace(country, country == 'Italia', 'Italy')) %>%
  mutate(country = replace(country, country == 'Polska', 'Poland')) %>%
  mutate(country = replace(country, country == 'România', 'Romania')) %>%
  mutate(country = replace(country, country == 'México', 'Mexico')) %>%
  mutate(country = replace(country, country == 'Srbija', 'Serbia')) %>%
  mutate(country = replace(country, country == 'Türkiye', 'Turkey')) %>%
  mutate(country = replace(country, country == 'России', 'Russia')) %>%
  mutate(country = replace(country, country == 'ישראל', 'Israel')) %>%
  mutate(country = replace(country, country == '中国', 'China')) %>%
  mutate(country = replace(country, country == '日本', 'Japan')) %>%
  mutate(country = replace(country, country == 'Česká Republika', 'Czech Republic')) %>%
  mutate(country = replace(country, country == 'Schweiz', 'Switzerland')) %>%
  mutate(country = replace(country, country == 'Österreich', 'Austria')) %>%
  mutate(country = replace(country, country == 'Danmark', 'Denmark')) %>%
  mutate(country = replace(country, country == 'Deutschland', 'Germany')) %>%
  mutate(country = replace(country, country == 'Brasil', 'Brazil')) %>%
  mutate(country = replace(country, country == 'السعودية','Saudi Arabia')) %>%
  mutate(country = replace(country, country == 'الكويت','Kuwait')) %>%
  mutate(country = replace(country, country == 'Ελλάδα', 'Greece')) %>%
  group_by(country) %>% 
  summarize(sample_size = n())

#Map Stuff --------------------------------------------------------------------

# download.file(
#   "https://raw.githubusercontent.com/holtzy/R-graph-gallery/master/DATA/world_shape_file.zip",
#   destfile = "world_shape_file.zip")
# 
# system("unzip world_shape_file.zip")

world_sf <- read_sf(paste0(
  getwd(), "/TM_WORLD_BORDERS_SIMPL-0.3.shp"
))

# clean up country
country_clean <- country_DF %>% 
  filter(!is.na(country)) %>% 
  mutate(sample_sizeF = ifelse(
    sample_size < 100, "< 100", ifelse(
      sample_size >= 100 & sample_size < 200, "100-199", ifelse(
        sample_size >= 200 & sample_size < 300, "200-299",ifelse(
        sample_size >= 300 & sample_size < 400, "300-399", "400+"
      )
    )
  )
  )
  )

world_join <- world_sf %>% 
  left_join(country_clean, by = c("NAME" = "country")) 

mypalette <- colorFactor(
  palette = "viridis",  # Or any other color palette like "Set1", "Accent"
  domain = world_join$sample_sizeF,
  na.color = "transparent"
)
mytext <- paste(
  "Country: ", world_join$NAME, "<br/>",
  "Sample Size: ", round(world_join$sample_size, 2),
  sep = ""
) %>%
  lapply(htmltools::HTML)


# UI ----------------------------------------------------------------------

ui <- dashboardPage(skin = 'green',
              dashboardHeader(title = "PSA 008 Tracker"),
              dashboardSidebar(
                sidebarMenu(
                  menuItem(tags$b("Overall"), tabName = "overall_tab"),
                  menuItem(tags$b("Countries"), tabName = "country_tab"),
                  menuItem(tags$b("All Data"), tabName = "all_tab")
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
                  country_tab,
                  all_tab
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
  
  output$all_DF_table <- renderDT({
    datatable(all_data, rownames = F,
              filter = "top",
              options = list(dom = 'tp'))
  })
  
  output$overall_total2 <- renderInfoBox({
    infoBox("Complete", nrow(show_DF %>% filter(Progress > 95)), icon = icon("users"), color = "blue")
  })
  
  output$overall_total <- renderInfoBox({
    infoBox("All", nrow(show_DF), icon = icon("users"), color = "purple")
  })
  
  
  output$country_DF_table <- renderDT({
    datatable(country_DF, rownames = F,
              filter = "top",
              options = list(dom = 'tp'))
  })
    
  output$map <- renderLeaflet({
    leaflet(world_join) %>%
      addTiles() %>%
      setView(lat = 10, lng = 0, zoom = 2) %>%
      addPolygons(
        data = world_join,
        fillColor = ~ mypalette(sample_sizeF),
        stroke = TRUE,
        fillOpacity = 0.9,
        color = "white",
        weight = 0.3,
        label = mytext,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      )%>%
      addLegend(
        pal = mypalette, values = ~sample_sizeF, opacity = 0.9,
        title = "Participant Count", position = "bottomleft"
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
