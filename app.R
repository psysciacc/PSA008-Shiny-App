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
  filter(Progress == 100) %>%
  # filter(!is.na(totalmoney)) %>%
  mutate(LabID = as.numeric(gsub("[[:punct:]]", "", LabID))) %>% 
  group_by(LabID) %>%
  summarize(sample_size = n(),
            StartDate = min(RecordedDate), 
            EndDate = max(RecordedDate),
            totalmoney = sum(totalmoney, na.rm = T),
            currency = names(which.max(table(currency))))

country_DF <- show_DF %>% 
  group_by(country) %>% 
  summarize(sample_size = n())

#Map Stuff --------------------------------------------------------------------

# download.file(
#   "https://raw.githubusercontent.com/holtzy/R-graph-gallery/master/DATA/world_shape_file.zip",
#   destfile = "world_shape_file.zip"
# )
# system("unzip world_shape_file.zip")
world_sf <- read_sf(paste0(
  getwd(), "/TM_WORLD_BORDERS_SIMPL-0.3.shp"
))
world_sf <- world_sf %>%
  mutate(POP2005 = ifelse(POP2005 == 0, NA, round(POP2005 / 1000000, 2)))

mypalette <- colorNumeric(
  palette = "viridis", domain = world_sf$POP2005,
  na.color = "transparent"
)
# mypalette(c(45, 43))

m <- leaflet(world_sf) %>%
  addTiles() %>%
  setView(lat = 10, lng = 0, zoom = 2) %>%
  addPolygons(fillColor = ~ mypalette(POP2005), stroke = FALSE)

mytext <- paste(
  "Country: ", world_sf$NAME, "<br/>",
  "Area: ", world_sf$AREA, "<br/>",
  "Population: ", round(world_sf$POP2005, 2),
  sep = ""
) %>%
  lapply(htmltools::HTML)

mybins <- c(0, 10, 20, 50, 100, 500, Inf)
mypalette <- colorBin(
  palette = "YlOrBr", domain = world_sf$POP2005,
  na.color = "transparent", bins = mybins
)

m <- leaflet(world_sf) %>%
  addTiles() %>%
  setView(lat = 10, lng = 0, zoom = 2) %>%
  addPolygons(
    fillColor = ~ mypalette(POP2005),
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
  ) %>%
  addLegend(
    pal = mypalette, values = ~POP2005, opacity = 0.9,
    title = "Population (M)", position = "bottomleft"
  )

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
