overall_tab <- 
  tabItem(tabName = "overall_tab",
          fluidRow(
            box(
              title = tags$b("Overall Counts"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              infoBoxOutput("overall_total"),
              infoBoxOutput("overall_total2")
            ),           
            # participant check ----
            box(
              title = tags$b("Participant Counts"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p("This section allows you to search for 
                participant information to determine 
                if they have completed the study. "),
              DTOutput("show_DF_table")
              
            ), # close box
            
            # lab table ----
            box(
              title = tags$b("Lab Counts"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p("This section allows you to see 
                the total number of participants 
                from your lab."),
              DTOutput("lab_DF_table")
              
            ) # close box
            
          ) #close row
          
  ) #close tab

country_tab <- 
  tabItem(tabName = "country_tab",
          fluidRow(
            
            # country table ----
            box(
              title = tags$b("Country Summary"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p("This section allows you to see 
                the total number of participants by country."),
              DTOutput("country_DF_table")
              
            ), # close box
            
            box(
              title = tags$b("Country Map"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p("This section allows you to see 
                the total number of participants by country."),
              leafletOutput("map"),
              
            ) # close box
            
          ) #close row
          
  ) #close tab

all_tab <- 
  tabItem(tabName = "all_tab",
          fluidRow(
            # country table ----
            box(
              title = tags$b("All Data"),
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              p("This section allows you to see all data 
                in case you can't seem to find a participant number. 
                Sometimes the data does not refresh right away, so you 
                can check back (usually an hour or two) - but it may 
                also be that we filtered the data on the first page. "),
              DTOutput("all_DF_table")
            ) # close box
          ) #close row
        ) #close tab