overall_tab <- 
  tabItem(tabName = "overall_tab",
          fluidRow(
            
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
              
            ) # close box
            
          ) #close row
          
  ) #close tab