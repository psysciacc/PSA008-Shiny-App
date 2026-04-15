# Written by Erin M. Buchanan

# Libraries ---------------------------------------------------------------

library(dplyr)
library(qualtRics)
library(DT)

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


saveRDS(the_study, "~/ShinyApps/PSA008/data/the_study.rds")
saveRDS(the_study_2, "~/ShinyApps/PSA008/data/the_study_2.rds")
