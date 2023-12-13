library(qualtRics)
library(rio)
library(dplyr)
library(tidyr)
library(gert)

# list of variables needed
variables_needed <- c("nationality", "playedwith", 
                      "Allocate_IG_M_1", "Allocate_OG_M_1", 
                      "Allocate_TG_M_1", "Allocate_IG_P_1",
                      "Allocate_OG_P_1", "Allocate_TG_P_1", 
                      "Allocate_TG_M_2", "Allocate_TG_M_2",
                      "Allocate_TG_Nat_2", 
                      "Allocate_IG_Nat_1", "Allocate_OG_Nat_1",
                      "Allocate_TG_Nat_1", "ParticipantCode")

# get pilot data ----------------------------------------------------------
pilot_DF <- import("pilot_data.csv") %>% 
  select(all_of(variables_needed)) %>% 
  pivot_longer(cols = c(-nationality, -playedwith, -ParticipantCode,
                        -Allocate_IG_Nat_1, -Allocate_OG_Nat_1, 
                        -Allocate_TG_Nat_1, -Allocate_TG_Nat_2),
               names_to = "type", 
               values_to = "amount") %>% 
  filter(!is.na(amount)) %>% 
  mutate(minimal = substr(type,13,13)) 

# get qualtrics data ------------------------------------------------------
survey_id <- "SV_8DoIVJyXWc1Ea7I" # update this

# get the survey data
the_study <- fetch_survey(survey_id, 
                          # use this later for filtering out nonsense 
                          #start_date = "2018-10-01",
                          #end_date = "2018-10-31",
                          #include_questions = variables_needed, 
                          convert = FALSE,
                          label = FALSE)

small_DF <- the_study %>% 
  select(all_of(variables_needed)) %>% 
  pivot_longer(cols = c(-nationality, -playedwith, -ParticipantCode,
                        -Allocate_IG_Nat_1, -Allocate_OG_Nat_1, 
                        -Allocate_TG_Nat_1, -Allocate_TG_Nat_2),
               names_to = "type", 
               values_to = "amount") %>% 
  filter(!is.na(amount)) %>% 
  mutate(minimal = substr(type,13,13)) %>% 
  mutate(ParticipantCode = as.character(ParticipantCode))


# grab one random ---------------------------------------------------------
if (nrow(small_DF) > 0) { 
  
  sampled_DF <- bind_rows(
    pilot_DF,
    small_DF
  ) %>% 
    na.omit()
  
} else {
    sampled_DF <- pilot_DF %>% 
      na.omit()
  }

codes <- unique(sampled_DF$ParticipantCode)

sampled_DF <- sampled_DF %>% 
  filter(ParticipantCode == codes[sample(1:length(codes), 1)])
  

# write json --------------------------------------------------------------
json_write <- paste0('[
  {
    "minimal": "', sampled_DF$minimal[1], '", 
    "nationality": "', sampled_DF$nationality[1], '", 
    "dg_min_in_self": ', sampled_DF %>% 
                       filter(type == "Allocate_IG_M_1" | type == "Allocate_IG_P_1") %>% 
                       pull(amount), ', 
    "dg_min_out_self": ', sampled_DF %>% 
                       filter(type == "Allocate_OG_M_1" | type == "Allocate_OG_P_1") %>% 
                       pull(amount), ',
    "dg_min_in_out": ',sampled_DF %>% 
                       filter(type == "Allocate_TG_M_2" | type == "Allocate_TG_P_2") %>% 
                       pull(amount), ',
    "dg_nat_in_self": ', sampled_DF$Allocate_IG_Nat_1[1], ',
    "dg_nat_out_self": ', sampled_DF$Allocate_OG_Nat_1[1], ',
    "dg_nat_in_out": ', sampled_DF$Allocate_TG_Nat_2[1], '
  }
]')

# group role recipient section in qualtrics
# need to set up something to write out in json format like this:
writeLines(json_write, "group_assignment.json")

# push to github ----------------------------------------------------------
setwd("~/ShinyApps/PSA008-Shiny-App.git")
git_add(".")
git_commit("Adding a file", author = "jerry <jerry@gmail.com>")

