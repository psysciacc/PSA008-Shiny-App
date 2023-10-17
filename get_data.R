
# Libraries ---------------------------------------------------------------

library(qualtRics)
library(dplyr)
library(tidyr)

# erin's test copy
survey_id <- "SV_cRMMbFKrVWfF6oS"

# real survey
# SV_1Nx2hkje0bInbee

# get survey data
variables_needed <- c("nationality", "playedwith", 
                      "Allocate_IG_M_1", "Allocate_OG_M_1", 
                      "Allocate_TG_M_1", "Allocate_IG_P_1",
                      "Allocate_OG_P_1", "Allocate_TG_P_1", 
                      "Allocate_TG_M_2", "Allocate_TG_M_2",
                      "Allocate_TG_Nat_2", 
                      "Allocate_IG_Nat_1", "Allocate_OG_Nat_1",
                      "Allocate_TG_Nat_1", "ParticipantCode")
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
                        -Allocate_TG_Nat_1),
               names_to = "type", 
               values_to = "amount") %>% 
  filter(!is.na(amount)) %>% 
  mutate(minimal = substr(type,13,13)) 

codes <- unique(small_DF$ParticipantCode)

sampled_DF <- small_DF %>% 
  filter(ParticipantCode == codes[sample(1:length(codes), 1)])

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


# Allocate_IG_M_1
# Allocate_OG_M_1
# Allocate_TG_M_1 

#ig = ingroup
#og = othergroup
#tg = both groups 
# 1 indicates self
# either p or m

# pull from options in saved data
# ok so where do we get the options 

