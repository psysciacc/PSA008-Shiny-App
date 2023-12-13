# set_canvas_token("cu1tAOSK9KEkdGsPkH5SlMC2vchz3jgD0795V9a66wQOEXK8RZXbMENyRXKrLvvu")

# Libraries ---------------------------------------------------------------
library(rcanvas)
library(googlesheets4)
library(dplyr)
library(tidyr)

# Set Variables -----------------------------------------------------------
set_canvas_domain("https://canvas.psysciacc.org")

# Gradebook ---------------------------------------------------------------
gradebook <- get_course_gradebook(course_id = 6, progress = TRUE)
gradebook <- gradebook %>% 
  select(user.name, user_id, sis_user_id, user.login_id) %>% 
  unique() %>% 
  dplyr::rename(canvas_id = user_id, 
                PSA_ID = sis_user_id)

# gradebook2 <- gradebook %>% 
#   filter(!is.na(assignment_name)) %>% 
#   select(user.name, user_id, sis_user_id, user.login_id, 
#          assignment_name, grades.final_score, score) %>% 
#   pivot_wider(data = .,
#               id_cols = c(user.name, user_id, sis_user_id, 
#                           user.login_id, grades.final_score), 
#               names_from = assignment_name,
#               values_from = score, 
#               values_fill = NA)


# Teams -------------------------------------------------------------------
teams <- get_course_user_groups(course_id = 6)
teams <- teams %>% 
  rename(canvas_id = id) %>% 
  select(-group_id) %>% 
  unique() %>% 
  filter(canvas_id != "3787") # test student

# eliminate duplicates
teams <- teams[!duplicated(teams$canvas_id), ]

# join teams with gradebook
gradebook_merge <- teams %>% 
  full_join(gradebook, by = c("canvas_id" = "canvas_id")) %>% 
  unique() %>% 
  arrange(group_name, PSA_ID)

# get assignment ids
temp <- get_assignment_list(course_id = 6)
temp %>% select(description, id)

# Collaborator Agreement --------------------------------------------------
# type id = 70
# assignment id = 112

# get all responses
list_responses <- get_submissions(course_id = 6,
                                  type = "quizzes",
                                  type_id = 70)

# ignore just open / close answers
list_responses <- list_responses$quiz_submissions %>% 
  filter(kept_score == 1) %>% 
  rename(canvas_id = user_id) %>% 
  select(canvas_id, kept_score) %>% 
  rename(Collab_Agreement = kept_score)

# merge with gradebook since only one question
gradebook_merge <- gradebook_merge %>% 
  full_join(
    list_responses, by = "canvas_id"
  ) 

# Authorship Survey (RR1) -------------------------------------------------
# type id = 91
# assignment id = 127

# get all responses
list_responses_keep <- get_submissions(course_id = 6,
                                  type = "quizzes",
                                  type_id = 91)

# ignore just open / close answers
list_responses <- list_responses_keep$quiz_submissions %>% 
  filter(kept_score == 1) %>% 
  rename(canvas_id = user_id) %>% 
  select(canvas_id, kept_score) %>% 
  rename(Authorship_Survey_RR1 = kept_score)

gradebook_merge <- gradebook_merge %>% 
  full_join(
    list_responses, by = "canvas_id"
  ) 

if (nrow(list_responses_keep) > 0) {
  authorship_survey <- list()
  ids <- unique(list_responses_keep$quiz_submissions$id)
  for (id in 1:length(ids)){
    
    authorship_survey[[id]] <- get_submission_single(course_id = 6,
                                              type_id = 91,
                                              user_id = list_responses_keep$quiz_submissions$id[id], 
                                              assignment_id = 127) 
    
    authorship_survey[[id]]$quiz_id <- ids[id]
    
  }
}

authorship_answers <- bind_rows(authorship_survey) %>% 
  select(submitted_at, Question_1:Question_7) 

old_aa <- read_sheet(authorship_answers, 
                     ss = "https://docs.google.com/spreadsheets/d/1LwZAtDUZ4dzGzRIBumnvS9_kskCkRHTYAbX4xet4ZU8/edit?usp=sharing",
                     sheet = "Authorship RR1")



write_sheet(authorship_answers, 
            ss = "https://docs.google.com/spreadsheets/d/1LwZAtDUZ4dzGzRIBumnvS9_kskCkRHTYAbX4xet4ZU8/edit?usp=sharing",
            sheet = "Authorship RR1")

# Ethics Documentation ----------------------------------------------------
# type id = 113
# assignment id = 127

# get all responses
list_responses_keep <- get_submissions(course_id = 6,
                                       type = "assignments",
                                       type_id = 113)

# ignore just open / close answers
list_responses <- list_responses_keep %>% 
  filter(grade == "complete") %>% 
  rename(canvas_id = user_id) %>% 
  select(canvas_id, grade, url) %>% 
  rename(Ethics_Submission = grade)

gradebook_merge <- gradebook_merge %>% 
  full_join(
    list_responses, by = "canvas_id"
  ) 

# DC survey ---------------------------------------------------------------
# data collection survey
# quiz = 78
# assignment id = 111
# user id depends on user 

# get all responses
list_responses_keep <- get_submissions(course_id = 6,
                                       type = "quizzes",
                                       type_id = 78)

# ignore just open / close answers
list_responses <- list_responses_keep$quiz_submissions %>% 
  filter(kept_score == 1) %>% 
  rename(canvas_id = user_id) %>% 
  select(canvas_id, kept_score) %>% 
  rename(Data_Collection_Survey = kept_score)

gradebook_merge <- gradebook_merge %>% 
  full_join(
    list_responses, by = "canvas_id"
  ) 

if (nrow(list_responses_keep) > 0) {
  dc_survey <- list()
  ids <- unique(list_responses_keep$quiz_submissions$id)
  for (id in 1:length(ids)){
    
    dc_survey[[id]] <- get_submission_single(course_id = 6,
                                                     type_id = 91,
                                                     user_id = list_responses_keep$quiz_submissions$id[id], 
                                                     assignment_id = 127) 
    
    dc_survey[[id]]$quiz_id <- ids[id]
    
  }
}

dc_answers <- bind_rows(dc_survey) %>% 
  select(submitted_at, Question_1:Question_7) 

write_sheet(dc_answers, 
            ss = "https://docs.google.com/spreadsheets/d/1LwZAtDUZ4dzGzRIBumnvS9_kskCkRHTYAbX4xet4ZU8/edit?usp=sharing",
            sheet = "Data_Collection_Answers")

# Final Gradebook Write ---------------------------------------------------
write_sheet(gradebook_merge, 
            ss = "https://docs.google.com/spreadsheets/d/1LwZAtDUZ4dzGzRIBumnvS9_kskCkRHTYAbX4xet4ZU8/edit?usp=sharing",
            sheet = "Gradebook")
