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

list_responses_keep <- list_responses_keep %>% 
  filter(quiz_submissions$score == 1)

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
                                              user_id = list_responses_keep$quiz_submissions$user_id[id], 
                                              assignment_id = 127) 
    
    authorship_survey[[id]]$quiz_id <- ids[id]
    
  }
}

# get question 7 answers
for (id in 1:length(ids)){
  authorship_survey[[id]]$Question7.1 <-
    authorship_survey[[id]]$submission_history[[1]]$submission_data[[1]]$answer_3610[7]
  authorship_survey[[id]]$Question7.2 <-
    authorship_survey[[id]]$submission_history[[1]]$submission_data[[1]]$answer_1971[7]
  authorship_survey[[id]]$Question7.3 <-
    authorship_survey[[id]]$submission_history[[1]]$submission_data[[1]]$answer_5708[7]
}

authorship_answers <- bind_rows(authorship_survey) %>% 
  select(submitted_at, user_id, Question_1:Question_6, Question7.1:Question7.3) %>% 
  rename(Full_Name = Question_1, 
          Reference_Name = Question_2,
          Affiliation = Question_3, 
          Country = Question_4, 
          Email = Question_5, 
          COI = Question_6, 
          ReadRR = Question7.1,
          ApproveRR = Question7.2,
          AgreeCA = Question7.3,
          canvas_id = user_id) %>% 
  mutate(Full_Name = gsub("<[^>]+>", "", Full_Name),
         Reference_Name = gsub("<[^>]+>", "", Reference_Name),
         Affiliation = gsub("<[^>]+>", "", Affiliation),
         Country = gsub("<[^>]+>", "", Country),
         Email = gsub("<[^>]+>", "", Email),
         COI = gsub("<[^>]+>", "", COI)
         ) %>% 
  left_join(
    gradebook_merge %>% select(canvas_id, PSA_ID), 
    by = "canvas_id"
  )

old_aa <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1LwZAtDUZ4dzGzRIBumnvS9_kskCkRHTYAbX4xet4ZU8/edit?usp=sharing",
                     sheet = "Authorship RR1")
old_aa <- old_aa %>% 
  select(-colnames(authorship_answers), PSA_ID)

authorship_answers2 <- authorship_answers %>% 
  full_join(
    old_aa, by = "PSA_ID"
  )

write_sheet(authorship_answers2, 
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

list_responses_keep <- list_responses_keep %>% 
  filter(quiz_submissions$score == 1)

if (nrow(list_responses_keep) > 0) {
  dc_survey <- list()
  ids <- unique(list_responses_keep$quiz_submissions$id)
  for (id in 85:length(ids)){
    
    dc_survey[[id]] <- get_submission_single(course_id = 6,
                                                     type_id = 78,
                                                     user_id = list_responses_keep$quiz_submissions$user_id[id], 
                                                     assignment_id = 111) 
    
    dc_survey[[id]]$quiz_id <- ids[id]
    
  }
}

# get selection answers
for (id in 1:length(ids)){
  
  dc_survey[[id]]$Question15.1 <- na.omit(dc_survey[[id]]$submission_history[[1]]$submission_data[[1]]$answer_6491)
  dc_survey[[id]]$Question15.2 <- na.omit(dc_survey[[id]]$submission_history[[1]]$submission_data[[1]]$answer_3324)
  dc_survey[[id]]$Question15.3 <- na.omit(dc_survey[[id]]$submission_history[[1]]$submission_data[[1]]$answer_2085)
  dc_survey[[id]]$Question15.4 <- na.omit(dc_survey[[id]]$submission_history[[1]]$submission_data[[1]]$answer_6774)
  dc_survey[[id]]$Question15.5 <- na.omit(dc_survey[[id]]$submission_history[[1]]$submission_data[[1]]$answer_8476)
}
  
dc_answers <- bind_rows(dc_survey) %>% 
  select(submitted_at, user_id, Question_1:Question_32, Question15.1:Question15.5) %>% 
  select(-Question_1, -Question_6, 
         -Question_12, -Question_13,
         -Question_19, -Question_22,
         -Question_25, -Question_30) %>% 
  rename(canvas_id = user_id,
         Data_Collection_Team = Question_2,
         Affiliation_Team = Question_3,
         Lead_DC = Question_4,
         Primary_Contact_DC = Question_5, 
         Translation_Help = Question_7,
         Translation_Lang = Question_8,
         Translation_Coor = Question_9,
         Translation_Email = Question_10, 
         Data_Doc = Question_11, 
         DC_Team = Question_14, 
         DC_Lang = Question_15,
         DC_Sample = Question_16,
         DC_Multiple = Question_17,
         DC_Country = Question_18,
         Student_Sample_One_Uni = Question15.1,
         Student_Sample_Multi_Uni = Question15.2,
         Local_Community = Question15.3,
         Local_Other = Question15.4,
         Other_Sources = Question15.5, 
         Describe_Sample = Question_20, 
         Collection_Plan = Question_21, 
         Compensation = Question_23, 
         Compensation_Describe = Question_24, 
         No_Pay = Question_26,
         Have_Funding = Question_27,
         How_Pay  = Question_28,
         Pay_Describe = Question_29,
         Ethics_Type = Question_31,
         Ethics_Describe = Question_32) %>% 
  mutate(
    Data_Collection_Team = factor(Data_Collection_Team, 
                                  levels = c("6374", "2374"),
                                  labels = c("Yes", "No")),
    Lead_DC = factor(Lead_DC, 
                     levels = c("2200", "1748", ""),
                     labels = c("Yes", "No", NA)),
    Translation_Help = factor(Translation_Help, 
                              levels = c("8579", "6219", ""),
                              labels = c("Yes", "No", NA)), 
    Translation_Coor = factor(Translation_Coor, 
                              levels = c("6102", "364", ""),
                              labels = c("Yes", "No", NA)),
    Data_Doc = factor(Data_Doc, 
                      levels = c("2310", "1132", ""),
                      labels = c("Yes","No", NA)),
    DC_Multiple = factor(DC_Multiple, 
                         levels = c("2034", "512", "235", ""),
                         labels = c("Just one", "Yes, Two", "Yes, Three+", NA)),
    Collection_Plan = factor(Collection_Plan,
                             levels = c("7655", "7344", "Lab", ""),
                             labels = c("Online", "Both", "Lab", NA)),
    Compensation = factor(Compensation, 
                          levels = c("5182", "3077", "5284", "8747", "7130", ""),
                          labels = c("Credit", "Money", "None", "Other", "Gift Card", NA)),
    Have_Funding = factor(Have_Funding, 
                          levels = c("3074", "664", "5526", ""),
                          labels = c("Yes", "No", "Not Applicable", NA)),
    How_Pay = factor(How_Pay, 
                     levels = c("8875", "211", "6671", "6479", "2399", ""),
                     labels = c("Gift Card", "Cash", "Online", "Not Applicable", "Other", NA)),
    Ethics_Type = factor(Ethics_Type, 
                         levels = c("2282", "6642", "4104", "8239", 
                                    "7775", "7976", ""),
                         labels = c("Local Ethics", "Rely Any", 
                                   "No Requirements", "Rely US", 
                                   "Other", "Previous Local", NA))
) %>% 
  left_join(
    gradebook_merge %>% select(PSA_ID, canvas_id, user.name, group_name),
    by = "canvas_id"
  )

old_dc <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1LwZAtDUZ4dzGzRIBumnvS9_kskCkRHTYAbX4xet4ZU8/edit?usp=sharing",
                     sheet = "Data Collection")
old_dc <- old_dc %>% 
  select(-colnames(dc_answers), PSA_ID)

dc_answers2 <- dc_answers %>% 
  full_join(
    old_dc, by = "PSA_ID"
  )

write_sheet(dc_answers2, 
            ss = "https://docs.google.com/spreadsheets/d/1LwZAtDUZ4dzGzRIBumnvS9_kskCkRHTYAbX4xet4ZU8/edit?usp=sharing",
            sheet = "Data Collection")

# Final Gradebook Write ---------------------------------------------------
old_grade <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1LwZAtDUZ4dzGzRIBumnvS9_kskCkRHTYAbX4xet4ZU8/edit?usp=sharing",
                     sheet = "Gradebook")
old_grade <- old_grade %>% 
  select(-colnames(gradebook_merge), PSA_ID)

gradebook_merge2 <- gradebook_merge %>% 
  full_join(
    old_grade, by = "PSA_ID"
  )

write_sheet(gradebook_merge2, 
            ss = "https://docs.google.com/spreadsheets/d/1LwZAtDUZ4dzGzRIBumnvS9_kskCkRHTYAbX4xet4ZU8/edit?usp=sharing",
            sheet = "Gradebook")