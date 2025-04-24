#devtools::install_github("IDEMSInternational/postgresr")
#devtools::install_github("IDEMSInternational/ExcelToShiny")

#importing the PfR xlsx
data_l <- import_list("PfR_Shiny.xlsx")

# Download data ----------------------------------------------------------------
#  download PfR app data from Metabase as an RDS file?
#   plhdata_org <- postgresr::get_user_data(site = plh_con, filter = FALSE)

plhdata_org <- openappr::get_user_data(site = plh_con, filter = FALSE) 
plhdata_org$device_info <- NULL

#plhdata_org <- get_user_data (filter_variable = "app_deployment_name",
#filter_variable_value = "pfr",
#site = plh_con, merge_check = FALSE, filter = TRUE)
#names(plhdata_org) <- gsub(x = names(plhdata_org), pattern = "\\-", replacement = ".")  
#View(plhdata_org)

# Remove all columns which are entirely NA
plhdata_org <- plhdata_org[, colSums(is.na(plhdata_org)) < nrow(plhdata_org)]

#cap date filter
mydate <- "2023-01-01"
plhdata_org <- plhdata_org %>% filter(as.Date(createdAt) > as.Date(mydate))

#app last launch
plhdata_org$`app_last_launch` <- plhdata_org$`rp-contact-field.app_last_launch`
plhdata_org $ app_last_launch_month <- as.yearmon(plhdata_org$app_last_launch)


plhdata_org$`app_last_sync` <- plhdata_org$`rp-contact-field._server_sync_latest`
plhdata_org $ app_last_sync_month <- as.yearmon(plhdata_org$app_last_sync)

plhdata_org$`app_launch_count` <- as.numeric(plhdata_org$`rp-contact-field.app_launch_count`)

#App last sync
plhdata_org <- plhdata_org %>%
  mutate(synced_7_days = ifelse(app_last_sync >= as.Date(lubridate::now(tzone = "UTC")) - 7,
                                1,0))

plhdata_org <- plhdata_org %>%
  mutate(synced_7_14_days = ifelse(app_last_sync >= as.Date(lubridate::now(tzone = "UTC")) - 14 &
                                     app_last_sync < as.Date(lubridate::now(tzone = "UTC")) - 7,
                                   1,0))

plhdata_org <- plhdata_org %>%
  mutate(synced_14_30_days = ifelse(app_last_sync >= as.Date(lubridate::now(tzone = "UTC")) - 30 &
                                      app_last_sync < as.Date(lubridate::now(tzone = "UTC")) - 14,
                                    1,0))

plhdata_org <- plhdata_org %>%
  mutate(synced_more_than_30_days = ifelse(app_last_sync < as.Date(lubridate::now(tzone = "UTC")) - 30,
                                           1,0))

plhdata_org$app_last_launch <- as.Date(plhdata_org$app_last_launch)


plhdata_org$`days_btwn_app_launches` <- as.numeric(plhdata_org$`rp-contact-field.max_days_between_app_launches`)

# Session Completion(true/false)
plhdata_org$`rp-contact-field.task_onboarding_completed` <- plhdata_org$'rp-contact-field.task_onboarding_completed'

# Session completion

# Create a Vector of Session IDs:
# The session_ids vector is initialized with a list of session names, 
#   which are strings representing different sessions or categories.

session_ids <- c("onboarding", "family_relation", "current_pract", "child_dev", 
                 "parent_childhood", "positive_parenting", "gender_power",
                 "impact_conflict", "sharing_care", "healthy_relation", "review",
                 "discipline", "education", "gender_equal", "prevent_abuse",
                 "reduce_conflict", "conclusion")

# Initialize an Empty List for Session Completion Data:
# An empty list session_completion is created to store the completion status for each session.

session_completion <- NULL
for (i in session_ids){    # A for loop iterates over each element (session ID) in the session_ids vector.
  var_name <- paste0("rp-contact-field.task_", i, "_completed") # the code constructs a variable name using the current session ID
  session_completion[[i]] <- plhdata_org[[var_name]] # extract the corresponding column from the plhdata_org data frame and assigns it to the session_completion list under the current session ID.
}

session_completion <- dplyr::bind_rows(session_completion) # The bind_rows function takes the list session_completion and combines all the individual data frames (or columns/vectors) within this list into one single data frame.
session_completion$uuid <- plhdata_org$app_user_id # Each row in the session_completion data frame has a corresponding uuid value, which is taken from the app_user_id column in the plhdata_org data frame. 
# view session completion with respective uuids
#View(session_completion)

# e.g. for onboarding:
session_completion %>% group_by(onboarding) %>% summarise(number_completed = n())


# or, you can rearrange and view them all at once:
session_completion_longer <- session_completion %>% pivot_longer(cols = !uuid, names_to = "session", values_to = "completed")

session_completion_longer_summary <- session_completion_longer %>% group_by(session, completed) %>% summarise(n())
session_completion_longer_summary

# across all
ggplot(session_completion_longer_summary, aes(x = completed, y = `n()`)) +
  geom_bar(stat = "identity")

# or by each one
ggplot(session_completion_longer_summary, aes(x = completed, y = `n()`)) +
  geom_bar(stat = "identity") +
  facet_wrap(vars(session))

# session completion frequency table summary
library(dplyr)
library(tidyr)
library(knitr)

# Create a mapping of session IDs to full names
session_names <- data.frame(
  session = c("onboarding", 
              "family_relation", 
              "current_pract", 
              "child_dev", 
              "parent_childhood", 
              "positive_parenting",
              "discipline_children",
              "gender_power",
              "impact_conflict",
              "sharing_care", 
              "healthy_relation",
              "parenting_teens", 
              "education", 
              "gender_equal", 
              "prevent_abuse",
              "reduce_conflict", 
              "conclusion"),
  full_name = c("Onboarding", 
                "1. Family relationships", 
                "2. Current beliefs and practices regarding parent-child interaction", 
                "3. Child development needs and behaviours", 
                "4. Influence of one’s own parents and positive discipline", 
                "5. Positive and sensitive parenting", 
                "6. How do we discipline our children",
                "7. Gendered relationships and Power",
                "8. Impact of parental conflict", 
                "9. Sharing child care responsibility",
                "10. Healthy marital relationships and communication", 
                "11. Parenting adolescents", 
                "12. Encouraging education", 
                "13. Greater gender equality", 
                "14. Prevention of sexual abuse for children below 17 years", 
                "15. Reducing parental conflict and improving communication", 
                "Conclusion")
)

# Summarize session completion counts
session_completion_summary <- session_completion_longer %>%
  group_by(session, completed) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = completed, values_from = count, values_fill = 0) %>%
  left_join(session_names, by = "session") %>%  # Merge full names
  select(full_name, everything())  # Reorder columns

# Rename columns for clarity
colnames(session_completion_summary) <- c("Full Session Name", "Session ID", "False", "True", "NA")

# Print the final summary table
print(session_completion_summary)


# Frequent sessions per user
# Summarize the session completion counts per user
frequent_sessions_per_user <- session_completion_longer %>%
  group_by(uuid) %>%
  summarise(`Number of sessions completed` = sum(completed == "true", na.rm = TRUE), .groups = 'drop')

frequent_sessions_per_user %>%
  group_by(`Number of sessions completed`) %>%
  summarise(`Number of Users` = n())

# Counting the number of users who are on Week X
# 
session_ids <- c("onboarding", "family_relation", "current_pract", "child_dev", "parent_childhood",
                 "positive_parenting", "gender_power", "impact_conflict", "sharing_care", "healthy_relation",
                 "discipline", "education", "gender_equal", "prevent_abuse", "reduce_conflict", "conclusion")
session_ids <- paste0("rp-contact-field.task_", session_ids, "_completed")
session_completion_data <- plhdata_org %>%
  dplyr::select(c(app_user_id, all_of(session_ids))) %>%
  dplyr::mutate(across(session_ids, ~ifelse(is.na(.), 0, ifelse(. == "false", 0, ifelse(. == "true", 1, 9999))))) %>%
  pivot_longer(cols = !app_user_id) %>%
  group_by(app_user_id) %>%
  mutate(count_ones = {
    # Find the first position of y == 1
    first_one <- which(value == 1)[1]
    
    # Initialize the vector to store counts
    temp <- rep(0, length(value))
    
    # If there's a 1 in the group, start counting from the first 1
    if (!is.na(first_one)) {
      count <- 0
      for (i in first_one:length(value)) {
        if (value[i] == 1) {
          count <- count + 1
          temp[i] <- count
        } else if (value[i] == 0) {
          break  # Stop counting when the first 0 appears after the 1
        }
      }
    }
    
    # Return the updated temp vector as the new column
    temp
  }) %>%
  ungroup()

session_completion_data_week <- session_completion_data %>%
  group_by(app_user_id) %>%
  summarise(total_consecutive_sessions = max(count_ones)) %>%
  mutate(week = current_week) %>%
  mutate(engagement = ifelse(total_consecutive_sessions < week - 1, "Low",
                             ifelse(total_consecutive_sessions < week, "Medium",
                                    ifelse(total_consecutive_sessions == week, "High",
                                           ifelse(total_consecutive_sessions > week, "Above",
                                                  "Check")))))

#view(session_completion_data_week)

#%>%  # Summarize counts
#  filter(completion_count > 5) %>% #Define "frequently" as visited more than once
#  summarise(n()) # how many rows are there ? (how many people have cmpleted >5 sessions)

# Check if frequent_sessions_per_user has any data and view the results
if (nrow(frequent_sessions_per_user) == 0) {
  warning("No frequent sessions found for any user.")
} else {
  #View(frequent_sessions_per_user)  # View the result if data is available
}


# test Number of participants who have completed a homepractice
homepractice_completion_data <- plhdata_org %>%
  dplyr::select(c(app_user_id, ends_with("hp_completed"))) %>%
  pivot_longer(cols = !app_user_id) %>%
  group_by(name) %>%
  summarise(`Number completed` = sum(value == "true", na.rm = TRUE)) %>%
  mutate(name = sub("^[^_]*_", "", name)) %>%
  mutate(name = sub("_hp_completed", "", name)) %>%
  mutate(name = tools::toTitleCase(gsub("_", " ", name))) 


homepractice_completion_data <- plhdata_org %>%
  dplyr::select(c(app_user_id, ends_with("hp_completed"))) %>%
  pivot_longer(cols = !app_user_id) %>%
  group_by(name) %>%
  summarise(`Number completed` = sum(value == "true", na.rm = TRUE)) %>%
  mutate(name = sub("^[^_]*_", "", name)) %>%
  mutate(name = sub("_hp_completed", "", name)) %>%
  mutate(name = tools::toTitleCase(gsub("_", " ", name))) 
# Plot the bar chart
# ggplot(homepractice_completion_data) +
#   geom_bar(aes(x = name, y = `Number completed`, fill = name), stat = "identity") +
#   labs(
#     title = "Number of Completed Items",
#     x = "Name",
#     y = "Number Completed"
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   guides(fill = "none")

plhdata_org_download <- plhdata_org
plhdata_org_download$contact_fields <- NULL
# to add into download tab: frequent_sessions_per_user, plhdata_org_download, session_completion_data_week, 
#session_completion_longer

# Completion of homepractice as yes/no/na values table
hp_done_data <- plhdata_org %>%
  dplyr::select(c(app_user_id, contains("hp_done_"))) %>%
  pivot_longer(cols = !app_user_id) %>%
  group_by(name) %>%
  summarise(`Number completed` = sum(value == "yes", na.rm = TRUE)) %>%
  mutate(name = tools::toTitleCase(gsub("_", " ", name)))

#hp_done_data_1 <- plhdata_org %>%
  #dplyr::select(c(app_user_id, contains("hp_done_"))) %>%
  #pivot_longer(cols = -app_user_id, names_to = "name", values_to = "value") %>%
  #group_by(name, value) %>%
  #summarise(`Count` = n(), .groups = "drop") %>%
  #pivot_wider(names_from = value, values_from = `Count`, values_fill = list(`Count` = 0)) %>%
  #rename(`Yes Count` = yes, `No Count` = no) %>%
  #hp_done_data_1 <- hp_done_data_1 %>%
  #mutate(name = forcats::fct_recode,
         #`1. Family relationships` = "rp-contact-field.hp_done_family_relation",
         #`2. Current beliefs and practices` = "rp-contact-field.hp_done_current_pract", 
         #`3. Child development needs and behaviours` = "rp-contact-field.hp_done_child_dev",
         #`4. Influence of one’s own parents` = "rp-contact-field.hp_done_parent_childhood",
         #`6. How do we discipline our children` = "rp-contact-field.hp_done_discipline_children",
         #`8. Impact of parental conflict` = "rp-contact-field.hp_done_impact_conflict",
         #`9. Sharing child care responsibility` = "rp-contact-field.hp_done_sharing_care",
         #`10. Healthy marital relationships` = "rp-contact-field.hp_done_healthy_relation",
         #`12. Encouraging education` = "rp-contact-field.hp_done_education",
         #`13. Greater gender equality` = "rp-contact-field.hp_done_gender_equal",
         #`14. Prevention of sexual abuse` = "rp-contact-field.hp_done_prevent_abuse",
         #`15. Reducing parental conflict` = "rp-contact-field.hp_done_reduce_conflict")

hp_done_data_1 <- plhdata_org %>%
  dplyr::select(c(app_user_id, contains("hp_done_"))) %>%
  pivot_longer(cols = -app_user_id, names_to = "name", values_to = "value") %>%
  group_by(name, value) %>%
  summarise(`Count` = n(), .groups = "drop") %>%
  pivot_wider(names_from = value, values_from = `Count`, values_fill = list(`Count` = 0)) %>%
  rename(`Yes Count` = yes, `No Count` = no) %>%
  mutate(name = as.factor(name)) %>%  # Convert to factor before using fct_recode
  mutate(name = forcats::fct_recode(name,
                                    `1. Family relationships hp_done_` = "rp-contact-field.hp_done_family_relation",
                                    `2. Current beliefs and practices hp_done_` = "rp-contact-field.hp_done_current_pract", 
                                    `3. Child development needs and behaviours hp_done_` = "rp-contact-field.hp_done_child_dev",
                                    `4. Influence of one’s own parents hp_done_` = "rp-contact-field.hp_done_parent_childhood",
                                    `6. How do we discipline our children hp_done_` = "rp-contact-field.hp_done_discipline_children",
                                    `8. Impact of parental conflict hp_done_` = "rp-contact-field.hp_done_impact_conflict",
                                    `9. Sharing child care responsibilityhp_done_` = "rp-contact-field.hp_done_sharing_care",
                                    `10. Healthy marital relationships hp_done_` = "rp-contact-field.hp_done_healthy_relation",
                                    `12. Encouraging education hp_done_` = "rp-contact-field.hp_done_education",
                                    `13. Greater gender equality hp_done_` = "rp-contact-field.hp_done_gender_equal",
                                    `14. Prevention of sexual abuse hp_done_` = "rp-contact-field.hp_done_prevent_abuse",
                                    `15. Reducing parental conflict hp_done_` = "rp-contact-field.hp_done_reduce_conflict"
  ))

    
# Responses to quizes in sessions
quiz_done_data <- plhdata_org %>%
  dplyr::select(c(app_user_id, contains("quiz_question"))) %>%
  pivot_longer(cols = -app_user_id, names_to = "name", values_to = "response") %>%
  group_by(name, response) %>%
  summarise(`Count` = n(), .groups = "drop") %>%
  mutate(name = stringr::str_remove(name, "rp-contact-field.quiz_question_"))
#pivot_wider(names_from = response, values_from = `Count`, values_fill = list(`Count` = 0)) %>%
#mutate(name = tools::toTitleCase(gsub("_", " ", name)))


library(readxl)

Quiz_responses <- read_excel("PfR_in_app_data_collected_via_App.xlsx", 
                             sheet = "quiz_questions_codebook")

Quiz_responses <- pivot_longer(Quiz_responses , cols = starts_with('option_'), names_to = "response")

Quiz_responses <- Quiz_responses %>% dplyr::select(c(name = id, topic_session, question_text, response, value)) %>%
  mutate(response = str_remove(response, "_text"))

quiz_done_data <- left_join(quiz_done_data, Quiz_responses) %>%
  dplyr::select(c(name, response, topic_session, question_text, value, Count))

