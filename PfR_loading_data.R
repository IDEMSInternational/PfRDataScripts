#devtools::install_github("IDEMSInternational/postgresr")
#devtools::install_github("IDEMSInternational/plhR")

#importing the PfR xlsx
data_l <- import_list("PfR_Shiny.xlsx")

# Download data ----------------------------------------------------------------
#  download PfR app data from Metabase as an RDS file?
#   plhdata_org <- postgresr::get_user_data(site = plh_con, filter = FALSE)

plhdata_org <- postgresr::get_user_data(site = plh_con, filter = FALSE) 

#plhdata_org <- get_user_data (filter_variable = "app_deployment_name",
                              #filter_variable_value = "pfr",
                              #site = plh_con, merge_check = FALSE, filter = TRUE)
#names(plhdata_org) <- gsub(x = names(plhdata_org), pattern = "\\-", replacement = ".")  
View(plhdata_org)


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
View(session_completion)

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


