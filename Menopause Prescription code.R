#Menopause and prescription data
T <- read.csv("Lifestyle_table_new.csv")

subset_data <- T %>%
  select(`Participant.ID`,
         `menopauseAge`,
         `Year.of.birth`,
         `Had.menopause`,
         'Date.of.attending.assessment.centre...Instance.0')

subset_data <- rename(subset_data, "Participant ID" = Participant.ID)
A = left_join(subset_data, Med_table_participant, by = "Participant ID")

filter_subset <- A%>% filter(grepl("Yes", Had.menopause))
filter_subset  <- filter_subset  %>%
  select(-"Date of attending assessment centre | Instance 0")

# Remove rows with missing values
filter_subset <- filter_subset[complete.cases(filter_subset$Year.of.birth, filter_subset$menopauseAge), ]

# Calculate MenopauseYear
filter_subset$MenopauseYear <- filter_subset$Year.of.birth + filter_subset$menopauseAge

# Print the updated results
print(n_rows)
print(missing_values)

#Filter so only had menopause +/- 5 years from when medications recorded
keyword <- "assessment"
matching_columns <- grep(keyword, names(filter_subset), value = TRUE)
print(matching_columns)

# Convert the 'Date.of.attending.assessment.centre...Instance.0' column to Date type
filter_subset$'Date.of.attending.assessment.centre...Instance.0' <- as.Date(filter_subset$'Date.of.attending.assessment.centre...Instance.0')

# Filter the data
filtered_df <- filter_subset %>%
  mutate(Menopause_Year = as.integer(MenopauseYear)) %>%
  filter(
    Date.of.attending.assessment.centre...Instance.0 >= (as.Date(as.character(Menopause_Year), "%Y") - years(5)) &
      Date.of.attending.assessment.centre...Instance.0 <= (as.Date(as.character(Menopause_Year), "%Y") + years(5))
  )


