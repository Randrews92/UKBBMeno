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

# percentage of menopausal women on psychotic drugs.
# Count the number of non-NA responses in the 'Treatment/medications' column
medication_count <- sum(!is.na(filtered_df$'Treatment/medication code | Instance 0'))

# Count the number of NA responses in the 'Treatment/medications' column
non_medication_count <- sum(is.na(filtered_df$'Treatment/medication code | Instance 0'))

# Calculate the total number of participants
total_participants <- nrow(filtered_df)

# Calculate the percentage of women on medications
percentage_on_medication <- (medication_count / total_participants) * 100

# Calculate the percentage of women not on medications
percentage_not_on_medication <- (non_medication_count / total_participants) * 100

# Print the percentages
cat("Percentage of women on medications:", percentage_on_medication, "%\n")
cat("Percentage of women not on medications:", percentage_not_on_medication, "%\n")

#Now doing comparison groups
subset_data2 <- Lifestyle_table_new %>%
  select(`Participant ID`,
         `menopauseAge`,
         `Year of birth`,
         `Had menopause`,
         'Date of attending assessment centre | Instance 0',
         'Sex')

subset_data2 <- subset_data2 %>%
  filter(str_detect(Sex, "(?i)female"))

Med_table_participant  <- Med_table_participant  %>%
  select(-"Date of attending assessment centre | Instance 0")
B = left_join(subset_data2, Med_table_participant, by = "Participant ID")

Nomeno_subset <- B %>% filter(is.na(`Had menopause`))

#range of years in filtered_df table
years <- format(filtered_df$Date.of.attending.assessment.centre...Instance.0, "%Y")

year_range <- range(as.numeric(years))

cat("The range of years:", year_range[1], "-", year_range[2])

#filter that range onto no meno cohort (2006-2010)
Nomeno_subset <- Nomeno_subset %>%
  filter(year(`Date of attending assessment centre | Instance 0`) >= 2006 & year(`Date of attending assessment centre | Instance 0`) <= 2010)

#now do the percentages for non meno group
nomenomedication_count <- sum(!is.na(Nomeno_subset$'Treatment/medication code | Instance 0'))

# Count the number of NA responses in the 'Treatment/medications' column
nomeno_non_medication_count <- sum(is.na(Nomeno_subset$'Treatment/medication code | Instance 0'))

# Calculate the total number of participants
total_nomenoparticipants <- nrow(Nomeno_subset)

# Calculate the percentage of women on medications
percentage_on_nomenomedication <- (nomenomedication_count / total_nomenoparticipants) * 100

# Calculate the percentage of women not on medications
percentage_not_on_nomenomedication <- (nomeno_non_medication_count / total_nomenoparticipants) * 100

# Print the percentages
cat("Percentage of women on medications:", percentage_on_nomenomedication, "%\n")
cat("Percentage of women not on medications:", percentage_not_on_nomenomedication, "%\n")

#Now do men
male_data <- Lifestyle_table_new %>%
  select(`Participant ID`,
         `menopauseAge`,
         `Year of birth`,
         `Had menopause`,
         'Date of attending assessment centre | Instance 0',
         'Sex')

male_data <- male_data%>% filter(grepl("Male", Sex))

male_data  <- male_data  %>%
  select(-"Had menopause",
         -"menopauseAge")
C = left_join(male_data, Med_table_participant, by = "Participant ID")

#filter date range into cohort (2006-2010)
C <- C %>%
  filter(year(`Date of attending assessment centre | Instance 0`) >= 2006 & year(`Date of attending assessment centre | Instance 0`) <= 2010)

#now do the percentages for non meno group
Men_medication_count <- sum(!is.na(C$'Treatment/medication code | Instance 0'))

# Count the number of NA responses in the 'Treatment/medications' column
Men_non_medication_count <- sum(is.na(C$'Treatment/medication code | Instance 0'))

# Calculate the total number of participants
total_Menparticipants <- nrow(C)

# Calculate the percentage of women on medications
percentage_on_Menmedication <- (Men_medication_count / total_Menparticipants) * 100

# Calculate the percentage of women not on medications
percentageMen_not_on_medication <- (Men_non_medication_count / total_Menparticipants) * 100

# Print the percentages
cat("Percentage of men on medications:", percentageMen_on_medication, "%\n")
cat("Percentage of men not on medications:", percentageMen_not_on_nomenomedication, "%\n")

#Hysterectomy
hysterectomy_data <- Lifestyle_table_new %>%
  select(`Participant ID`,
         `menopauseAge`,
         `Year of birth`,
         `Had menopause`,
         'Date of attending assessment centre | Instance 0',
         'Sex')

D = left_join(hysterectomy_data, Med_table_participant, by = "Participant ID")

hysterectomy_data <- D %>% filter(grepl("Not sure - had a hysterectomy", `Had menopause`))

#filter date range into cohort (2006-2010)
hysterectomy_data2 <- hysterectomy_data %>%
  filter(year(`Date of attending assessment centre | Instance 0`) >= 2006 & year(`Date of attending assessment centre | Instance 0`) <= 2010)

#now do the percentages for non meno group
hyster_medication_count <- sum(!is.na(hysterectomy_data$'Treatment/medication code | Instance 0'))

# Count the number of NA responses in the 'Treatment/medications' column
hyster_non_medication_count <- sum(is.na(hysterectomy_data2$'Treatment/medication code | Instance 0'))

# Calculate the total number of participants
total_hysterparticipants <- nrow(hysterectomy_data2)

# Calculate the percentage of women on medications
percentage_on_hystermedication <- (hyster_medication_count / total_hysterparticipants) * 100

# Calculate the percentage of women not on medications
percentage_not_on_hystermedication <- (hyster_non_medication_count / total_hysterparticipants) * 100


#graph
library(ggplot2)

# Create a data frame with the percentages
data <- data.frame(
  Category = c("Menopause: On Psychotropic Medications", "Menopause: Not on Psychotropic Medications", 
               "No Menopause: On Psychotropic Medications", "No Menopause: Not on Psychotropic Medications",
               "Hysterectomy: On Psychotropic Medications", "Hysterectomy: Not on Psychotropic Medications",
               "Men: On Psychotropic Medications", "Men: Not on Psychotropic Medications"),
  Percentage = c(percentage_on_medication, percentage_not_on_medication,
                 percentage_on_nomenomedication, percentage_not_on_nomenomedication,
                 percentage_on_hystermedication, percentage_not_on_hystermedication,
                 percentage_on_Menmedication, percentageMen_not_on_medication)
)

# Plot
ggplot(data, aes(x = Category, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_blank())  # Remove x-axis labels
  labs(title = "Percentages of Psychotropic Medication users per menopausal status", 
       x = "Menopausal Category",
       y = "Percentage") +
  theme_minimal() +
  theme(legend.position = "bottom")  # Position legend at the bottom

  ggsave("LOE_Death.png", LOE_Death, width = 6, height = 4, dpi = 300)