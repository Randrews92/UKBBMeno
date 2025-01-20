
##### No need to run this script, just gives info on the processes used to derive key vars. 

install.packages("gtsummary")
install.packages("broom.mixed")
install.packages("tidyverse")
library(gtsummary)
library(broom.mixed)
library(data.table)
library(tidyverse)
# Loading libraries
install.packages('car')
library(car) # For Anova and repeated measures functions
# Load necessary libraries
install.packages('afex')
install.packages('emmeans')
library(afex) # for mixed ANOVA
library(emmeans) # for post-hoc and extracting SD_within
# Load necessary libraries
install.packages("mediation")
library(mediation)



########### Creating cognitive testing columns ####################################

#### This script gives the processes used for deriving whether women who underwent repeated cognitive tests had signifcant decline. 

# use dx download Women_cognitive_tests_participant.csv

cog_tests <- read.csv("Women_cognitive_tests_participant.csv")

cog_tests[cog_tests == ""] <- NA

na_counts <- data.frame(Column = names(cog_tests), 
                        NA_Count = colSums(is.na(cog_tests)))
na_counts

# Recode and merge pilot pairs matching

# If it's NA, use 'Body.mass.index..BMI....Instance.0.participant...p23104_i0.'
cog_tests <- cog_tests %>%
  mutate(
    merged_Number.of.incorrect.matches.in.round...Instance.0...Array.1 = coalesce(
      Number.of.incorrect.matches.in.round...Instance.0...Array.1,
      Number.of.incorrect.matches.in.round..pilot....Instance.0...Array.1
    )
  )


cog_tests <- cog_tests %>%
  mutate(
    merged_Number.of.incorrect.matches.in.round...Instance.0...Array.2 = coalesce(
      Number.of.incorrect.matches.in.round...Instance.0...Array.2,
      Number.of.incorrect.matches.in.round..pilot....Instance.0...Array.2
    )
  )


cog_tests <- cog_tests %>%
  mutate(
    merged_Number.of.incorrect.matches.in.round...Instance.0...Array.3 = coalesce(
      Number.of.incorrect.matches.in.round...Instance.0...Array.3,
      Number.of.incorrect.matches.in.round..pilot....Instance.0...Array.3
    )
  )


na_counts <- data.frame(Column = names(cog_tests), 
                        NA_Count = colSums(is.na(cog_tests)))
na_counts

# Calculate the instance 0 matched pairs score by summing merged array 1,2,3
cog_tests <- cog_tests%>%
  rowwise() %>%
  mutate(Pairs_Score_Instance.0 = sum(merged_Number.of.incorrect.matches.in.round...Instance.0...Array.1,
                                      merged_Number.of.incorrect.matches.in.round...Instance.0...Array.2,
                                      merged_Number.of.incorrect.matches.in.round...Instance.0...Array.3 , na.rm = TRUE))

# Calculate the instance 1 matched pairs score by summing merged array 1,2,3
cog_tests <- cog_tests%>%
  rowwise() %>%
  mutate(Pairs_Score_Instance.1 = sum(Number.of.incorrect.matches.in.round...Instance.1...Array.1,
                                      Number.of.incorrect.matches.in.round...Instance.1...Array.2,
                                      Number.of.incorrect.matches.in.round...Instance.1...Array.3 , na.rm = TRUE))


# Calculate the instance 2 matched pairs score by summing merged array 1,2,3
cog_tests <- cog_tests%>%
  rowwise() %>%
  mutate(Pairs_Score_Instance.2 = sum(Number.of.incorrect.matches.in.round...Instance.2...Array.1,
                                      Number.of.incorrect.matches.in.round...Instance.2...Array.2,
                                      Number.of.incorrect.matches.in.round...Instance.2...Array.3 , na.rm = TRUE))


# Calculate the instance 3 matched pairs score by summing merged array 1,2,3
cog_tests <- cog_tests%>%
  rowwise() %>%
  mutate(Pairs_Score_Instance.3 = sum(Number.of.incorrect.matches.in.round...Instance.3...Array.1,
                                      Number.of.incorrect.matches.in.round...Instance.3...Array.2,
                                      Number.of.incorrect.matches.in.round...Instance.3...Array.3 , na.rm = TRUE))


cog_tests <- cog_tests %>%
  mutate(Pairs_Score_Instance.0 = ifelse(is.na(merged_Number.of.incorrect.matches.in.round...Instance.0...Array.1), NA, Pairs_Score_Instance.0))

cog_tests <- cog_tests %>%
  mutate(Pairs_Score_Instance.1 = ifelse(is.na(Number.of.incorrect.matches.in.round...Instance.1...Array.1), NA, Pairs_Score_Instance.1))


cog_tests <- cog_tests %>%
  mutate(Pairs_Score_Instance.2 = ifelse(is.na(Number.of.incorrect.matches.in.round...Instance.2...Array.1), NA, Pairs_Score_Instance.2))

cog_tests <- cog_tests %>%
  mutate(Pairs_Score_Instance.3 = ifelse(is.na(Number.of.incorrect.matches.in.round...Instance.3...Array.1), NA, Pairs_Score_Instance.3))


# Now we can subset our data to add to the regression data

cog_tests_reg <- cog_tests %>%
  select(Participant.ID,
         Mean.time.to.correctly.identify.matches...Instance.0,
         Mean.time.to.correctly.identify.matches...Instance.1,
         Mean.time.to.correctly.identify.matches...Instance.2,
         Mean.time.to.correctly.identify.matches...Instance.3,
         Pairs_Score_Instance.0,
         Pairs_Score_Instance.1,
         Pairs_Score_Instance.2,
         Pairs_Score_Instance.3)


write.csv(cog_tests_reg,'cog_tests_reg.csv')



#######################Deriving meno status and transitions#####################################

##### This script describes prepping the menopause cohort to identify new cases from baseline. 

meno <- read.csv("Women_menopause_participant.csv")

cat(colnames(meno), sep = "\n")

## Perimenopause

# Individuals in this study (https://www.frontiersin.org/journals/dementia/articles/10.3389/frdem.2023.1098693/full) were placed into the following groups: 
# (1) “premenopausal” (women who reported being premenopausal at all timepoints), 
# (2) “perimenopausal” (women who reported being premenopausal at baseline, and postmenopause in subsequent assessments)
# (3) “postmenopausal” (women who reported being postmenopausal at all timepoints).

# We will also use HRT use as a criterion for perimenopause. 
# e.g. used HRT in instance 0,1,2,3 but said No to menopause instance 0, 1, 2, or 3 and didn't have a surgical menopause

# Step 1: Identify women who were in premenopause, menopause, surgical menopause, perimenopause during instance 0

unique(meno$Had.menopause...Instance.0)

# Create menopausal status columns for each instance
meno[meno == ""] <- NA


# Remove rows with 'Prefer not to answer'
meno <- meno %>%
  filter(Had.menopause...Instance.0 != "Prefer not to answer")


meno <- meno %>%
  mutate(
    # Instance 0 logic: Assign initial menopausal status
    Menopausal_Status_Instance_0 = case_when(
      Bilateral.oophorectomy..both.ovaries.removed....Instance.0 == "Yes" ~ "Surgical menopause",  # Surgical menopause first
      Had.menopause...Instance.0 == "Yes" ~ "Postmenopausal",  # Then postmenopausal
      (Had.menopause...Instance.0 == "No" | Had.menopause...Instance.0 %in% c("Not sure - other reason", "Not sure - had a hysterectomy")) & 
        ("Yes" %in% c(Had.menopause...Instance.1, Had.menopause...Instance.2, Had.menopause...Instance.3)) & 
        !("Yes" %in% c(Bilateral.oophorectomy..both.ovaries.removed....Instance.1, Bilateral.oophorectomy..both.ovaries.removed....Instance.2, Bilateral.oophorectomy..both.ovaries.removed....Instance.3)) ~ "Perimenopausal",  # Perimenopausal last in order
      (Had.menopause...Instance.0 == "No" | Had.menopause...Instance.0 %in% c("Not sure - other reason", "Not sure - had a hysterectomy"))  
      & Ever.used.hormone.replacement.therapy..HRT....Instance.0 == "Yes" ~ "Perimenopausal",  # Also transition to Perimenopausal if on HRT
      Had.menopause...Instance.0 == "No" & ("No" %in% c(Had.menopause...Instance.1, Had.menopause...Instance.2, Had.menopause...Instance.3)) ~ "Premenopausal",  # Otherwise premenopausal
      Had.menopause...Instance.0 %in% c("Not sure - other reason", "Not sure - had a hysterectomy") ~ "Not Sure",
      TRUE ~ NA_character_  # Handle missing cases
    ),
    
    # Instance 1 logic: Transition based on previous status
    Menopausal_Status_Instance_1 = case_when(
      Menopausal_Status_Instance_0 == "Perimenopausal" & Had.menopause...Instance.1 == "Yes" ~ "Postmenopausal",  # Transition to postmenopausal
      Menopausal_Status_Instance_0 == "Perimenopausal" & Bilateral.oophorectomy..both.ovaries.removed....Instance.1 == "Yes" ~ "Surgical menopause",  # Transition to surgical menopause
      Menopausal_Status_Instance_0 == "Perimenopausal" ~ "Perimenopausal",  # Keep perimenopausal
      Menopausal_Status_Instance_0 == "Postmenopausal" ~ "Postmenopausal",  # Stay postmenopausal
      Menopausal_Status_Instance_0 == "Surgical menopause" ~ "Surgical menopause",  # Stay surgical menopause
      Bilateral.oophorectomy..both.ovaries.removed....Instance.1 == "Yes" ~ "Surgical menopause",  # If oophorectomy, transition to surgical menopause
      Had.menopause...Instance.1 == "Yes" ~ "Postmenopausal",  # If menopause is Yes, transition to postmenopausal
      Had.menopause...Instance.1 == "No" & ("No" %in% c(Had.menopause...Instance.2, Had.menopause...Instance.3)) ~ "Premenopausal",  # Stay premenopausal if no change
      Had.menopause...Instance.1 == "No" & Ever.used.hormone.replacement.therapy..HRT....Instance.1 == "Yes" ~ "Perimenopausal",  # Transition to perimenopausal if HRT
      TRUE ~ NA_character_
    ),
    
    # Instance 2 logic: Transition based on previous status
    Menopausal_Status_Instance_2 = case_when(
      Menopausal_Status_Instance_1 == "Perimenopausal" & Had.menopause...Instance.2 == "Yes" ~ "Postmenopausal",  # Transition to postmenopausal
      Menopausal_Status_Instance_1 == "Perimenopausal" & Bilateral.oophorectomy..both.ovaries.removed....Instance.2 == "Yes" ~ "Surgical menopause",  # Transition to surgical menopause
      Menopausal_Status_Instance_1 == "Perimenopausal" ~ "Perimenopausal",  # Keep perimenopausal
      Menopausal_Status_Instance_1 == "Postmenopausal" ~ "Postmenopausal",  # Stay postmenopausal
      Menopausal_Status_Instance_1 == "Surgical menopause" ~ "Surgical menopause",  # Stay surgical menopause
      Bilateral.oophorectomy..both.ovaries.removed....Instance.2 == "Yes" ~ "Surgical menopause",  # If oophorectomy, transition to surgical menopause
      Had.menopause...Instance.2 == "Yes" ~ "Postmenopausal",  # If menopause is Yes, transition to postmenopausal
      Had.menopause...Instance.2 == "No" & ("No" %in% c(Had.menopause...Instance.3)) ~ "Premenopausal",  # Stay premenopausal if no change
      Had.menopause...Instance.2 == "No" & Ever.used.hormone.replacement.therapy..HRT....Instance.2 == "Yes" ~ "Perimenopausal",  # Transition to perimenopausal if HRT
      TRUE ~ NA_character_
    ),
    
    # Instance 3 logic: Final check for transitions
    Menopausal_Status_Instance_3 = case_when(
      Menopausal_Status_Instance_2 == "Perimenopausal" & Had.menopause...Instance.3 == "Yes" ~ "Postmenopausal",  # Transition to postmenopausal
      Menopausal_Status_Instance_2 == "Perimenopausal" & Bilateral.oophorectomy..both.ovaries.removed....Instance.3 == "Yes" ~ "Surgical menopause",  # Transition to surgical menopause
      Menopausal_Status_Instance_2 == "Perimenopausal" ~ "Perimenopausal",  # Keep perimenopausal
      Menopausal_Status_Instance_2 == "Postmenopausal" ~ "Postmenopausal",  # Stay postmenopausal
      Menopausal_Status_Instance_2 == "Surgical menopause" ~ "Surgical menopause",  # Stay surgical menopause
      Bilateral.oophorectomy..both.ovaries.removed....Instance.3 == "Yes" ~ "Surgical menopause",  # If oophorectomy, transition to surgical menopause
      Had.menopause...Instance.3 == "Yes" ~ "Postmenopausal",  # If menopause is Yes, transition to postmenopausal
      Had.menopause...Instance.3 == "No" ~ "Premenopausal",  # Stay premenopausal if no change
      Had.menopause...Instance.3 == "No" & Ever.used.hormone.replacement.therapy..HRT....Instance.3 == "Yes" ~ "Perimenopausal",  # Transition to perimenopausal if HRT
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup()


# Count how many we have per meno status at baseline 

meno%>%
  group_by(Menopausal_Status_Instance_0)%>%
  summarise(count=n_distinct(Participant.ID))

# Figure out Not Sure coded ones 
subset_notsure <- meno%>%filter(grepl('Not Sure', Menopausal_Status_Instance_0))
subset_notsure%>%
  group_by(Had.menopause...Instance.2)%>%
  summarise(count=n_distinct(Participant.ID))

# Figure out Perimenopause coded ones 
subset <- meno%>%filter(grepl('Perimenopausal', Menopausal_Status_Instance_0))
subset%>%
  group_by(Menopausal_Status_Instance_2)%>%
  summarise(count=n_distinct(Participant.ID))

# Assuming columns Had.menopause...Instance.0 through Had.menopause...Instance.3
meno$Max_Instance <- apply(meno[, c("Had.menopause...Instance.0", 
                                    "Had.menopause...Instance.1", 
                                    "Had.menopause...Instance.2", 
                                    "Had.menopause...Instance.3")], 
                           1, function(row) {
                             # Find the maximum instance with a non-NA value
                             max(which(!is.na(row)), na.rm = TRUE) - 1
                           })

# Replace -Inf with NA if all instances are NA
meno$Max_Instance[is.infinite(meno$Max_Instance)] <- NA

# Count how many we have per max instance 

meno%>%
  group_by(Max_Instance)%>%
  summarise(count=n_distinct(Participant.ID))


#Step 2: Work out who transitioned through menopause so we can have a number: 

# Assuming your menopausal status columns are named Menopausal.Status...Instance.0 to Menopausal.Status...Instance.3
meno$Transition_to_meno <- apply(meno[, c("Menopausal_Status_Instance_0", 
                                          "Menopausal_Status_Instance_1", 
                                          "Menopausal_Status_Instance_2", 
                                          "Menopausal_Status_Instance_3")], 
                                 1, function(statuses) {
                                   # Check if there is a transition from premenopausal/Not Sure/perimenopausal to postmenopausal/surgical
                                   for (i in seq_along(statuses)[-length(statuses)]) {
                                     if (statuses[i] %in% c("Premenopausal", "Not Sure", "Perimenopausal") &&
                                         any(statuses[(i+1):length(statuses)] %in% c("Postmenopausal", "Surgical menopause"))) {
                                       return("Y")
                                     }
                                   }
                                   return("N")
                                 })


meno%>%
  group_by(Transition_to_meno)%>%
  summarise(count=n_distinct(Participant.ID))

unique(meno$Menopausal_Status_Instance_0)

# Now create and save the subset 

cat(colnames(meno), sep = "\n")

meno_all_instances_reg <- meno %>%
  select(-Ever.had.hysterectomy..womb.removed....Instance.0,
         -Ever.had.hysterectomy..womb.removed....Instance.1,
         -Ever.had.hysterectomy..womb.removed....Instance.2,
         -Ever.had.hysterectomy..womb.removed....Instance.3,
         -Non.cancer.illness.code..self.reported...Instance.0,
         -Non.cancer.illness.code..self.reported...Instance.1,
         -Non.cancer.illness.code..self.reported...Instance.2,
         -Non.cancer.illness.code..self.reported...Instance.3,
         -Treatment.medication.code...Instance.0,
         -Treatment.medication.code...Instance.1,
         -Treatment.medication.code...Instance.2,
         -Treatment.medication.code...Instance.3,
         -Date.N95.first.reported..menopausal.and.other.perimenopausal.disorders.)


write_csv(meno_all_instances_reg, "meno_all_instances_reg.csv")



##### Prepping the tests to define cognitve signifcant decline based on this paper: https://pmc.ncbi.nlm.nih.gov/articles/PMC4844168/
# That paper used "smallest real difference" to identify significant cogntive decline across time. 

# dx download meno_all_instances_reg.csv
cat(colnames(cog_tests), sep = "\n")

cog_tests<- read.csv("cog_tests_reg.csv")

head(cog_tests)

cat(colnames(cog_tests), sep = "\n")

# Step 1: Standardize column names (remove duplicate Instance parts)
cog_tests <- cog_tests %>%
  rename_with(
    .cols = starts_with("Mean.time"),
    .fn = ~ gsub("\\_Instance\\_Instance", "_Instance", .)
  ) %>%
  rename_with(
    .cols = starts_with("Pairs_Score"),
    .fn = ~ gsub("\\_Instance\\_Instance", "_Instance", .)
  )

# Step 2: Reshape the data into long format
cognitive_long <- cog_tests %>%
  pivot_longer(
    cols = starts_with("Mean.time") | starts_with("Pairs_Score"),
    names_to = c("Test_Type", "Instance"),
    names_sep = "Instance.",
    values_to = "Score"
  ) %>%
  mutate(
    Instance = as.integer(Instance),
    Participant.ID = as.factor(Participant.ID)
  )

# Check the reshaped data
head(cognitive_long)

cat(colnames(cognitive_long), sep = "\n")
# Step 2: Conduct repeated-measures ANOVA for processing speed
anova_processing <- aov_car(Score ~ Instance + Error(Participant.ID/Instance), 
                            data = filter(cognitive_long, Test_Type == "Mean.time.to.correctly.identify.matches..."))

# Extract the within-subject SD for processing speed
# Assuming your ANOVA model is stored in `anova_memory`
anova_processing_residuals <- residuals(anova_processing)

# Calculate the within-subject standard deviation
SD_within_processing <- sd(anova_processing_residuals)

# Step 3: Calculate SRD for processing speed at 95% confidence
z_value <- 1.96 # for 95% confidence
SRD_processing <- z_value * SD_within_processing * sqrt(2)

# Step 4: Conduct repeated-measures ANOVA for visual memory
anova_memory <- aov_car(Score ~ Instance + Error(Participant.ID/Instance), 
                        data = filter(cognitive_long, Test_Type == "Pairs_Score_"))

# Extract the within-subject SD for visual memory
# Assuming your ANOVA model is stored in `anova_memory`
anova_memory_residuals <- residuals(anova_memory)

# Calculate the within-subject standard deviation
SD_within_memory <- sd(anova_memory_residuals)

# Step 5: Calculate SRD for visual memory at 95% confidence
SRD_memory <- z_value * SD_within_memory * sqrt(2)

# Step 6: Apply the SRD threshold to determine significant decline
# Using the original `cog_tests` data, compute declines across instances
cog_tests <- cog_tests %>%
  mutate(
    Significant_Decline_Processing = if_else(
      pmin(
        abs(Mean.time.to.correctly.identify.matches...Instance.1 - Mean.time.to.correctly.identify.matches...Instance.0),
        abs(Mean.time.to.correctly.identify.matches...Instance.2 - Mean.time.to.correctly.identify.matches...Instance.1),
        abs(Mean.time.to.correctly.identify.matches...Instance.3 - Mean.time.to.correctly.identify.matches...Instance.2),
        na.rm = TRUE
      ) > SRD_processing, 
      "Yes", "No"
    ),
    
    Significant_Decline_Visual_Memory = if_else(
      pmin(
        abs(Pairs_Score_Instance.1 - Pairs_Score_Instance.0),
        abs(Pairs_Score_Instance.2 - Pairs_Score_Instance.1),
        abs(Pairs_Score_Instance.3 - Pairs_Score_Instance.2),
        na.rm = TRUE
      ) > SRD_memory, 
      "Yes", "No"
    )
  )

cog_tests%>%
  group_by(Significant_Decline_Visual_Memory)%>%
  summarise(count=n_distinct(Participant.ID))


write_csv(cog_tests, "cog_tests_reg.csv")

# Lets explore whether the menopause transition impacted cognitive decline 

meno <- read.csv("meno_all_instances_reg.csv")


cog_meno <- meno%>%
  left_join(cog_tests, by='Participant.ID')

cog_meno%>%
  group_by(Transition_to_meno, Significant_Decline_Visual_Memory, Significant_Decline_Processing)%>%
  summarise(count=n_distinct(Participant.ID))

# Assuming 'cognitive_data' contains the variables: Participant.ID, Time, Cognitive_Score, Menopause_Status
cog_meno$Significant_Decline_Processing <- as.factor(cog_meno$Significant_Decline_Processing)

cog_meno$Significant_Decline_Visual_Memory <- as.factor(cog_meno$Significant_Decline_Visual_Memory)


cognitive_model <- lm(Mean.time.to.correctly.identify.matches...Instance.3 ~ Transition_to_meno,
                      data = cog_meno)

summary(cognitive_model)


cognitive_model <- lm(Pairs_Score_Instance.3~ Transition_to_meno,
                      data = cog_meno)

summary(cognitive_model)

# Perform a t-test comparing the means between groups
t_test_result <- t.test(Mean.time.to.correctly.identify.matches...Instance.2 ~ Transition_to_meno, data = cog_meno)

print(t_test_result)


t_test_result <- t.test(Pairs_Score_Instance.3 ~ Transition_to_meno, data = cog_meno)

print(t_test_result)

# Perform one-way ANOVA to compare processing speed across menopausal status levels
anova_result <- aov(Mean.time.to.correctly.identify.matches...Instance.2 ~ Menopausal_Status_Instance_0, data = cog_meno)
print(anova_result)

# Perform linear regression comparing processing speed by menopausal status
lm_result <- lm(Mean.time.to.correctly.identify.matches...Instance.0 ~ Menopausal_Status_Instance_0, data = cog_meno)

# View the regression summary
summary(lm_result)


# Let's add the new data onto the big table 

fullcog <- read.csv("fullcog_reg.csv")



fullcog_meno <- fullcog%>%
  left_join(cog_meno, by='Participant.ID')


fullcog_meno <- fullcog_meno %>%
  select(-X.1,
         -X.x,
         -X.y)


fullcog_meno%>%
  group_by(Significant_Decline_Visual_Memory)%>%
  summarise(count=n_distinct(Participant.ID))

subset_full_cog <- fullcog_meno%>%filter(Max_Instance>1)

subset_meno <- fullcog_meno%>%filter(grepl('Y', Transition_to_meno))


# Perform linear regression comparing processing speed by menopausal status

fullcog_meno <- read.csv("fullcog_meno.csv")
lm_result <- lm( ~ Menopausal_Status_Instance_0, data =subset_meno)

# View the regression summary
summary(lm_result)


write.csv(fullcog_meno, "fullcog_meno.csv")


### Revisit smallest real difference: 

cat(colnames(fullcog_meno), sep = "\n")

# Step 1: Standardize column names (remove duplicate Instance parts)
fullcog_meno <- fullcog_meno %>%
  rename_with(
    .cols = starts_with("Mean.time"),
    .fn = ~ gsub("\\_Instance\\_Instance", "_Instance", .)
  ) %>%
  rename_with(
    .cols = starts_with("Pairs_Score"),
    .fn = ~ gsub("\\_Instance\\_Instance", "_Instance", .)
  )

# Step 2: Reshape the data into long format
cognitive_long <- fullcog_meno %>%
  pivot_longer(
    cols = starts_with("Mean.time") | starts_with("Pairs_Score"),
    names_to = c("Test_Type", "Instance"),
    names_sep = "Instance.",
    values_to = "Score"
  ) %>%
  mutate(
    Instance = as.integer(Instance),
    Participant.ID = as.factor(Participant.ID)
  )

# Check the reshaped data
head(cognitive_long)

cat(colnames(cognitive_long), sep = "\n")
# Step 2: Conduct repeated-measures ANOVA for processing speed
anova_processing <- aov_car(Score ~ Instance + Error(Participant.ID/Instance), 
                            data = filter(cognitive_long, Test_Type == "Mean.time.to.correctly.identify.matches..."))

# Extract the within-subject SD for processing speed
# Assuming your ANOVA model is stored in `anova_memory`
anova_processing_residuals <- residuals(anova_processing)

# Calculate the within-subject standard deviation
SD_within_processing <- sd(anova_processing_residuals)

# Step 3: Calculate SRD for processing speed at 95% confidence
z_value <- 1.96 # for 95% confidence
SRD_processing <- z_value * SD_within_processing * sqrt(2)

# Step 4: Conduct repeated-measures ANOVA for visual memory
anova_memory <- aov_car(Score ~ Instance + Error(Participant.ID/Instance), 
                        data = filter(cognitive_long, Test_Type == "Pairs_Score_"))

# Extract the within-subject SD for visual memory
# Assuming your ANOVA model is stored in `anova_memory`
anova_memory_residuals <- residuals(anova_memory)

# Calculate the within-subject standard deviation
SD_within_memory <- sd(anova_memory_residuals)

# Step 5: Calculate SRD for visual memory at 95% confidence
SRD_memory <- z_value * SD_within_memory * sqrt(2)

# Step 6: Apply the SRD threshold to determine significant decline
# Step 6: Apply the SRD threshold to determine significant decline specifically for increases in scores
# Using the original `fullcog_meno` data, compute declines across instances

fullcog_meno <- fullcog_meno %>%
  mutate(
    Significant_Decline_Processing = if_else(
      pmax(
        Mean.time.to.correctly.identify.matches...Instance.1 - Mean.time.to.correctly.identify.matches...Instance.0,
        Mean.time.to.correctly.identify.matches...Instance.2 - Mean.time.to.correctly.identify.matches...Instance.0,
        Mean.time.to.correctly.identify.matches...Instance.3 - Mean.time.to.correctly.identify.matches...Instance.0,
        na.rm = TRUE
      ) > SRD_processing, 
      "Yes", "No"
    ),
    
    Significant_Decline_Visual_Memory = if_else(
      pmax(
        Pairs_Score_Instance.1 - Pairs_Score_Instance.0,
        Pairs_Score_Instance.2 - Pairs_Score_Instance.0,
        Pairs_Score_Instance.3 - Pairs_Score_Instance.0,
        na.rm = TRUE
      ) > SRD_memory, 
      "Yes", "No"
    )
  )

# Summarize results to check count of participants with a significant decline
fullcog_meno %>%
  group_by(Significant_Decline_Processing, Significant_Decline_Visual_Memory) %>%
  summarise(count = n_distinct(Participant.ID))

# Summarize counts by significant decline status
fullcog_meno %>%
  group_by(Significant_Decline_Visual_Memory) %>%
  summarise(count = n_distinct(Participant.ID))



##### Reliable change (RC) is about whether people changed sufficiently that the change is unlikely to be due to simple measurement unreliability.

# Reliability is known for these tests (https://pmc.ncbi.nlm.nih.gov/articles/PMC7170235/)
reliability_processing <- 0.55 
reliability_memory <- 0.41 

# Compute SE_diff for each test
SE_diff_processing <- sqrt(2) * SD_within_processing * sqrt(1 - reliability_processing)
SE_diff_memory <- sqrt(2) * SD_within_memory * sqrt(1 - reliability_memory)

# Identify significant declines based on RCI (Reliable Change Index)
# Identify significant declines based on RCI with NA handling for missing follow-ups
fullcog_meno <- fullcog_meno %>%
  mutate(
    Significant_Decline_Processing_R = case_when(
      # Check if all follow-up instances are NA
      is.na(Mean.time.to.correctly.identify.matches...Instance.1) &
        is.na(Mean.time.to.correctly.identify.matches...Instance.2) &
        is.na(Mean.time.to.correctly.identify.matches...Instance.3) ~ NA_character_,
      
      # Check if any follow-up instance shows a significant decline from baseline
      (Mean.time.to.correctly.identify.matches...Instance.1 - Mean.time.to.correctly.identify.matches...Instance.0) / SE_diff_processing >= 1.96 |
        (Mean.time.to.correctly.identify.matches...Instance.2 - Mean.time.to.correctly.identify.matches...Instance.0) / SE_diff_processing >= 1.96 |
        (Mean.time.to.correctly.identify.matches...Instance.3 - Mean.time.to.correctly.identify.matches...Instance.0) / SE_diff_processing >= 1.96 ~ "Yes",
      
      # Set "No" for participants without significant declines
      TRUE ~ "No"
    ),
    
    Significant_Decline_Visual_Memory_R = case_when(
      # Check if all follow-up instances are NA
      is.na(Pairs_Score_Instance.1) &
        is.na(Pairs_Score_Instance.2) &
        is.na(Pairs_Score_Instance.3) ~ NA_character_,
      
      # Check if any follow-up instance shows a significant decline from baseline
      (Pairs_Score_Instance.1 - Pairs_Score_Instance.0) / SE_diff_memory >= 1.96 |
        (Pairs_Score_Instance.2 - Pairs_Score_Instance.0) / SE_diff_memory >= 1.96 |
        (Pairs_Score_Instance.3 - Pairs_Score_Instance.0) / SE_diff_memory >= 1.96 ~ "Yes",
      
      # Set "No" for participants without significant declines
      TRUE ~ "No"
    )
  )


# Summarize results to check count of participants with a significant decline
fullcog_meno %>%
  group_by(Significant_Decline_Visual_Memory_R) %>%
  summarise(count = n_distinct(Participant.ID))

# Summarize counts by significant decline status
fullcog_meno %>%
  group_by(Significant_Decline_Processing_R) %>%
  summarise(count = n_distinct(Participant.ID))



# Now let's see whether cognitive decline is associated with dementia outcomes in those who transtioned

na_counts <- data.frame(Column = names(fullcog_meno), 
                        NA_Count = colSums(is.na(fullcog_meno)))

cat(apply(na_counts, 1, paste, collapse = ": "), sep = "\n")

cat(colnames(fullcog_meno), sep = "\n")

levels(fullcog_meno$Menopausal_Status_Instance_0)
fullcog_meno$Menopausal_Status_Instance_0 <- as.factor(fullcog_meno$Menopausal_Status_Instance_0)
T <- unique(fullcog_meno$Description.of.cause.of.death...Instance.0)
print(T)
# Create binary dementia column (include death reason in there)

fullcog_meno <- fullcog_meno %>%
  mutate(Dementia_Diagnosis = if_else(!is.na(Date.of.all.cause.dementia.report), "Yes", "No"))

fullcog_meno%>%
  group_by(Dementia_Diagnosis)%>%
  summarise(count=n_distinct(Participant.ID))

# Explicitly specify dplyr::select to avoid conflicts
mean_time_cols <- dplyr::select(fullcog_meno, dplyr::starts_with("Mean.time.to.correctly.identify.matches"))
pairs_score_cols <- dplyr::select(fullcog_meno, dplyr::starts_with("Pairs_Score_Instance"))

# Calculate average scores for each participant across available instances
fullcog_meno <- fullcog_meno %>%
  mutate(
    Avg_Mean_Time = rowMeans(mean_time_cols, na.rm = TRUE),
    Avg_Pairs_Score = rowMeans(pairs_score_cols, na.rm = TRUE)
  )

# Lets try seeing if changes in cognition at different stages of menopause can predict dementia. 

fullcog_meno <- fullcog_meno %>%
  # Calculate average Mean.time for each participant and Menopausal Status level
  mutate(
    Avg_Mean_time_Not_Sure = ifelse(Menopausal_Status_Instance_0 == "Not Sure", 
                                    rowMeans(dplyr::select(., starts_with("Mean.time.to.correctly.identify.matches")), na.rm = TRUE), NA_real_),
    Avg_Mean_time_Perimenopausal = ifelse(Menopausal_Status_Instance_0 == "Perimenopausal", 
                                          rowMeans(dplyr::select(., starts_with("Mean.time.to.correctly.identify.matches")), na.rm = TRUE), NA_real_),
    Avg_Mean_time_Postmenopausal = ifelse(Menopausal_Status_Instance_0 == "Postmenopausal", 
                                          rowMeans(dplyr::select(., starts_with("Mean.time.to.correctly.identify.matches")), na.rm = TRUE), NA_real_),
    Avg_Mean_time_Premenopausal = ifelse(Menopausal_Status_Instance_0 == "Premenopausal", 
                                         rowMeans(dplyr::select(., starts_with("Mean.time.to.correctly.identify.matches")), na.rm = TRUE), NA_real_),
    Avg_Mean_time_Surgical_Menopause = ifelse(Menopausal_Status_Instance_0 == "Surgical menopause", 
                                              rowMeans(dplyr::select(., starts_with("Mean.time.to.correctly.identify.matches")), na.rm = TRUE), NA_real_),
    
    # Calculate average Pairs_Score for each participant and Menopausal Status level
    Avg_Pairs_Score_Not_Sure = ifelse(Menopausal_Status_Instance_0 == "Not Sure", 
                                      rowMeans(dplyr::select(., starts_with("Pairs_Score_Instance")), na.rm = TRUE), NA_real_),
    Avg_Pairs_Score_Perimenopausal = ifelse(Menopausal_Status_Instance_0 == "Perimenopausal", 
                                            rowMeans(dplyr::select(., starts_with("Pairs_Score_Instance")), na.rm = TRUE), NA_real_),
    Avg_Pairs_Score_Postmenopausal = ifelse(Menopausal_Status_Instance_0 == "Postmenopausal", 
                                            rowMeans(dplyr::select(., starts_with("Pairs_Score_Instance")), na.rm = TRUE), NA_real_),
    Avg_Pairs_Score_Premenopausal = ifelse(Menopausal_Status_Instance_0 == "Premenopausal", 
                                           rowMeans(dplyr::select(., starts_with("Pairs_Score_Instance")), na.rm = TRUE), NA_real_),
    Avg_Pairs_Score_Surgical_Menopause = ifelse(Menopausal_Status_Instance_0 == "Surgical menopause", 
                                                rowMeans(dplyr::select(., starts_with("Pairs_Score_Instance")), na.rm = TRUE), NA_real_)
  )



# View the updated dataset with new columns
head(fullcog_meno)

levels(fullcog_meno$Menopausal_Status_Instance_0)


fullcog_meno <- fullcog_meno %>%
  mutate(Surgical_Menopause_YN = if_else(if_any(starts_with("Menopausal_Status_Instance"), 
                                                ~ . == "Surgical menopause"), 
                                         "Y", "N"))


fullcog_meno <- fullcog_meno %>%
  mutate(
    Min_Age_at_Menopause = dplyr::select(., starts_with("Age.at.menopause..last.menstrual.period....Instance")) %>%
      mutate(across(everything(), ~ as.numeric(na_if(na_if(., "Do not know"), "Prefer not to answer")))) %>%
      apply(1, function(x) if(all(is.na(x))) NA else min(x, na.rm = TRUE))
  )


repro <- read.csv("Women_reproductive_participant.csv")
unique(fullcog_meno$Age.at.menarche.num)

r <- fullcog_meno%>%
  group_by(Min_Age_at_Menopause)%>%
  summarise(count=n_distinct(Participant.ID))

# Ensure correct column names and perform the join
fullcog_meno <- fullcog_meno %>%
  left_join(
    dplyr::select(repro, Participant.ID, Age.when.periods.started..menarche....Instance.0), 
    by = "Participant.ID"
  )

fullcog_meno <- fullcog_meno %>%
  mutate(
    Min_Age_at_Menarche = as.numeric(na_if(na_if(Age.when.periods.started..menarche....Instance.0, 
                                                 "Do not know"), 
                                           "Prefer not to answer"))
  )

r <- fullcog_meno%>%
  group_by(LOE)%>%
  summarise(count=n_distinct(Participant.ID))


fullcog_meno$LOE <- fullcog_meno$Min_Age_at_Menopause-fullcog_meno$Min_Age_at_Menarche

fullcog_meno$Early_meno <- fullcog_meno$Min_Age_at_Menopause-fullcog_meno$Min_Age_at_Menarche


filtered_meno_transition <- fullcog_meno %>%
  filter(Transition_to_meno == 'Y' | !grepl('Not Sure|Premenopausal', Menopausal_Status_Instance_0))

r <- filtered_meno_transition%>%
  group_by(LOE)%>%
  summarise(count=n_distinct(Participant.ID))

# get age at bilat in meno age column, recalc LOE

unique(fullcog_meno$Sleep_duration_num)
fullcog_meno <- fullcog_meno %>%
  mutate(
    Sleep_duration_num = as.numeric(na_if(na_if(Sleep.duration...Instance.0, 
                                                "Do not know"), 
                                          "Prefer not to answer")))

head(filtered_meno_transition)



######################### This preps the supplement use columns so we can compare mins/ vist

#### get supplements data in there 

supps <- read.csv("Women_supplements_participant.csv")


unique(supps$Vitamin.and.mineral.supplements...Instance.0)

unique(supps$Vitamin.supplements..pilot....Instance.0)

unique(supps$Mineral.and.other.dietary.supplements...Instance.0)

unique(supps$Vitamin.and.mineral.supplements..pilot....Instance.0)

# Define a function to merge pilot and non-pilot columns by combining unique values
merge_columns <- function(df, col_main, col_pilot) {
  # Apply across each row, splitting values by '|' and removing duplicates
  combined_values <- apply(df[, c(col_main, col_pilot)], 1, function(row) {
    # Split values by '|' and flatten to unique set
    combined <- unique(unlist(strsplit(paste(row, collapse = "|"), split = "\\|")))
    # Remove any empty strings and collapse back with '|'
    paste(combined[combined != ""], collapse = "|")
  })
  # Return the combined column as a new vector
  return(combined_values)
}

# Applying the function for each column pair
supps$Combined_Mineral_Supplements <- merge_columns(supps, 
                                                    "Mineral.and.other.dietary.supplements...Instance.0",
                                                    "Vitamin.and.mineral.supplements..pilot....Instance.0")

supps$Combined_Vitamin_Supplements <- merge_columns(supps,
                                                    "Vitamin.and.mineral.supplements...Instance.0",
                                                    "Vitamin.supplements..pilot....Instance.0")

supps$Fish_oil_combined <- ifelse(
  supps$`Fish oil (including cod liver oil` == "Y" | supps$`Fish oil (including cod liver oil)` == "Y",
  "Y",
  "N"
)
# Ensure correct column names and perform the join

cat(colnames(supps), sep = "\n")

fullcog_meno <- fullcog_meno %>%
  left_join(
    dplyr::select(supps,Participant.ID,
                  Fish_oil_combined,
                  Glucosamine,
                  `Folic acid or Folate (Vit B9)`,
                  `Multivitamins +/- minerals`,
                  Iron,
                  Calcium,
                  `Vitamin E`,
                  `Vitamin C`,
                  `Evening primrose oil`,
                  `Vitamin B`,
                  `Vitamin D`,
                  `Vitamin A`,
                  `Zinc`,
                  Selenium,
                  Garlic,
                  Ginkgo), 
    by = "Participant.ID"
  )


# Get unique supplements from both columns
unique_supplements <- unique(unlist(strsplit(paste(supps$Combined_Mineral_Supplements, supps$Combined_Vitamin_Supplements, sep = "|"), "\\|")))

# Remove any leading/trailing whitespace
unique_supplements <- trimws(unique_supplements)

# Now create a new binary column for each unique supplement
for (supp in unique_supplements) {
  # Create a binary Y/N column based on whether each supplement is present in Combined_Mineral_Supplements or Combined_Vitamin_Supplements
  supps[[supp]] <- ifelse(
    grepl(supp, supps$Combined_Mineral_Supplements, fixed = TRUE) | grepl(supp, supps$Combined_Vitamin_Supplements, fixed = TRUE),
    "Y",
    "N"
  )
}

# View the updated dataset with new binary columns
head(supps)

# View the updated dataframe with combined columns
head(supps)

#write.csv("fullcog_meno.csv")

#fullcog_meno <- read.csv("fullcog_meno.csv")

