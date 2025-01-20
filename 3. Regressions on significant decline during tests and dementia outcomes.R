
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



# dx download fullcog_meno.csv

# fullcog_meno.csv is the fully prepped regression dataset and contains:
# Their menostatus at the different instances
# Whether they transitioned to different meno stages during UKBB repeat assessments
# Their dementia outcomes so far
# Supplement usage
# Covariates (sleep, depression, trauma, BMI, diet, alcohol, smoking, ethnicity)
# Comorbidities (angina, sctroke, diabetes)
# Whether they had cognitive decline in processing or memory during testing (smallest real difference [SRD] and reliable change index [RCI])

fullcog_meno <- read.csv("fullcog_meno.csv")


na_counts <- data.frame(Column = names(fullcog_meno), 
                        NA_Count = colSums(is.na(fullcog_meno)))

cat(apply(na_counts, 1, paste, collapse = ": "), sep = "\n")

cat(colnames(fullcog_meno), sep = "\n")

fullcog_meno$Significant_Decline_Processing<- as.factor(fullcog_meno$Significant_Decline_Processing)
fullcog_meno$Significant_Decline_Processing_R<- as.factor(fullcog_meno$Significant_Decline_Processing_R)
fullcog_meno$Transition_to_meno <- as.factor(fullcog_meno$Transition_to_meno)
fullcog_meno$Dementia_Diagnosis <- as.factor(fullcog_meno$Dementia_Diagnosis)

# Step 1: See if transitioning to menopause during testing impacts processing speed or memory
model_a <- glm(Transition_to_meno ~
                 Avg_Mean_Time + #Processing speed 
                 Avg_Pairs_Score+ # Memory
                 Age.when.attended.assessment.centre...Instance.0 +
                 APOE4 +
                 Ethnicity +
                 DietScore +
                 Townsend.deprivation.index.at.recruitment +
                 Ever.smoked...Instance.0 +
                 Alcohol.drinker.status...Instance.0 +
                 merged_BMI_column +
                 Long.standing.illness..disability.or.infirmity...Instance.0 +
                 Serious_illness_injury_assault_to_yourself +
                 Serious_illness_injury_assault_of_close_relative +
                 Death_of_spouse_or_partner +
                 Death_of_close_relative +
                 Financial_difficulties +
                 Marital_separation_divorce +
                 Seen.doctor..GP..for.nerves..anxiety..tension.or.depression...Instance.0 +
                 Diabetes.diagnosed.by.doctor...Instance.0 +
                 High_blood_pressure +
                 Angina +
                 Heart_attack +
                 Stroke,
               data = fullcog_meno, family = binomial)

summary(model_a)

# Transitioning to menopause worsened processing and memory as both were positively associated with Transition_to_menoY

# Mean.time.to.correctly.identify.matches = Avg_Mean_Time
# Number.of.incorrect.matches.in.round = Avg_Pairs_Score
# RCI (reliable change index) significant decline in processing speed= Significant_Decline_Processing_R
# RCI significant decline in visual memory= Significant_Decline_Visual_Memory_R
# SRD (smallest real difference) significant decline in processing speed= Significant_Decline_Processing
# SRD memory significant decline in visual memory= Significant_Decline_Visual_Memory

### Lets run regressions to explore how menopause impacts cognitive test performance

lm_processing <- glm(Significant_Decline_Processing ~ Transition_to_meno+
                       Menopausal_Status_Instance_0+
                       Sleep_duration_num+
                       +Age.when.attended.assessment.centre...Instance.0+
                       #Mean.time.to.correctly.identify.matches...Instance.0+
                       Ethnicity+
                       DietScore+
                       Townsend.deprivation.index.at.recruitment+
                       Ever.smoked...Instance.0+
                       Alcohol.drinker.status...Instance.0+
                       # Weight.change.compared.with.1.year.ago...Instance.0+
                       merged_BMI_column+
                       APOE4+
                       Number.of.live.births...Instance.0+
                       Yrs_OC+ 
                       Yrs_HRT+
                       #Number.of.treatments.medications.taken...Instance.0+
                       +Serious_illness_injury_assault_to_yourself
                     +Serious_illness_injury_assault_of_close_relative
                     +Had_endometriosis
                     + Diagnosed_infertility
                     +Death_of_spouse_or_partner
                     +Death_of_close_relative
                     +Financial_difficulties
                     +Marital_separation_divorce
                     +Seen.doctor..GP..for.nerves..anxiety..tension.or.depression...Instance.0
                     +Diabetes.diagnosed.by.doctor...Instance.0
                     +High_blood_pressure
                     +Angina
                     +Heart_attack+
                       Fish_oil_combined+
                       Folic.acid.or.Folate..Vit.B9.+
                       Multivitamins.....minerals+
                       Vitamin.B
                     , data =fullcog_meno, family = binomial) 


summary(lm_processing)

tbl_regression(lm_processing, exponentiate = TRUE)

## Transitioning to menopause positively predicted significant decline in processing speed (RCI) and (SRD)

fullcog_meno$Significant_Decline_Visual_Memory <- as.factor(fullcog_meno$Significant_Decline_Visual_Memory_R)

lm_memory <- glm(Significant_Decline_Visual_Memory ~ 
                   Transition_to_meno+
                   #Within_5_Years_of_Menopause+
                   #Menopausal_Status_Instance_0+
                   +Age.when.attended.assessment.centre...Instance.0+
                   Pairs_Score_Instance.0+
                   Ethnicity+
                   DietScore+
                   Townsend.deprivation.index.at.recruitment+
                   Ever.smoked...Instance.0+
                   Alcohol.drinker.status...Instance.0+
                   Weight.change.compared.with.1.year.ago...Instance.0+
                   merged_BMI_column+
                   APOE4+
                   Number.of.live.births...Instance.0+
                   Sleeplessness...insomnia...Instance.0+
                   Morning.evening.person..chronotype....Instance.0+
                   Getting.up.in.morning...Instance.0+
                   Frequency.of.depressed.mood.in.last.2.weeks...Instance.0
                 +Frequency.of.tiredness...lethargy.in.last.2.weeks...Instance.0+
                   Hearing.difficulty.problems...Instance.0+
                   Disability.benefits+
                   Overall.health.rating...Instance.0+
                   Yrs_OC+ 
                   Yrs_HRT+
                   Long.standing.illness..disability.or.infirmity...Instance.0+
                   +Serious_illness_injury_assault_to_yourself
                 +Serious_illness_injury_assault_of_close_relative
                 +Had_endometriosis
                 + Diagnosed_infertility
                 +Death_of_spouse_or_partner
                 +Death_of_close_relative
                 +Financial_difficulties
                 +Marital_separation_divorce
                 +Seen.doctor..GP..for.nerves..anxiety..tension.or.depression...Instance.0
                 +Diabetes.diagnosed.by.doctor...Instance.0
                 +High_blood_pressure
                 +Angina
                 +Heart_attack
                 +Stroke
                 +Back_pain
                 +Hip_pain
                 +Knee_pain
                 +Headache
                 +Neck_or_shoulder_pain
                 +Stomach_or_abdominal_pain
                 +Facial_pain
                 +Pain_all_over_body
                 +Irritability...Instance.0
                 +Miserableness...Instance.0
                 +Sensitivity...hurt.feelings...Instance.0
                 +Fed.up.feelings...Instance.0
                 +Nervous.feelings...Instance.0
                 +Worrier...anxious.feelings...Instance.0
                 +Tense....highly.strung....Instance.0
                 +Loneliness..isolation...Instance.0
                 , data =fullcog_meno, family = binomial)

# View the regression summary
summary(lm_memory)


tbl_regression(lm_memory, exponentiate = TRUE)
tbl_regression(lm_processing, exponentiate = TRUE)


### In BOTH cases going through menopause transition increased risks of significant decline in processing and memory.

################### LME #########################################################

##### We can also look at variances in average processing speed instead of significant decline derived from RCI or SRD

# NOTE: LME failed to converge- this needs more work. 

# Get it into long data so we can do an LME:

fullcog <- fullcog_meno %>%
  dplyr::select(Participant.ID,
                Townsend.deprivation.index.at.recruitment,
                Age.when.attended.assessment.centre...Instance.0,
                Ever.smoked...Instance.0,
                Alcohol.drinker.status...Instance.0,
                merged_BMI_column,
                DietScore,
                Qualifications,
                Ethnicity,
                Disability.benefits,
                APOE4,
                Number.of.live.births...Instance.0,
                Number.of.stillbirths...Instance.0,
                Yrs_HRT,
                Yrs_OC,
                Had_endometriosis,
                Diagnosed_infertility,
                Seen.doctor..GP..for.nerves..anxiety..tension.or.depression...Instance.0,
                Serious_illness_injury_assault_to_yourself,
                Serious_illness_injury_assault_of_close_relative,
                Death_of_spouse_or_partner,
                Death_of_close_relative,
                Financial_difficulties,
                Marital_separation_divorce,
                Long.standing.illness..disability.or.infirmity...Instance.0,
                Weight.change.compared.with.1.year.ago...Instance.0,
                Number.of.treatments.medications.taken...Instance.0,
                Diabetes.diagnosed.by.doctor...Instance.0,
                High_blood_pressure,
                Angina,
                Heart_attack,
                Stroke,
                Sleep_duration_num,
                Fish_oil_combined,
                Glucosamine,
                Folic.acid.or.Folate..Vit.B9.,
                Multivitamins.....minerals,
                Iron,
                Calcium,
                Vitamin.E,
                Vitamin.C,
                Vitamin.B,
                Vitamin.D,
                Vitamin.A,
                Zinc,
                Selenium,
                Min_Age_at_Menopause,
                Min_Age_at_Menarche,
                LOE,
                Menopausal_Status_Instance_0,
                Menopausal_Status_Instance_1,
                Menopausal_Status_Instance_2,
                Menopausal_Status_Instance_3,
                Transition_to_meno,
                Within_5_Years_of_Menopause,
                Mean.time.to.correctly.identify.matches...Instance.0,
                Mean.time.to.correctly.identify.matches...Instance.1,
                Mean.time.to.correctly.identify.matches...Instance.2,
                Mean.time.to.correctly.identify.matches...Instance.3,
                Pairs_Score_Instance.0,
                Pairs_Score_Instance.1,
                Pairs_Score_Instance.2,
                Pairs_Score_Instance.3
  )


fullcog_long <- fullcog %>%
  pivot_longer(cols = matches("Instance"),  # Only pivot columns with 'Instance' in their name
               names_to = c(".value", "Instance"),  # Split the variable names into .value and Instance
               names_pattern = "(.*)[_.]Instance[_.](\\d+)$") %>%
  mutate(Instance = factor(Instance, levels = c(0, 1, 2, 3)))  # Ensure that Instance is ordered correctly

head(fullcog_long)

fullcog_long$Instance <- as.character(fullcog_long$Instance)
fullcog_long$Instance <- as.numeric(fullcog_long$Instance)


### Run the LME

lme_processing <- lmer(Mean.time.to.correctly.identify.matches.. ~ 
                         Transition_to_meno+
                         #Menopausal_Status+
                         +Age.when.attended.assessment.centre..+
                         Ethnicity+
                         DietScore+
                         Townsend.deprivation.index.at.recruitment+
                         Ever.smoked..+
                         Alcohol.drinker.status..+
                         Weight.change.compared.with.1.year.ago..+
                         merged_BMI_column+
                         APOE4+
                         Number.of.live.births..+
                         #Sleeplessness...insomnia...Instance.0+
                         #Morning.evening.person..chronotype....Instance.0+
                         #Getting.up.in.morning...Instance.0+
                         #Frequency.of.depressed.mood.in.last.2.weeks...Instance.0
                         #+Frequency.of.tiredness...lethargy.in.last.2.weeks...Instance.0+
                         #Hearing.difficulty.problems...Instance.0+
                         #Disability.benefits+
                         #Overall.health.rating...Instance.0+
                         Yrs_OC+ 
                         Yrs_HRT+
                         Number.of.treatments.medications.taken..+
                         +Serious_illness_injury_assault_to_yourself
                       +Serious_illness_injury_assault_of_close_relative
                       +Had_endometriosis
                       + Diagnosed_infertility
                       +Death_of_spouse_or_partner
                       +Death_of_close_relative
                       +Financial_difficulties
                       +Marital_separation_divorce
                       +Seen.doctor..GP..for.nerves..anxiety..tension.or.depression..
                       +Diabetes.diagnosed.by.doctor..
                       +High_blood_pressure
                       +Angina
                       +Heart_attack
                       +Stroke
                       +Sleep_duration_num+
                         #+Back_pain
                         #+Hip_pain
                         #+Knee_pain
                         #+Headache
                         #+Neck_or_shoulder_pain
                         #+Stomach_or_abdominal_pain
                         #+Facial_pain
                         #+Pain_all_over_body
                         #+Irritability...Instance.0
                         #+Miserableness...Instance.0
                         #+Sensitivity...hurt.feelings...Instance.0
                         #+Fed.up.feelings...Instance.0
                         #+Nervous.feelings...Instance.0
                         #+Worrier...anxious.feelings...Instance.0
                         #+Tense....highly.strung....Instance.0
                         #+Loneliness..isolation...Instance.0+
                         Fish_oil_combined+
                         #Zinc+
                         #Selenium+
                         #Garlic+
                         #Ginkgo+
                         #Iron+
                         #Calcium+
                         #Glucosamine+
                         Folic.acid.or.Folate..Vit.B9.+
                         Multivitamins.....minerals+
                         # Iron+
                         #Calcium+
                         #Vitamin.E+
                         #Vitamin.C+
                         #Evening.primrose.oil+
                         Vitamin.B+
                         #Vitamin.D+
                         #Vitamin.A
                         +(1|Participant.ID)
                       , data =fullcog_long) 

# View the regression summary
summary(lme_processing)

tbl_regression(lme_processing, exponentiate = TRUE)

### LME model failed to converge


################## DEMENTIA #####################################################

# Step 2: Model the effect of cognitive decline and menopause transition on dementia diagnosis

fullcog_meno$Dementia_Diagnosis <- as.factor(fullcog_meno$Dementia_Diagnosis)

#### Create a cohort of women transitioning to postmenopausal during cognitive testing: 

# Identify women who reached meno within 5 years of first assessment

# Add a column indicating if participant was within 5 years of menopause at first assessment
fullcog_meno <- fullcog_meno %>%
  mutate(Within_5_Years_of_Menopause = ifelse(abs(Min_Age_at_Menopause - Age.when.attended.assessment.centre...Instance.0) <= 5, "Y", "N"))

filtered_meno_transition1 <- fullcog_meno %>% # Filter to get meno transitioners only
  filter(Transition_to_meno == 'Y' | Within_5_Years_of_Menopause=='Y' | !grepl('Premenopausal', Menopausal_Status_Instance_0))

unique(filtered_meno_transition1$Menopausal_Status_Instance_0)

n_distinct(filtered_meno_transition1$Participant.ID)

filtered_meno_transition2 <- filtered_meno_transition1 %>%
  filter(grepl('Not Sure', Menopausal_Status_Instance_0))

n_distinct(filtered_meno_transition2$Participant.ID)



# Step 2: Model the effect of cognitive decline and menopause transition on dementia diagnosis
dementia_a <- glm(Dementia_Diagnosis ~ 
                 Avg_Mean_Time+
                 Avg_Pairs_Score+
                 #Surgical_Menopause_YN+
                 Menopausal_Status_Instance_0+
                 Yrs_OC+
                 Yrs_HRT+
                 Diagnosed_infertility+
                 Had_endometriosis+
                 Age.when.attended.assessment.centre...Instance.0+
                 LOE+
                 Number.of.live.births...Instance.0+
                 APOE4 + 
                 Ethnicity + 
                 DietScore + 
                 Townsend.deprivation.index.at.recruitment + 
                 Ever.smoked...Instance.0 + 
                 Alcohol.drinker.status...Instance.0 + 
                 merged_BMI_column+ 
                 Sleep_duration_num+
                 
                 Serious_illness_injury_assault_to_yourself + 
                 Serious_illness_injury_assault_of_close_relative + 
                 Death_of_spouse_or_partner + 
                 Death_of_close_relative + 
                 Financial_difficulties + 
                 Marital_separation_divorce + 
                 Seen.doctor..GP..for.nerves..anxiety..tension.or.depression...Instance.0 + 
                 Diabetes.diagnosed.by.doctor...Instance.0 + 
                 High_blood_pressure + 
                 Angina + 
                 Heart_attack + 
                 Stroke+
                 
                 Qualifications+ 
                 Fish_oil_combined+
                 #Garlic,
                 #Ginkgo+
                 
                 Folic.acid.or.Folate..Vit.B9.+
                 Multivitamins.....minerals+
                 
                 #`Evening primrose oil`+
                 Vitamin.B+
                 Vitamin.D,
               data =filtered_meno_transition1, family = binomial)

summary(dementia_a)

dementia_b <- glm(Dementia_Diagnosis ~ 
                 Avg_Mean_Time+
                 Avg_Pairs_Score+
                 Surgical_Menopause_YN+
                 Transition_to_meno*Age.when.attended.assessment.centre...Instance.0+
                 #Within_5_Years_of_Menopause+
                 Yrs_OC+
                 Yrs_HRT+
                 
                 LOE+
                 #Number.of.live.births...Instance.0+
                 #Diagnosed_infertility+
                 #Had_endometriosis+
                 APOE4 + 
                 Ethnicity + 
                 DietScore + 
                 Townsend.deprivation.index.at.recruitment + 
                 Ever.smoked...Instance.0 + 
                 Alcohol.drinker.status...Instance.0 + 
                 merged_BMI_column+ 
                 Sleep_duration_num+
                 Long.standing.illness..disability.or.infirmity...Instance.0 + 
                 Serious_illness_injury_assault_to_yourself + 
                 Serious_illness_injury_assault_of_close_relative + 
                 Death_of_spouse_or_partner + 
                 Death_of_close_relative + 
                 Financial_difficulties + 
                 Marital_separation_divorce + 
                 Seen.doctor..GP..for.nerves..anxiety..tension.or.depression...Instance.0 + 
                 Diabetes.diagnosed.by.doctor...Instance.0 + 
                 High_blood_pressure + 
                 Angina + 
                 Heart_attack + 
                 Stroke+
                 Qualifications+ 
                 Fish_oil_combined+
                 Zinc+
                 Selenium+
                 #Garlic,
                 #Ginkgo+
                 Iron+
                 Calcium+
                 Glucosamine+
                 Folic.acid.or.Folate..Vit.B9.+
                 #Multivitamins.....minerals+
                 Iron+
                 Calcium+
                 Vitamin.E+
                 #Vitamin.C+
                 #`Evening primrose oil`+
                 Vitamin.B+
                 Vitamin.D,
               #Vitamin.A,
               data =filtered_meno_transition1, family = binomial)

summary(dementia_b)







