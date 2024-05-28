install.packages("gtsummary")
install.packages("broom.mixed")
library(gtsummary)
library(broom.mixed)
library(dplyr)

women_apoe <- women_apoe  %>%
  select(-"...1",
         -"...3")
#Get outcomes from key_outcomes_with_death and baseline from lifestyle_table_new

outcomes <- read.csv('Key_outcomes_table_withdeath.csv')

baseline <- read.csv('Lifestyle_table_new.csv')

### Merge bilat/menopause ages so we have just one menopause age column 
# If data is entered in both just use the bilat oopherectomy data if it's smaller than meno age data:
#Amend this code below to include correct variable/ table names

baseline$mergedAge <- ifelse(
  !is.na(baseline$bilateral_oophorectomyAge) & !is.na(baseline$menopauseAge),
  pmin(baseline$bilateral_oophorectomyAge, baseline$menopauseAge, na.rm = TRUE),
  ifelse(!is.na(baseline$bilateral_oophorectomyAge), baseline$bilateral_oophorectomyAge, baseline$menopauseAge)
)

# Create a cohort who experienced menopause around the same time they were at instance 0 using the new merged meno age column
# Aim for within 5 years each way in women who did not have bilat ooph & from 0 years to 5 years in women who had bilat ooph.

baseline$yrsfromMeno <- baseline$menopauseAge-baseline$Age.at.recruitment

baseline$yrsfromBilat <- baseline$bilateral_oophorectomyAge-baseline$Age.at.recruitment

meno1 <- baseline %>%
  filter((between(yrsfromMeno, -5, 5)) | (between(yrsfromBilat, 0, 5)))

# This type of code will create a binary Y/N column for women who had a bilateral oopherectomy using the bilateral_oophorectomy_age column

meno1$Oophorectomy_Occurred <- ifelse(!is.na(meno1$bilateral_oophorectomyAge), "Y", "N")

# This code can be used for HRT, oral contraceptives and dementia once it's added. 
#HRT
meno1$HRT_Used <- ifelse(!is.na(meno1$Ever.used.hormone.replacement.therapy..HRT.), "Y", "N")

#contraceptives
meno1$Contraceptive_Used <- ifelse(!is.na(meno1$Ever.taken.oral.contraceptive.pill), "Y", "N")

# Identify all columns where someone reported vitamin/ mineral supplement use.
# Amend vitamin.supplement.user column so Yes for used supplements or No based on data from other vit/min columns. 
# Again this code below will need to be amended for the main meno cohort table you're using. 

baseline$recoded_vits1 <- ifelse(baseline$Vitamin.and.mineral.supplements...Instance.0 == "None of the above", "N",
                                 ifelse(baseline$Vitamin.and.mineral.supplements...Instance.0 == "Prefer not to say", "Unknown", "Y"))
# Do similar to the above for any other vitamin/ mineral columns 
# Create a new column which checks if there's a Y in any of the vit/ min columns, if not it will input "Unknown" or "None of the above" from recoded_vits1
# My suggested vit code
meno1$Vitamin_or_Supplement_User <- "N"
meno1$Vitamin_or_Supplement_User <- ifelse(meno1$Vitamin.and.mineral.supplements...Instance.0 %in% c(NA, "Prefer not to answer", "None of the above") & 
                                             meno1$Vitamin.and.or.mineral.supplement.use...Instance.0 %in% c(NA, "Prefer not to answer", "None of the above"),
                                           "N",
                                           "Y")


# Next step is to identify dementia/ alzhiemers columns from outcomes (first occurrence dates should be fine)
# Then we can join those columns onto meno cohort on ID
keyword <- "G30"
matching_columns <- grep(keyword, names(outcomes), value = TRUE)
print(matching_columns)

dementia_columns <- c("Date.F00.first.reported..dementia.in.alzheimer.s.disease.", 
                      "Date.F02.first.reported..dementia.in.other.diseases.classified.elsewhere.", 
                      "Date.F03.first.reported..unspecified.dementia.",
                      "Date.F05.first.reported..delirium..not.induced.by.alcohol.and.other.psychoactive.substances.",
                      "Date.G30.first.reported..alzheimer.s.disease.")

dementia_columns <- outcomes %>%
  select('Participant.ID',
         'Date.F00.first.reported..dementia.in.alzheimer.s.disease.', 
         'Date.F02.first.reported..dementia.in.other.diseases.classified.elsewhere.', 
         'Date.F03.first.reported..unspecified.dementia.',
         'Date.F05.first.reported..delirium..not.induced.by.alcohol.and.other.psychoactive.substances.',
         'Date.G30.first.reported..alzheimer.s.disease.')
meno_dementia = left_join(meno1, dementia_columns, by = "Participant.ID")

meno_dementia  <- meno_dementia  %>%
  select(-"...1",
         -"...2",
         -"Date.F05.first.reported..delirium..not.induced.by.alcohol.and.other.psychoactive.substances.")

# combine the earliest date from all dementia columns into one column:
meno_dementia$dementia_diagnosis <- NA

meno_dementia$dementia_diagnosis <- pmin(as.Date(meno_dementia$Date.F00.first.reported..dementia.in.alzheimer.s.disease., origin = "1970-01-01"),
                                         as.Date(meno_dementia$Date.F02.first.reported..dementia.in.other.diseases.classified.elsewhere., origin = "1970-01-01"),
                                         as.Date(meno_dementia$Date.F03.first.reported..unspecified.dementia., origin = "1970-01-01"),
                                         as.Date(meno_dementia$Date.G30.first.reported..alzheimer.s.disease., origin = "1970-01-01"),
                                         na.rm = TRUE)
# create a time distance variable from date of Instance 0 to first occurance of dementia in combined date column
meno_dementia$dementia_diagnosis <- as.Date(meno_dementia$dementia_diagnosis, origin = "1970-01-01")
meno_dementia$Date.of.attending.assessment.centre...Instance.0 <- as.Date(meno_dementia$Date.of.attending.assessment.centre...Instance.0, origin = "1970-01-01")

meno_dementia$dementia_time_distance <- ifelse(is.na(meno_dementia$dementia_diagnosis) | is.na(meno_dementia$Date.of.attending.assessment.centre...Instance.0), 
                                               NA, 
                                               meno_dementia$dementia_diagnosis - meno_dementia$Date.of.attending.assessment.centre...Instance.0)

# create a binary variable for dementia Y/N by seeing if there are any values in combined demntia date column
meno_dementia$Had_Dementia <- "N"
meno_dementia$Had_Dementia <- ifelse(!is.na(meno_dementia$dementia_diagnosis), "Y", "N")
# Quality control 
# Check all date ranges and age ranges (if they're bizarre we'll exclude)
# Look up other similar ukbb studies and explore their methods for exclusions/ quality control
meno_death_filtered <- meno_dementia %>% 
  filter(Participant.ID != 4001063)

ids_to_remove <- c(2062700, 2542951, 3144677, 3325607, 3486895, 3828146, 4098093, 4284920, 4482316, 5079250, 5397385, 5441538)
meno_death_filtered <- meno_death_filtered %>%
  filter(!Participant.ID %in% ids_to_remove)

# Future steps: Cox proprotional hazard regression

# Future steps: we'll look at matching men and women, and look at controlling for APO-E.

meno_dementia_new <- read.csv('meno_dementia_new.csv')
Death_table_participant <- read.csv('Death_table_participant.csv')

#creating Lifetime oestrogen exposure column.
meno_dementia_new$LOE = meno_dementia_new$mergedAge - meno_dementia_new$menarcheAge

Death_table_participant$Death_date = Death_table_participant$Date.of.death...Instance.0
Death_table_participant$Participant.ID = Death_table_participant$Participant.ID
meno_death <- merge(meno_dementia_new, Death_table_participant[, c("Participant.ID", "Death_date")], by = "Participant.ID", all.x = TRUE)

meno_death$Death_date <- as.Date(meno_death$Death_date)
class(meno_death$Death_date)

meno_death$dementia_diagnosis <- as.Date(meno_death$dementia_diagnosis)
class(meno_death$dementia_diagnosis)

meno_death <- meno_death %>%
  mutate(
    Death_date = as.Date(Death_date),
    dementia_diagnosis = as.Date(dementia_diagnosis),
    dementia_diagnosis2 = if_else(
      is.na(dementia_diagnosis) & !is.na(Death_date),
      Death_date,
      if_else(
        is.na(dementia_diagnosis),
        as.Date('2022-12-08'),
        dementia_diagnosis
      )
    )
  )


####### Next steps #######

## Time distances
# For both the male and female tables we need a new time_distance column 
# we need times distance from date of Instance 0 to date in dementia_diagnosis2 column
meno_dementia$dementia_diagnosis2 <- as.Date(meno_dementia$dementia_diagnosis, origin = "1970-01-01")
meno_dementia$Date.of.attending.assessment.centre...Instance.0 <- as.Date(meno_dementia$Date.of.attending.assessment.centre...Instance.0, origin = "1970-01-01")

meno_death$dementia_time_distance2 <- ifelse(is.na(meno_death$dementia_diagnosis2) | is.na(meno_death$Date.of.attending.assessment.centre...Instance.0), 
                                               NA, 
                                               meno_death$dementia_diagnosis2 - meno_death$Date.of.attending.assessment.centre...Instance.0)

men_only_regression$dementia_diagnosis2 <- as.Date(men_only_regression$dementia_diagnosis2, origin = "1970-01-01")
men_only_regression$Date.of.attending.assessment.centre...Instance.0 <- as.Date(men_only_regression$Date.of.attending.assessment.centre...Instance.0, origin = "1970-01-01")

men_only_regression$dementia_time_distance2 <- ifelse(is.na(men_only_regression$dementia_diagnosis2) | is.na(men_only_regression$Date.of.attending.assessment.centre...Instance.0), 
                                             NA, 
                                             men_only_regression$dementia_diagnosis2 - men_only_regression$Date.of.attending.assessment.centre...Instance.0)

## Psych Covariates: 
psychiatric_variables_participant <- psychiatric_variables_participant %>%
  rename(Participant.ID = `Participant ID`)
meno_death = left_join(meno_death, psychiatric_variables_participant, by = "Participant.ID")

men_only_regression = left_join(men_only_regression, psychiatric_variables_participant, by = "Participant.ID")



## Merging & Saving sets
# We need to merge the male and female datasets to create a 'both' cohort.
# We then need to save all 3 of the individual data tables of the men/women/both cohorts into the UKBB project

## Cox regression analysis: 

# Once we have the new columns we can conduct a Cox regression exploring dementia in women.

# These are the variables we will need in the regression formula:

# Outcomes: Dementia Y/N, distance
# Reproductive: Age at last live birth, LOE, contraceptive pill, HRT, oopherectomy, mergedAge
# Lifestyle: Exercise, Alcohol, Smoking, Supplement use, BMI, sleep duration, Diet, frequency of tirdness/lethargy**
# Covariates: Deprivation, Ethnicity, Age at recruitment, Qualifications
# Medical Covariates: number of meds taken, heart problems, diabetes, cholesterol, cancer, osteoarthritus, rhuematoid arthritus
# Psych covariates: trauma (illness injury bereavement), neuroticism score

# Some of these variables may need some additional prepping/ sanitation 
# REMOVED Medication.for.cholesterol..blood.pressure.or.diabetes...Instance.0 in women_apoe as all NA responses.
# Regression formula (will need to add variables accordingly)
install.packages("survival")
library(survival)
library(dplyr)
#Altering columns
women_apoe$Had_Dementia <- as.numeric(women_apoe$Had_Dementia == 'Y' | women_apoe$Had_Dementia == 'y')
women_apoe <- women_apoe %>%
  rename(Frequency.of.tiredness.lethargy.in.last.2.weeks = "Frequency of tiredness / lethargy in last 2 weeks | Instance 0")
women_apoe <- women_apoe %>%
  rename(Illness.injury.bereavement.stress.in.last.2.years = "Illness, injury, bereavement, stress in last 2 years | Instance 0")
women_apoe <- women_apoe %>%
  rename(Neuroticism.score = "Neuroticism score | Instance 0")

women_apoe <- women_apoe %>%
  mutate(Ethnic.background...Instance.0 = as.factor(Ethnic.background...Instance.0))
women_apoe <- women_apoe %>%
  mutate(Sleep.duration...Instance.0 = as.factor(Sleep.duration...Instance.0))
women_apoe <- women_apoe %>%
  mutate(Sleeplessness...insomnia...Instance.0 = as.factor(Sleeplessness...insomnia...Instance.0))
women_apoe <- women_apoe %>%
  mutate(Vascular.heart.problems.diagnosed.by.doctor...Instance.0 = as.factor(Vascular.heart.problems.diagnosed.by.doctor...Instance.0))
women_apoe <- women_apoe %>%
  mutate(Diabetes.diagnosed.by.doctor...Instance.0 = as.factor(Diabetes.diagnosed.by.doctor...Instance.0))
women_apoe <- women_apoe %>%
  mutate(Ever.had.osteoarthritis.affecting.one.or.more.joints..e.g..hip..knee..shoulder. = as.factor(Ever.had.osteoarthritis.affecting.one.or.more.joints..e.g..hip..knee..shoulder.))
women_apoe <- women_apoe %>%
  mutate(Ever.had.rheumatoid.arthritis.affecting.one.or.more.joints = as.factor(Ever.had.rheumatoid.arthritis.affecting.one.or.more.joints))
women_apoe <- women_apoe %>%
  mutate(Cancer.diagnosed.by.doctor...Instance.0 = as.factor(Cancer.diagnosed.by.doctor...Instance.0))
women_apoe <- women_apoe %>%
  mutate(Cancer.code..self.reported...Instance.0 = as.factor(Cancer.code..self.reported...Instance.0))
women_apoe <- women_apoe %>%
  mutate(SmokingBaseline = as.factor(SmokingBaseline))
women_apoe <- women_apoe %>%
  mutate(AlcoholBaseline = as.factor(AlcoholBaseline))
women_apoe <- women_apoe %>%
  mutate(Vitamin_or_Supplement_User = as.factor(Vitamin_or_Supplement_User))
women_apoe <- women_apoe %>%
  mutate(Oophorectomy_Occurred = as.factor(Oophorectomy_Occurred))
women_apoe <- women_apoe %>%
  mutate(HRT_Used = as.factor(HRT_Used))
women_apoe <- women_apoe %>%
  mutate(Contraceptive_Used = as.factor(Contraceptive_Used))
women_apoe <- women_apoe %>%
  mutate(Frequency.of.tiredness.lethargy.in.last.2.weeks = as.factor(Frequency.of.tiredness.lethargy.in.last.2.weeks))
women_apoe <- women_apoe %>%
  mutate(Neuroticism.score = as.factor(Neuroticism.score))
women_apoe <- women_apoe %>%
  mutate(Illness.injury.bereavement.stress.in.last.2.years = as.factor(Illness.injury.bereavement.stress.in.last.2.years))

#Remove negative values from dementia time distance2 column
negative_dementia_time_distance_ids <- women_apoe %>%
  filter(dementia_time_distance2 < 0) %>%
  select(Participant.ID)
print(negative_dementia_time_distance_ids)

ids_to_remove <- c (1440685, 1588172, 1863127, 1982460, 2224142, 2263519, 4362678, 4501875, 4810643, 4994228, 5470676)
women_apoe <- women_apoe %>%
  filter(!Participant.ID %in% ids_to_remove)

#missing value overview: 
missing_ids <- women_apoe %>%
  filter(is.na(SmokingBaseline) | is.na(AlcoholBaseline) | is.na(Frequency.of.tiredness.lethargy.in.last.2.weeks)) %>%
  select(Participant.ID, SmokingBaseline, AlcoholBaseline, Frequency.of.tiredness.lethargy.in.last.2.weeks)

ids_to_remove <- c(1585054, 2587429, 5305788)
women_apoe <- women_apoe %>%
  filter(!Participant.ID %in% ids_to_remove)

missing_ids <- women_apoe %>%
  filter(is.na(Had.menopause)) %>%
  select(Participant.ID, Had.menopause)
women_apoe <- women_apoe %>% 
  filter(Participant.ID != 1185904)

women_apoe <- women_apoe %>%
  mutate(`Cancer.code..self.reported...Instance.0` = ifelse(is.na(`Cancer.code..self.reported...Instance.0`), 'No', `Cancer.code..self.reported...Instance.0`))

#Feature plot of variable distributions
install.packages("caret")
library(caret)
featurePlot(women_apoe %>% select(APOE4, HRT_Used), women_apoe %>% select(Had_Dementia), plot='density')

#Data cleaning
head(women_apoe)

summary(women_apoe)

glimpse(women_apoe)

colSums(is.na(women_apoe))

#overview of missing values: Townsend - 0.12% missing, MET activity - 19.43%, Age at first live birth - 33.95%, BMI - 0.3%, No. of meds taken - 0.01%, Meds for cholesterol - 100%, 
#Ever had Rheumatoid arthritis - 55.09%, Ever had osteoarthritis  55.09%, QualScore - 0.70%, No. of live births - 0.06%, bilat_oophAge - 95.91%
#Age at last live birth - 33.99%, LOE - 2.75%, Neuroticism - 18.70%, Illnesss, injury etc - 0.17%

#cox
cox_model <- coxph(Surv(dementia_time_distance2, Had_Dementia) ~ Age.at.last.live.birth + LOE + Contraceptive_Used + HRT_Used + Oophorectomy_Occurred + mergedAge + Summed.MET.minutes.per.week.for.all.activity...Instance.0 + AlcoholBaseline + SmokingBaseline + Vitamin_or_Supplement_User + Body.mass.index..BMI....Instance.0 + Sleep.duration...Instance.0 + DietScore + Frequency.of.tiredness.lethargy.in.last.2.weeks + Townsend.deprivation.index.at.recruitment + Ethnic.background...Instance.0 + Age.at.recruitment + QualScore + Number.of.treatments.medications.taken...Instance.0 + Vascular.heart.problems.diagnosed.by.doctor...Instance.0 + Diabetes.diagnosed.by.doctor...Instance.0 + Cancer.diagnosed.by.doctor...Instance.0 + Ever.had.osteoarthritis.affecting.one.or.more.joints..e.g..hip..knee..shoulder. + Ever.had.rheumatoid.arthritis.affecting.one.or.more.joints + Illness.injury.bereavement.stress.in.last.2.years + Neuroticism.score, data = women_apoe)

# GtSummary tables 
tbl_regression(cox_model, exponentiate=TRUE)

summary(cox_model)

#next session
women_apoe <- read.csv('women_apoe_new.csv')

# Example of how regression table can be improved to be publication ready (will need variables amended accoridngly)

fm3 %>%
  tbl_regression(broom.mixed::tidy,
                 exponentiate = FALSE,
                 label = list(
                   Engagement ~ "Total Weeks Engaged",
                   HRT~"HRT Use",
                   contraceptives~"Contraceptive Use",
                   supplements~"Supplement Use",
                   comorbs~ "Current Comorbidities",
                   Period~"Period Logging",
                   Trigger~"Trigger Logging",
                   Activity~"Used Activities",
                   PrePost~ "Pre 2-Months App Use Versus Post 2-Months App Use",
                   'id.sd__(Intercept)'~ "SD of Random Effects of ID (Intercept)",
                   Residual.sd__Observation~ "SD of Residual Observation"
                 )) %>% 
  bold_labels() %>%
  italicize_levels()%>%
  modify_column_hide(columns = p.value)%>%
  as_gt() %>%
  gt::tab_options(table.font.names = "Times New Roman") 

# Descriptive stats

# In the brackets below only input the column numbers in meno_death which were used in the regression above, also remove ID column

trial2 <- meno_death[,c(-1,-14)] 

table1 <- tbl_summary(trial2, by= Had_Dementia)

table1

# Example of how a tbl_summary can be amended to be publication ready (will need variables altered)

table2 <- 
  tbl_summary(
    trial2,
    type = list(c(Age) ~ "continuous"),
    by = Engagement, # split table by group
    missing = "no",# don't list missing data separately
    value = list(contraceptives ~ "2",
                 HRT ~ "2",
                 supplements~"2",
                 comorbs~"2"),
    label = list(
      Engagement ~ "Number of Weeks Engaged",
      HRT~"HRT Use",
      contraceptives~"Contraceptive Use",
      supplements~"Supplement Use",
      comorbs~ "Current Comorbidities",
      Period~"Period Logging",
      Trigger~"Trigger Logging",
      Activity~"Used Activities",
      LogTotal~ "Symptom Log Total",
      MinSymscore~ "Baseline Symptom Total",
      MaxSymscore~ "Symptom Total after 2-months")) %>%
  add_p() %>% # test for a difference between groups
  add_q(method = "BH")%>%
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels() %>%
  modify_spanning_header(all_stat_cols() ~ "**Weeks Engaged**")%>%
  modify_footnote(
    all_stat_cols() ~ "Median (IQR) or Frequency (%)"
  ) %>%
  as_gt() %>%
  gt::tab_options(table.font.names = "Times New Roman")

table2

cholesterol_column_participant <- cholesterol_column_participant %>%
  rename(Participant.ID = "Participant ID")



