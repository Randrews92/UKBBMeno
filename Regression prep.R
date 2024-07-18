install.packages("gtsummary")
install.packages("broom.mixed")
library(gtsummary)
library(broom.mixed)
library(dplyr)

women_table <- women_table  %>%
  select(-"X.4",
         -"X.3",
         -"X.2",
         -"X.1",
         -"X")
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

if ("cli" %in% search()) detach("package:carat", unload = TRUE)

install.packages('pacman')
library(pacman)
pacman::p_load(carat)
install.packages("cli")
library(cli)
install.packages("pillar")
library(pillar)
#Data cleaning
head(women_apoe)

summary(women_apoe)

glimpse(women_apoe)

colSums(is.na(women_table))

#overview of missing values: Townsend - 0.12% missing, MET activity - 19.43%, Age at first live birth - 33.95%, BMI - 0.3%, No. of meds taken - 0.01%, Meds for cholesterol - 100%, 
#Ever had Rheumatoid arthritis - 55.09%, Ever had osteoarthritis  55.09%, QualScore - 0.70%, No. of live births - 0.06%, bilat_oophAge - 95.91%
#Age at last live birth - 33.99%, LOE - 2.75%, Neuroticism - 18.70%, Illnesss, injury etc - 0.17%

install.packages("gtsummary")
library(gtsummary)
#cox
cox_model <- coxph(Surv(dementia_time_distance2, Had_Dementia) ~ Age.at.last.live.birth + LOE + Contraceptive_Used + HRT_Used + Oophorectomy_Occurred + mergedAge + Summed.MET.minutes.per.week.for.all.activity...Instance.0 + AlcoholBaseline + SmokingBaseline + Vitamin_or_Supplement_User + Body.mass.index..BMI....Instance.0 + Sleep.duration...Instance.0 + DietScore + Frequency.of.tiredness.lethargy.in.last.2.weeks + Townsend.deprivation.index.at.recruitment + Ethnic.background...Instance.0 + Age.at.recruitment + QualScore + Number.of.treatments.medications.taken...Instance.0 + Vascular.heart.problems.diagnosed.by.doctor...Instance.0 + Diabetes.diagnosed.by.doctor...Instance.0 + Cancer.diagnosed.by.doctor...Instance.0 + Ever.had.osteoarthritis.affecting.one.or.more.joints..e.g..hip..knee..shoulder. + Ever.had.rheumatoid.arthritis.affecting.one.or.more.joints + Illness.injury.bereavement.stress.in.last.2.years + Neuroticism.score, data = women_apoe)
cox_model <- coxph(Surv(dementia_time_distance2, Had_Dementia) ~ LOE + Body.mass.index..BMI....Instance.0, data = women_table)
cox_model <- coxph(Surv(dementia_time_distance2, Had_Dementia) ~ LOE + Contraceptive_Used + HRT_Used + Oophorectomy_Occurred + APOE4 + BMI + DietScore_binary + Vitamin_or_Supplement_User, data = women_table)

# GtSummary tables
tbl_regression(cox_model, exponentiate=TRUE)

summary(cox_model)

cox_model <- glm(Had_Dementia ~ LOE + Body.mass.index..BMI....Instance.0 + Contraceptive_Used + HRT_Used, data = women_table, family = "binomial")

#next session
women_apoe <- read.csv('women_apoe.csv')
women_table <- read.csv('Regression5.csv')
men_table <- read.csv('men_table.csv')
#count the amunt of Y/N or 1/0. e.g. 
install.packages("dplyr")
library(dplyr)
#Binary
A <- women_apoe %>%
  group_by(APOE4) %>%
  summarise(COUNT = n_distinct(Participant.ID))
B <- women_apoe %>%
  group_by(Had.menopause) %>%
  summarise(COUNT = n_distinct(Participant.ID))
C <- women_apoe %>%
  group_by(Oophorectomy_Occurred) %>%
  summarise(COUNT = n_distinct(Participant.ID))
C <- women_apoe %>%
  group_by(DietScore_binary) %>%
  summarise(COUNT = n_distinct(Participant.ID))
#Summary of counts in binary variables:
#Summary of counts in binary variables:
#APOE4 = Yes = 13,646, No = 34,275
#Had.menopause = Yes = 47,595, Hysterectomy = 309, other reason = 17
#Had.Dementia = Yes = 138, No = 47783
#Vitamin_or_Supplement_User = Yes = 18,368, No = 29,553
#Contraceptive_Used = Yes = 42,159, No = 42,159
#HRT_Used = Yes = 13,533, No = 34,388
#Oophorectomy_occurred = Yes = 1961, No = 45,960

#Medians:
median_menarche_age <- median(women_apoe$menarcheAge, na.rm = TRUE)
print(median_menarche_age)
#Median menarche age = 13
#Impute data:
library(dplyr)
women_apoe <- women_apoe %>%
  mutate(menarcheAge = ifelse(is.na(menarcheAge), 13, menarcheAge))
print(women_apoe)


#fix LOE column with imputed menarcheAge values:
women_apoe$LOE = women_apoe$mergedAge - women_apoe$menarcheAge

#Histogram
install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
install.packages("factoextra")
library(factoextra)
options(repr.plot.width=20, repr.plot.height=15)
library(dplyr)
library(tidyr)

women_table2 %>% gather(attributes, value, 1:ncol(women_table2)) %>%
  ggplot(aes(x = value)) +
  geom_histogram(fill="lightblue",colour="black") +
  facet_wrap(~attributes, scales = "free")

women_table2 <- women_table2 %>%
  mutate(HRT_Used = ifelse(HRT_Used == "Y", 1, ifelse(HRT_Used == "N", 0, NA)))
women_table2 <- women_table2 %>%
  mutate(Vitamin_or_Supplement_User = ifelse(Vitamin_or_Supplement_User == "Y", 1, ifelse(Vitamin_or_Supplement_User == "N", 0, NA)))
women_table2 <- women_table2 %>%
  mutate(APOE4 = ifelse(APOE4 == "Y", 1, ifelse(APOE4 == "N", 0, NA)))
women_table2 <- women_table2 %>%
  mutate(Oophorectomy_Occurred = ifelse(Oophorectomy_Occurred == "Y", 1, ifelse(Oophorectomy_Occurred == "N", 0, NA)))

#Fixing DietScore
library(dplyr)
women_apoe <- women_apoe %>%
  mutate(DietScore_binary = ifelse(DietScore >= 5, 1, 0))

##
women_table <- women_apoe %>%
  select(QualScore, SmokingBaseline, AlcoholBaseline, DietScore, menopauseAge, menarcheAge, Number.of.live.births, bilateral_oophorectomyAge, mergedAge, yrsfromMeno, yrsfromBilat, Oophorectomy_Occurred, HRT_Used, Contraceptive_Used, Vitamin_or_Supplement_User, Had_Dementia, LOE, APOE4)

women_table2 <- women_table %>%
  select(Participant.ID, DietScore, menarcheAge, mergedAge, Oophorectomy_Occurred, HRT_Used, Contraceptive_Used, Vitamin_or_Supplement_User, Had_Dementia, LOE, APOE4)

women_table2 <- women_table %>%
  select(Body.mass.index..BMI....Instance.0)

#Fixing Missing values
#Arthritis
library(dplyr)
women_table <- women_table %>%
  mutate(Ever.had.rheumatoid.arthritis.affecting.one.or.more.joints = ifelse(is.na(Ever.had.rheumatoid.arthritis.affecting.one.or.more.joints), "No", Ever.had.rheumatoid.arthritis.affecting.one.or.more.joints))
women_table <- women_table %>%
  mutate(Ever.had.osteoarthritis.affecting.one.or.more.joints..e.g..hip..knee..shoulder. = ifelse(is.na(Ever.had.osteoarthritis.affecting.one.or.more.joints..e.g..hip..knee..shoulder.), "No", Ever.had.osteoarthritis.affecting.one.or.more.joints..e.g..hip..knee..shoulder.))
#BMI
median_BMI <- median(women_table$Body.mass.index..BMI....Instance.0, na.rm = TRUE)
print(median_BMI)
women_table <- women_table %>%
  rename(BMI = Body.mass.index..BMI....Instance.0)

#Median BMI = 25.7706
women_table <- women_table %>%
  mutate(Body.mass.index..BMI....Instance.0 = ifelse(is.na(Body.mass.index..BMI....Instance.0), 25.7706, Body.mass.index..BMI....Instance.0))
print(women_table)
colSums(is.na(women_table))

#MET
#missing = 9312
#Range:
library(dplyr)
met_range <- range(women_table$Summed.MET.minutes.per.week.for.all.activity...Instance.0, na.rm = TRUE)
met_median <- median(women_table$Summed.MET.minutes.per.week.for.all.activity...Instance.0, na.rm = TRUE)
met_range
met_median
#Range = from 0-19,278
#Median = 1644

#Removing Age at first+last live birth for now due to missing data:
library(dplyr)
women_table  <- women_table  %>%
  select(-'X.1',
         -'X.2',
         -'X.3',
         -'X.4',
         -'X',
         -'Age.at.first.live.birth...Instance.0',
         -'Age.at.last.live.birth')

#Other columns
#Vascular
response_counts <- table(women_table$Vascular.heart.problems.diagnosed.by.doctor...Instance.0)
print(response_counts)

women_table$Vascular_problem_binary <- ifelse(
  women_table$Vascular.heart.problems.diagnosed.by.doctor...Instance.0 %in% c("None of the above", "Prefer not to answer"), 
  "N", 
  "Y"
)
#Diabetes
response_counts <- table(women_table$Diabetes.diagnosed.by.doctor...Instance.0)
print(response_counts)
women_table$Diabetes_binary <- ifelse(
  women_table$Diabetes.diagnosed.by.doctor...Instance.0 %in% c("No", "Do not know", "Prefer not to answer"), 
  "N", 
  "Y"
)
response_counts <- table(women_table$Diabetes_binary)

#Depression columns
Depression_columns  <- Depression_columns  %>%
  select(-'...1')
women_table_depression = left_join(women_table, Depression_columns, by = "Participant.ID")

depression_check <- women_table_depression %>%
  select(`Participant.ID`,
         `Year.of.birth`,
         `Date.F32.first.reported..depressive.episode.`,
         `Date.F33.first.reported..recurrent.depressive.disorder.`)

#Regression table columns
Regression_columns <- women_table_depression %>%
  select(`Participant.ID`,
         `Year.of.birth`,
         `Townsend.deprivation.index.at.recruitment`,
         `Ethnic.background...Instance.0`,
         `Summed.MET.minutes.per.week.for.all.activity...Instance.0`,
         `BMI`,
         `Sleep.duration...Instance.0`,
         `Sleeplessness...insomnia...Instance.0`,
         `Number.of.treatments.medications.taken...Instance.0`,
         `Vascular_problem_binary`,
         `Diabetes_binary`,
         `Ever.had.rheumatoid.arthritis.affecting.one.or.more.joints`,
         `Ever.had.osteoarthritis.affecting.one.or.more.joints..e.g..hip..knee..shoulder.`,
         `Cancer.diagnosed.by.doctor...Instance.0`,
         `Age.at.recruitment`,
         `QualScore`,
         `SmokingBaseline`,
         `AlcoholBaseline`,
         `DietScore`,
         `Number.of.live.births`,
         `mergedAge`,
         `Oophorectomy_Occurred`,
         `HRT_Used`,
         `Contraceptive_Used`,
         `Vitamin_or_Supplement_User`,
         `LOE`,
         `Frequency.of.tiredness.lethargy.in.last.2.weeks`,
         `Illness.injury.bereavement.stress.in.last.2.years`,
         `APOE4`,
         `Has.Date`,
         `Had_Dementia`)
         
colSums(is.na(Regression_columns))

imputation_columns <- Regression_columns %>%
  select(`Participant.ID`,
         `Townsend.deprivation.index.at.recruitment`,
         `Summed.MET.minutes.per.week.for.all.activity...Instance.0`,
         `Number.of.treatments.medications.taken...Instance.0`,
         `QualScore`,
         `Number.of.live.births`)
         
Regression_columns$
install.packages("mice")
library(mice)

imputed_data <- mice(imputation_columns, m=1, method='pmm', maxit=5, seed=500, printFlag=FALSE)
summary(imputed_data)

completed_data <- complete(imputed_data, action = 1)        
colSums(is.na(completed_data))

Regression_columns <- Regression_columns  %>%
  select(-'Townsend.deprivation.index.at.recruitment',
         -'Summed.MET.minutes.per.week.for.all.activity...Instance.0',
         -'Number.of.treatments.medications.taken...Instance.0',
         -'QualScore',
         -'Number.of.live.births')

Regression_columns = left_join(Regression_columns, completed_data, by = "Participant.ID")

Regression_variables <- Regression_columns %>%
  select(`Participant.ID`,
         `Year.of.birth`,
         `Townsend.deprivation.index.at.recruitment`,
         `Ethnic.background...Instance.0`,
         `Summed.MET.minutes.per.week.for.all.activity...Instance.0`,
         `BMI`,
         `Sleep.duration...Instance.0`,
         `Sleeplessness...insomnia...Instance.0`,
         `Number.of.treatments.medications.taken...Instance.0`,
         `Vascular_problem_binary`,
         `Diabetes_binary`,
         `Ever.had.rheumatoid.arthritis.affecting.one.or.more.joints`,
         `Ever.had.osteoarthritis.affecting.one.or.more.joints..e.g..hip..knee..shoulder.`,
         `Cancer.diagnosed.by.doctor...Instance.0`,
         `Age.at.recruitment`,
         `QualScore`,
         `SmokingBaseline`,
         `AlcoholBaseline`,
         `DietScore`,
         `Number.of.live.births`,
         `mergedAge`,
         `Oophorectomy_Occurred`,
         `HRT_Used`,
         `Contraceptive_Used`,
         `Vitamin_or_Supplement_User`,
         `LOE`,
         `Frequency.of.tiredness.lethargy.in.last.2.weeks`,
         `Illness.injury.bereavement.stress.in.last.2.years`,
         `APOE4`,
         `Has.Date`,
         `Had_Dementia`)

#MET column
library(dplyr)
Regression_variables <- Regression_variables %>%
  mutate(MET_category = case_when(
    Summed.MET.minutes.per.week.for.all.activity...Instance.0 < 800 ~ "low",
    Summed.MET.minutes.per.week.for.all.activity...Instance.0 >= 800 & Summed.MET.minutes.per.week.for.all.activity...Instance.0 < 2400 ~ "moderate",
    Summed.MET.minutes.per.week.for.all.activity...Instance.0 >= 2400 ~ "high"
  ))


#Adding other variables:
subset_data <- women_table %>%
  select(`Participant.ID`,
         `Sex`,
         `DietScore_binary`,
         `dementia_time_distance2`)

Regression2 = left_join(Regression_variables, subset_data, by = "Participant.ID")

#Statin and beta blocker columns
library(dplyr)
statin_user <- statin_user_participant0 %>%
  rename(Participant.ID = "Participant ID")
Regression2 = left_join(Regression2, statin_user, by = "Participant.ID")
Regression2 <- Regression2 %>%
  rowwise() %>%
  mutate(Statin_user = ifelse(any(!is.na(c(`Treatment/medication code | Instance 0`,
                                           `Treatment/medication code | Instance 1`,
                                           `Treatment/medication code | Instance 2`,
                                           `Treatment/medication code | Instance 3`))),
                              "Y", "N"))
Regression2 <- ungroup(Regression2)
response_counts <- table(Regression2$Statin_user)
print(response_counts)

Regression2 <- Regression2  %>%
  select(-'Treatment/medication code | Instance 3',
         -'Treatment/medication code | Instance 2',
         -'Treatment/medication code | Instance 1',
         -'Treatment/medication code | Instance 0')

colSums(is.na(Regression2))
betablocker_user <- betablocker_user_participant %>%
  rename(Participant.ID = "Participant ID")
Regression2 = left_join(Regression2, betablocker_user, by = "Participant.ID")
Regression2 <- Regression2 %>%
  rowwise() %>%
  mutate(Betablocker_user = ifelse(any(!is.na(c(`Treatment/medication code | Instance 0`,
                                                `Treatment/medication code | Instance 1`,
                                                `Treatment/medication code | Instance 2`,
                                                `Treatment/medication code | Instance 3`))),
                                   "Y", "N"))
Regression2 <- ungroup(Regression2)

Regression2 <- Regression2  %>%
  select(-'Treatment/medication code | Instance 3',
         -'Treatment/medication code | Instance 2',
         -'Treatment/medication code | Instance 1',
         -'Treatment/medication code | Instance 0')

#Illness/bereavement column
response_counts <- table(Regression2$Illness.injury.bereavement.stress.in.last.2.years)
print(response_counts)

library(dplyr)
Regression2 <- Regression2 %>%
  mutate(Illness.injury.bereavement.stress.in.last.2.years = ifelse(
    Illness.injury.bereavement.stress.in.last.2.years %in% c('None of the above', 'NA', 'Prefer not to answer') | 
      is.na(Illness.injury.bereavement.stress.in.last.2.years),
    'No',
    Illness.injury.bereavement.stress.in.last.2.years
  ))

colSums(is.na(Regression2))

#changing to a count of stressful events
count_stressful_events <- function(events) {
  if (is.na(events) || events == "") {
    return(0)
  }
  return(length(strsplit(events, "\\|")[[1]]))
}
Regression3 <- Regression2 %>%
  mutate(number_of_stressful_events = sapply((Illness.injury.bereavement.stress.in.last.2.years), count_stressful_events))

#remove original column
library(dplyr)
Regression3 <- Regression3  %>%
  select(-"Illness.injury.bereavement.stress.in.last.2.years")
#changes needed for certain variables
#Smoking: very high association with Prefer not to answer (122), look into participants
#Alcohol: change Prefer not to answer (23) to No? 
#         Look into unexpected association with never
#cancer: change Yes - you will be asked about this later by an interviewer to Yes
#        change Do not know (150) and Prefer not to answer (14) to No?


library(dplyr)
Regression3 <- Regression3 %>%
  mutate(Cancer.diagnosed.by.doctor...Instance.0 = ifelse(Cancer.diagnosed.by.doctor...Instance.0 == "Yes - you will be asked about this later by an interviewer", "Yes", Cancer.diagnosed.by.doctor...Instance.0))

Regression3 <- Regression3 %>%
  rename(Depression_diagnosis = "Has.Date")

library(dplyr)
Regression5 <- Regression5  %>%
  select(-"...1",
         -"...2",
         -"...3")

#HRT and contra columns
HRT_Contra_ages_participant <- HRT_Contra_ages_participant %>%
  rename(Participant.ID = "Participant ID")
Regression3 = left_join(Regression3, HRT_Contra_ages_participant, by = "Participant.ID")

Regression4 <- Regression3 %>%
  mutate(across(starts_with("Age last used hormone-replacement therapy (HRT)"), 
                ~replace(., . == "Still taking HRT", 999))) %>%
  mutate(across(starts_with("Age last used hormone-replacement therapy (HRT)"), as.numeric))

Regression4 <- Regression4 %>%
  mutate(across(starts_with("Age started hormone-replacement therapy (HRT)"), as.numeric))

# Function creation for still using section
calculate_finished_HRT <- function(...) {
  ages <- c(...)
  valid_ages <- ages[!is.na(ages) & ages != 999]
  max_age <- ifelse(length(valid_ages) > 0, max(valid_ages), NA)
  still_using_index <- which(ages == 999)
  max_age_index <- ifelse(length(valid_ages) > 0, which(ages == max_age), NA)
  
  if (length(still_using_index) > 0) {
    latest_still_using_index <- max(still_using_index)
    if (is.na(max_age) || latest_still_using_index > max_age_index) {
      return(999)
    } else {
      return(max_age)
    }
  } else {
    return(max_age)
  }
}

Regression4 <- Regression4 %>%
  rowwise() %>%
  mutate(
    started_HRT = min(c_across(starts_with("Age started hormone-replacement therapy (HRT)")), na.rm = TRUE),
    finished_HRT = calculate_finished_HRT(c_across(starts_with("Age last used hormone-replacement therapy (HRT)")))
  ) %>%
  ungroup()

Regression4 <- Regression4 %>%
  mutate(started_HRT = if_else(is.infinite(started_HRT), NA_real_, started_HRT))

#same for contraception
Regression5 <- Regression4 %>%
  mutate(across(starts_with("Age when last used oral contraceptive pill"), 
                ~replace(., . == "Still taking the pill", 999))) %>%
  mutate(across(starts_with("Age when last used oral contraceptive pill"), as.numeric))

Regression5 <- Regression5 %>%
  mutate(across(starts_with("Age started oral contraceptive pill"), as.numeric))

# Function creation for still using section
calculate_finished_Pill <- function(...) {
  ages <- c(...)
  valid_ages <- ages[!is.na(ages) & ages != 999]
  max_age <- ifelse(length(valid_ages) > 0, max(valid_ages), NA)
  still_using_index <- which(ages == 999)
  max_age_index <- ifelse(length(valid_ages) > 0, which(ages == max_age), NA)
  
  if (length(still_using_index) > 0) {
    latest_still_using_index <- max(still_using_index)
    if (is.na(max_age) || latest_still_using_index > max_age_index) {
      return(999)
    } else {
      return(max_age)
    }
  } else {
    return(max_age)
  }
}

Regression5 <- Regression5 %>%
  rowwise() %>%
  mutate(
    started_pill = min(c_across(starts_with("Age started oral contraceptive pill")), na.rm = TRUE),
    finished_pill = calculate_finished_Pill(c_across(starts_with("Age when last used oral contraceptive pill")))
  ) %>%
  ungroup()

Regression5 <- Regression5 %>%
  mutate(started_pill = if_else(is.infinite(started_pill), NA_real_, started_pill))

# Create the yrs_on_HRT column
Regression5 <- Regression5 %>%
  mutate(yrs_on_HRT = if_else(finished_HRT == 999, "still_using", as.character(finished_HRT - started_HRT)))

Regression5 <- Regression5 %>%
  mutate(yrs_on_pill = if_else(finished_pill == 999, "still_using", as.character(finished_pill - started_pill)))

#delete columns
keyword <- "oral"
matching_columns <- grep(keyword, names(Regression5), value = TRUE)
print(matching_columns)

library(dplyr)
Regression5 <- Regression5  %>%
  select(-"Age started hormone-replacement therapy (HRT) | Instance 0",
         -"Age started hormone-replacement therapy (HRT) | Instance 1",
         -"Age started hormone-replacement therapy (HRT) | Instance 2",
         -"Age started hormone-replacement therapy (HRT) | Instance 3",
         -"Age last used hormone-replacement therapy (HRT) | Instance 0",
         -"Age last used hormone-replacement therapy (HRT) | Instance 1",
         -"Age last used hormone-replacement therapy (HRT) | Instance 2",
         -"Age last used hormone-replacement therapy (HRT) | Instance 3",
         -"Age started oral contraceptive pill | Instance 0",
         -"Age started oral contraceptive pill | Instance 2",
         -"Age started oral contraceptive pill | Instance 1",
         -"Age started oral contraceptive pill | Instance 3",
         -"Age when last used oral contraceptive pill | Instance 0",
         -"Age when last used oral contraceptive pill | Instance 1",
         -"Age when last used oral contraceptive pill | Instance 2",
         -"Age when last used oral contraceptive pill | Instance 3")

#Removing outliers messing up the cox
removed_participants <- Regression6$Participant.ID[Regression6$SmokingBaseline == 'Prefer not to answer']
Regression7 <- Regression6[Regression6$SmokingBaseline != 'Prefer not to answer', ]
print(removed_participants)
#removed smoking participants - 1082133, 1118345, 1193582, 1244246, 1324617, 1358756, 1396087, 1446991, 1454853, 1506408, 1523732
# 1602389, 1809073, 1821677, 1854375, 1972466, 2007889, 2024211, 2041232, 2062039, 2098910, 2149372,
# 2173974, 2188385, 2210717, 2292807, 2297568, 2369862, 2372338, 2376733, 2393308, 2532618, 2544333,
# 2550031, 2595940, 2658427, 2710997, 2736714, 2798149, 2877554, 3014229, 3018593, 3019632, 3044489,
# 3063720, 3072392, 3078367, 3121343, 3178458, 3178915, 3208220, 3217262, 3347288, 3369296, 3427511,
# 3456164, 3472884, 3546575, 3599656, 3631922, 3648892, 3658654, 3748195, 3767689, 3771718, 3879295,
# 3920079, 3933540, 4070786, 4143394, 4158809, 4226970, 4277040, 4324901, 4361578, 4368392, 4403441,
# 4427029, 4438265, 4461197, 4485502, 4640534, 4710676, 4745340, 4777320, 4816857, 4828298, 4864117,
# 4889155, 4919939, 4957220, 4979054, 5013679, 5059012, 5063207, 5100809, 5132371, 5172910, 5185934,
# 5201240, 5238799, 5273729, 5333993, 5340204, 5475092, 5508495, 5520593, 5521892, 5551592, 5573053,
# 5599029, 5600511, 5673180, 5693777, 5722292, 5748345, 5753920, 5814835, 5878719, 5966815, 5999693, 6010771
removed_participants <- Regression7$Participant.ID[Regression7$AlcoholBaseline == 'Prefer not to answer']
Regression7 <- Regression7[Regression7$AlcoholBaseline != 'Prefer not to answer', ]
print(removed_participants)
# removed alcohol ppts: 1020440, 1062481, 1226002, 1381827, 1476412, 1488343, 2681796, 2763318, 3391934, 3411134, 3661732,
# 3870048, 4071199, 4332463, 4417474, 4567401, 5053071, 5482477, 5592040, 5633609, 5721182, 5804286,
removed_participants <- Regression7$Participant.ID[Regression7$Cancer.diagnosed.by.doctor...Instance.0 == 'Prefer not to answer']
Regression7 <- Regression7[Regression7$Cancer.diagnosed.by.doctor...Instance.0 != 'Prefer not to answer', ]
print(removed_participants)
#removed cancer ppts: 1062138, 1365390, 2177351, 2236394, 2243993, 2672050, 4063704, 4369356, 5195804, 5287775, 5418701, 5524027, 5711105
removed_participants <- Regression7$Participant.ID[Regression7$Ever.had.rheumatoid.arthritis.affecting.one.or.more.joints == 'Prefer not to answer']
Regression7 <- Regression7[Regression7$Ever.had.rheumatoid.arthritis.affecting.one.or.more.joints != 'Prefer not to answer', ]
print(removed_participants)
#removed rheumatoid ppts:1146032, 1586384, 2667042, 2712207
removed_participants <- Regression7$Participant.ID[Regression7$Ever.had.osteoarthritis.affecting.one.or.more.joints..e.g..hip..knee..shoulder. == 'Prefer not to answer']
Regression7 <- Regression7[Regression7$Ever.had.osteoarthritis.affecting.one.or.more.joints..e.g..hip..knee..shoulder. != 'Prefer not to answer', ]
print(removed_participants)
#removed osteo ppts: 2588207, 3425533, 5231735, 5891021
# Replace 'Do not know' with 'No'
Regression7 <- Regression7 %>%
  mutate(`Ever.had.osteoarthritis.affecting.one.or.more.joints..e.g..hip..knee..shoulder.` = recode(
    `Ever.had.osteoarthritis.affecting.one.or.more.joints..e.g..hip..knee..shoulder.`,
    'Do not know' = 'No'
  ))
Regression7 <- Regression7 %>%
  mutate(`Ever.had.rheumatoid.arthritis.affecting.one.or.more.joints` = recode(
    `Ever.had.rheumatoid.arthritis.affecting.one.or.more.joints`,
    'Do not know' = 'No'
  ))
removed_participants <- Regression7$Participant.ID[Regression7$Sleeplessness...insomnia...Instance.0 == 'Prefer not to answer']
Regression7 <- Regression7[Regression7$Sleeplessness...insomnia...Instance.0 != 'Prefer not to answer', ]
print(removed_participants)
#removed insomnia ppts: 1037225, 1037253, 1076677, 1274243, 1399154, 1535831, 2508318, 2891276, 2914897, 3428102, 3572115,
# 3767630, 3790245, 3791346, 3813086, 4019979, 4254813, 4558506, 4947402, 5359636, 5864952, 5900053, 5912482
#cox
install.packages("gtsummary")
library(gtsummary)
install.packages("survival")
library(survival)
library(dplyr)

cox_model <- coxph(Surv(dementia_time_distance2, Had_Dementia) ~ LOE + Contraceptive_Used + HRT_Used + Oophorectomy_Occurred + APOE4 + BMI + DietScore_binary + Vitamin_or_Supplement_User + Vascular_problem_binary + Diabetes_binary + QualScore + MET_category + SmokingBaseline + AlcoholBaseline + Cancer.diagnosed.by.doctor...Instance.0 + Statin_user + Betablocker_user + Depression_diagnosis + Ever.had.rheumatoid.arthritis.affecting.one.or.more.joints + Ever.had.osteoarthritis.affecting.one.or.more.joints..e.g..hip..knee..shoulder. + number_of_stressful_events + Sleeplessness...insomnia...Instance.0 + Townsend.deprivation.index.at.recruitment + Number.of.treatments.medications.taken...Instance.0 + Number.of.live.births, data = Regression7)

cox_model <- coxph(Surv(dementia_time_distance2, Had_Dementia) ~ LOE + Contraceptive_Used + HRT_Used + Oophorectomy_Occurred + APOE4 + BMI + DietScore_binary + Vitamin_or_Supplement_User + Vascular_problem_binary:Statin_user:Betablocker_user + Diabetes_binary + QualScore + MET_category + SmokingBaseline + AlcoholBaseline + Cancer.diagnosed.by.doctor...Instance.0 + Depression_diagnosis + Ever.had.rheumatoid.arthritis.affecting.one.or.more.joints + Ever.had.osteoarthritis.affecting.one.or.more.joints..e.g..hip..knee..shoulder. + number_of_stressful_events + Sleeplessness...insomnia...Instance.0 + Townsend.deprivation.index.at.recruitment + Number.of.treatments.medications.taken...Instance.0 + Number.of.live.births, data = Regression7)

tbl_regression(cox_model, exponentiate=TRUE)

summary(cox_model)

#Frequency.of.tiredness.lethargy.in.last.2.weeks, Sleep.duration...Instance.0, Ethnic.background...Instance.0, yrs_on_HRT, yrs_on_pill
response_counts <- table(Regression7$Ethnic.background...Instance.0)
print(response_counts)

colSums(is.na(Regression5))

#ression table can be improved to be publication ready (will need variables amended accoridngly)
mean_year_of_birth <- mean(women_table$Year.of.birth, na.rm = TRUE)
mean_year_of_birth
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



