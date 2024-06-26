install.packages("gtsummary")
install.packages("broom.mixed")
library(gtsummary)
library(broom.mixed)
library(dplyr)

#Get outcomes from key_outcomes_with_death and baseline from lifestyle_table_new

outcomes <- read.csv('Key_outcomes_table_withdeath.csv')

baseline <- read.csv('Lifestyle_table_new.csv')

### Merge bilat/menopause ages so we have just one menopause age column 
# If data is entered in both just use the bilat oopherectomy data if it's smaller than meno age data:
#Amend this code below to include correct variable/ table names

baseline$mergedAge <- ifelse(!is.na(baseline$bilateral_oophorectomyAge) & baseline$bilateral_oophorectomyAge < baseline$menopauseAge, baseline$bilateral_oophorectomyAge, baseline$menopauseAge)

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

# Future steps: Cox proprotional hazard regression

# Future steps: we'll look at matching men and women, and look at controlling for APO-E.

meno_dementia_new <- read.csv('meno_dementia_new.csv')
Death_table_participant <- read.csv('Death_table_participant.csv')


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


# Next steps- apply the date code above to the men-only 

men_only_regression <- read.csv('men_only_regression.csv')

men_only_regression$Death_date <- as.Date(men_only_regression$Death_date)
class(men_only_regression$Death_date)

men_only_regression$dementia_diagnosis <- as.Date(men_only_regression$dementia_diagnosis)
class(men_only_regression$dementia_diagnosis)

men_only_regression <- men_only_regression %>%
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

## Psych Covariates: 

# We need to add the following to male and female datasets: 
# Frequency of tiredness/ lethargy (instance 0)
# Illness injury bereavement last 2 years (instance 0)
# Neuroticism score 

## Merging & Saving sets

# We need to merge the male and female datasets to create a 'both' cohort.
# We then need to save all 3 of the individual data tables of the men/women/both cohorts into the UKBB project

## Cox regression analysis: 

# Once we have the new columns we can conduct a Cox regression exploring dementia in women.

# These are the variables we will need in the regression formula:

# Outcomes: Dementia Y/N, distance
# Reproductive: Age at last live birth, Total number of children, LOE, contraceptive pill, HRT, oopherectomy, mergedAge
# Lifestyle: Exercise, Alcohol, Smoking, Supplement use, BMI, sleep duration, Diet, frequency of tirdness/lethargy
# Covariates: Deprivation, Ethnicity, Age at recruitment, Qualifications
# Medical Covariates: number of meds taken, heart problems, diabetes, cholesterol, cancer, osteoarthritus, rhuematoid arthritus
# Psych covariates: trauma (illness injury bereavement), neuroticism score

# Some of these variables may need some additional prepping/ sanitation 

# Regression formula (will need to add variables accordingly)

cox_model <- coxph(Surv(distance, Had_Dementia) ~ ageAtbirth + LOE + ..., data = meno_death)

# GtSummary tables 

tbl_regression(cox_model, exponentiate=TRUE)

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




