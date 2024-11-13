
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

#To Do:
# Comorbidities (physical, reproductive, sleep, psychiatric and cognitive).
#Age at recruitment/ age at assessment. 
#Deprivation
#Hysterectomy and other gynae surgeries.
#Supplements and drug names (including contraceptive type and HRT type).
#Contraceptive/ HRT use durations. 
#Pregnancies/ Live births/ fertility
#Cognitive, Psychiatric, Sleep symptoms.
#Trauma history

# First create a Women-reproductive factors cohort 

# Next a Women-physical comorbidities cohort

# Women-psych comorbidities 

# Women- sleep

# Women-dementia outcomes (plus hearing and pregnancy)

# Women-APOE

# Women- demographics and other health factors


##### Fixing the tables

# Women-reproductive

reproductive <- read.csv('Women_reproductive_participant.csv')

reproductive[reproductive == ""] <- NA

# Lets remove all 'Do not know' and 'Prefer not to answer' values

# Remove rows where any column contains 'Do not know' or 'Prefer not to answer'
reproductive<- reproductive[!apply(reproductive, 1, 
                        function(row) 
                          any(row %in% c('Do not know', 
                                         'Prefer not to answer'))), ]


na_counts <- data.frame(Column = names(reproductive ), 
                        NA_Count = colSums(is.na(reproductive)))
na_counts


reproductive%>%
  group_by(Ever.had.hysterectomy..womb.removed....Instance.0)%>%
  summarise(count=n_distinct(Participant.ID))

reproductive%>%
  group_by(Had.menopause...Instance.0)%>%
  summarise(count=n_distinct(Participant.ID))


unique(reproductive$Ever.had.hysterectomy..womb.removed....Instance.0)
# Lets create the age at menopause variable 

reproductive$Age.at.menopause <- ifelse(
  !is.na(reproductive$Age.at.bilateral.oophorectomy..both.ovaries.removed....Instance.0) 
  & !is.na(reproductive$Age.at.menopause..last.menstrual.period....Instance.0),
  pmin(reproductive$Age.at.bilateral.oophorectomy..both.ovaries.removed....Instance.0, 
       reproductive$Age.at.menopause..last.menstrual.period....Instance.0, na.rm = TRUE),
  ifelse(!is.na(reproductive$Age.at.bilateral.oophorectomy..both.ovaries.removed....Instance.0), 
         reproductive$Age.at.bilateral.oophorectomy..both.ovaries.removed....Instance.0, reproductive$Age.at.menopause..last.menstrual.period....Instance.0)
)


reproductive$Age.at.menopause <- ifelse(
  !is.na(reproductive$Age.at.hysterectomy...Instance.0) 
  & !is.na(reproductive$Age.at.menopause),
  pmin(reproductive$Age.at.hysterectomy...Instance.0, 
       reproductive$Age.at.menopause, na.rm = TRUE),
  ifelse(!is.na(reproductive$Age.at.hysterectomy...Instance.0), 
         reproductive$Age.at.hysterectomy...Instance.0, reproductive$Age.at.menopause)
)


na_count <- sum(is.na(reproductive$Age.at.menopause))
na_count <- sum(is.na(reproductive$Had.menopause))
# Print the result
na_count

# Now create LOE

reproductive$Age.at.menopause.num <- as.numeric(reproductive$Age.at.menopause)

reproductive$Age.at.menarche.num <- as.numeric(reproductive$Age.when.periods.started..menarche....Instance.0)

reproductive$LOE <- reproductive$Age.at.menopause.num-reproductive$Age.at.menarche.num

# Years on HRT 

reproductive$Age.last.used.HRT.num <- as.numeric(reproductive$Age.last.used.hormone.replacement.therapy..HRT....Instance.0)

reproductive$Age.first.used.HRT.num <- as.numeric(reproductive$Age.started.hormone.replacement.therapy..HRT....Instance.0)

reproductive$Yrs_HRT <- (reproductive$Age.last.used.HRT.num-reproductive$Age.first.used.HRT.num)+1

reproductive$Yrs_HRT <- replace(reproductive$Yrs_HRT, is.na(reproductive$Yrs_HRT), 0)

# Years on OC

reproductive$Age.last.used.OC.num <- as.numeric(reproductive$Age.when.last.used.oral.contraceptive.pill...Instance.0)

reproductive$Age.first.used.OC.num <- as.numeric(reproductive$Age.started.oral.contraceptive.pill...Instance.0)

reproductive$Yrs_OC <- (reproductive$Age.last.used.OC.num-reproductive$Age.first.used.OC.num)+1

reproductive$Yrs_OC <- replace(reproductive$Yrs_OC, is.na(reproductive$Yrs_OC), 0)

# Ever had endo 

reproductive <- reproductive%>%
  mutate(Had_endometriosis = ifelse(is.na(Date.N80.first.reported..endometriosis.), 'N', 'Y'))

# Ever infertile 

reproductive <- reproductive%>%
  mutate(Diagnosed_infertility = ifelse(is.na(Date.N97.first.reported..female.infertility.), 'N', 'Y'))

# Regular menstrual cycle was defined as cycle length with 22 to 34 days. Cycle length was categorized as ≤21, 22 to 27, 28 to 34, and ≥35 days or too irregular. https://www.ahajournals.org/doi/10.1161/JAHA.122.029020 

reproductive<- reproductive %>%
  mutate(menstrual_cycle = case_when(
    Length.of.menstrual.cycle...Instance.0  == 'Irregular cycle' ~ 'Irregular cycle',
    Length.of.menstrual.cycle...Instance.0 <= 21 ~ '≤21 days',
    Length.of.menstrual.cycle...Instance.0 >= 22 & Length.of.menstrual.cycle...Instance.0  <= 27 ~ '22 to 27 days',
    Length.of.menstrual.cycle...Instance.0  >= 28 & Length.of.menstrual.cycle...Instance.0  <= 34 ~ '28 to 34 days',
    Length.of.menstrual.cycle...Instance.0  >= 35 ~ '≥35 days',
    TRUE ~ NA_character_  # Optional: NA for any other values that don't match
  ))


# Let's remove NAs on certain variables 


na_counts <- data.frame(Column = names(reproductive ), 
                        NA_Count = colSums(is.na(reproductive)))
na_counts


reproductive <- reproductive%>%
  filter(!is.na(Number.of.live.births...Instance.0 ))


na_counts <- data.frame(Column = names(reproductive ), 
                        NA_Count = colSums(is.na(reproductive)))
na_counts

# Now lets create a subset with just the variables we need for the regression:

cat(colnames(reproductive), sep = "\n")

reproductive_reg <- reproductive %>%
  select(Participant.ID,
         Had.menopause...Instance.0,
         Time.since.last.menstrual.period...Instance.0,
         Number.of.live.births...Instance.0,
         Ever.taken.oral.contraceptive.pill...Instance.0,
         Ever.used.hormone.replacement.therapy..HRT....Instance.0,
         Ever.had.hysterectomy..womb.removed....Instance.0,
         Bilateral.oophorectomy..both.ovaries.removed....Instance.0,
         Ever.had.stillbirth..spontaneous.miscarriage.or.termination...Instance.0,
         Number.of.stillbirths...Instance.0,
         Age.at.menopause.num,
         Age.at.menarche.num,
         LOE,
         menstrual_cycle,
         Age.last.used.HRT.num,
         Age.first.used.HRT.num,
         Yrs_HRT,
         Age.last.used.OC.num,
         Age.first.used.OC.num,
         Yrs_OC,
         Had_endometriosis,
         Diagnosed_infertility
         
         )

write.csv(reproductive_reg,'reproductive_reg.csv')

# Now dx upload reproductive_reg to Project 

# Use dx download reproductive_reg.csv to pick up where left off :) 

reproductive_reg <- read.csv('reproductive_reg.csv')


##### Fixing the tables

# Women-physical

physical <- read.csv('Women_physical_participant.csv')

physical[physical == ""] <- NA

# Lets get heart problems into binary columns 

# Again get rid of 'Do not know' and 'prefer not to answer'
physical<- physical[!apply(physical, 1, 
                                   function(row) 
                                     any(row %in% c('Do not know', 
                                                    'Prefer not to answer'))), ]




na_counts <- data.frame(Column = names(physical ), 
                        NA_Count = colSums(is.na(physical)))
na_counts

unique(physical$Vascular.heart.problems.diagnosed.by.doctor...Instance.0)

# Define the list of conditions to create binary columns for
conditions <- c("High blood pressure", "Angina", "Heart attack", "Stroke")



# Use mutate with lapply to create binary columns for each condition
physical <- physical %>%
  mutate(
    High_blood_pressure = ifelse(str_detect(Vascular.heart.problems.diagnosed.by.doctor...Instance.0, "High blood pressure"), "Y", "N"),
    Angina = ifelse(str_detect(Vascular.heart.problems.diagnosed.by.doctor...Instance.0, "Angina"), "Y", "N"),
    Heart_attack = ifelse(str_detect(Vascular.heart.problems.diagnosed.by.doctor...Instance.0, "Heart attack"), "Y", "N"),
    Stroke = ifelse(str_detect(Vascular.heart.problems.diagnosed.by.doctor...Instance.0, "Stroke"), "Y", "N")
  )

# Lets do the same for pain 

unique(physical$Pain.type.s..experienced.in.last.month...Instance.0)

library(dplyr)
library(stringr)

# Add binary columns for each pain type
physical <- physical %>%
  mutate(
    Back_pain = ifelse(str_detect(Pain.type.s..experienced.in.last.month...Instance.0, "Back pain"), "Y", "N"),
    Hip_pain = ifelse(str_detect(Pain.type.s..experienced.in.last.month...Instance.0, "Hip pain"), "Y", "N"),
    Knee_pain = ifelse(str_detect(Pain.type.s..experienced.in.last.month...Instance.0, "Knee pain"), "Y", "N"),
    Headache = ifelse(str_detect(Pain.type.s..experienced.in.last.month...Instance.0, "Headache"), "Y", "N"),
    Neck_or_shoulder_pain = ifelse(str_detect(Pain.type.s..experienced.in.last.month...Instance.0, "Neck or shoulder pain"), "Y", "N"),
    Stomach_or_abdominal_pain = ifelse(str_detect(Pain.type.s..experienced.in.last.month...Instance.0, "Stomach or abdominal pain"), "Y", "N"),
    Facial_pain= ifelse(str_detect(Pain.type.s..experienced.in.last.month...Instance.0, "Facial pain"), "Y", "N"),
    Pain_all_over_body= ifelse(str_detect(Pain.type.s..experienced.in.last.month...Instance.0, "Pain all over the body"), "Y", "N")
  )

# View the updated data frame
head(physical)

# For diabetes we need to change Y to N's if they only had gestational (following this method: https://diabetesjournals.org/care/article/40/12/1710/36905/Associations-Between-Diabetes-and-Both)
 unique(physical$Diabetes.diagnosed.by.doctor...Instance.0)
 unique(physical$Gestational.diabetes.only...Instance.0)
 
 # Modify the 'Diabetes.diagnosed.by.doctor...Instance.0' column based on 'Gestational.diabetes.only...Instance.0'
 physical <- physical %>%
   mutate(
     Diabetes.diagnosed.by.doctor...Instance.0 = ifelse(
       Gestational.diabetes.only...Instance.0 == "Yes" & 
         Diabetes.diagnosed.by.doctor...Instance.0 == "Yes", 
       "No", 
       Diabetes.diagnosed.by.doctor...Instance.0
     )
   )
 
# Now let's remove NA's based on one column (as NAs here account for NAs across all data) 
 
 physical <- physical%>%
   filter(!is.na(Angina))
 
 
 na_counts <- data.frame(Column = names(physical ), 
                         NA_Count = colSums(is.na(physical)))
 na_counts

# Now we have all variables we need lets create the subset for the regression

cat(colnames(physical), sep = "\n")

physical_reg <- physical %>%
  select(Participant.ID,
         Overall.health.rating...Instance.0,
         Long.standing.illness..disability.or.infirmity...Instance.0,
         Weight.change.compared.with.1.year.ago...Instance.0,
         Number.of.treatments.medications.taken...Instance.0,
         Diabetes.diagnosed.by.doctor...Instance.0,
         High_blood_pressure,
         Angina,
         Heart_attack,
         Stroke,
         Back_pain,
         Hip_pain,
         Knee_pain,
         Headache,
         Neck_or_shoulder_pain,
         Stomach_or_abdominal_pain,
         Facial_pain,
         Pain_all_over_body
  )

write.csv(physical_reg,'physical_reg.csv')

# Now dx upload physical_reg to Project 

# Use dx download physical_reg.csv to pick up where left off :) 

physical_reg <- read.csv('physical_reg.csv')

##### Fixing the tables

# Women-psychological 

psych <- read.csv('Women_psychological_participant.csv')

psych[psych == ""] <- NA

# Lets remove all 'Do not know' and 'Prefer not to answer' values

# Remove rows where any column contains 'Do not know' or 'Prefer not to answer'
psych<- psych[!apply(psych, 1, 
                                   function(row) 
                                     any(row %in% c('Do not know', 
                                                    'Prefer not to answer'))), ]


# check out NAs


na_counts <- data.frame(Column = names(psych ), 
                        NA_Count = colSums(is.na(psych)))
na_counts

# Combine then pivot out trauma 

# Merge the two columns, prioritizing 'Illness..injury..bereavement..stress.in.last.2.years...Instance.0'
# If it's NA, use 'Illness..injury..bereavement..stress.in.last.2.years..pilot....Instance.0'
psych <- psych %>%
  mutate(
    merged_illness_column = coalesce(
      Illness..injury..bereavement..stress.in.last.2.years...Instance.0,
      Illness..injury..bereavement..stress.in.last.2.years..pilot....Instance.0
    )
  )

unique(psych$merged_illness_column)

# List of distinct trauma types to create binary columns
illness_types <- c("Serious illness, injury or assault to yourself",
                   "Serious illness, injury or assault of a close relative",
                   "Death of a spouse or partner",
                   "Death of a close relative",
                   "Financial difficulties",
                   "Marital separation/divorce")

# Create binary columns for each trauma type
# Create binary columns for each trauma type in the 'psych' dataset
psych <- psych %>%
  mutate(
    Serious_illness_injury_assault_to_yourself = ifelse(str_detect(merged_illness_column, "Serious illness, injury or assault to yourself"), "Y", "N"),
    Serious_illness_injury_assault_of_close_relative = ifelse(str_detect(merged_illness_column, "Serious illness, injury or assault of a close relative"), "Y", "N"),
    Death_of_spouse_or_partner = ifelse(str_detect(merged_illness_column, "Death of a spouse or partner"), "Y", "N"),
    Death_of_close_relative = ifelse(str_detect(merged_illness_column, "Death of a close relative"), "Y", "N"),
    Financial_difficulties = ifelse(str_detect(merged_illness_column, "Financial difficulties"), "Y", "N"),
    Marital_separation_divorce = ifelse(str_detect(merged_illness_column, "Marital separation/divorce"), "Y", "N")
  )


# Now let's remove NA's based on one column (as NAs here account for NAs across all data) 

psych <- psych%>%
  filter(!is.na(merged_illness_column))


na_counts <- data.frame(Column = names(psych ), 
                        NA_Count = colSums(is.na(psych)))
na_counts

# Now we have all variables we need lets create the subset for the regression

cat(colnames(psych), sep = "\n")

psych_reg <- psych %>%
  select(Participant.ID,
         Able.to.confide...Instance.0,
         Irritability...Instance.0,
         Miserableness...Instance.0,
         Sensitivity...hurt.feelings...Instance.0,
         Fed.up.feelings...Instance.0,
         Nervous.feelings...Instance.0,
         Worrier...anxious.feelings...Instance.0,
         Tense....highly.strung....Instance.0,
         Loneliness..isolation...Instance.0,
         Frequency.of.depressed.mood.in.last.2.weeks...Instance.0,
         Frequency.of.unenthusiasm...disinterest.in.last.2.weeks...Instance.0,
         Frequency.of.tenseness...restlessness.in.last.2.weeks...Instance.0,
         Frequency.of.tiredness...lethargy.in.last.2.weeks...Instance.0,
         Seen.doctor..GP..for.nerves..anxiety..tension.or.depression...Instance.0,
         merged_illness_column,
         Serious_illness_injury_assault_to_yourself,
         Serious_illness_injury_assault_of_close_relative,
         Death_of_spouse_or_partner,
         Death_of_close_relative,
         Financial_difficulties,
         Marital_separation_divorce
  )

write.csv(psych_reg,'psych_reg.csv')

# Now dx upload psych_reg to Project 

# Use dx download psych_reg.csv to pick up where left off :) 

psych_reg <- read.csv('psych_reg.csv')


##### Fixing the tables

# Women- sleep

sleep <- read.csv('Women_sleep_participant.csv')

sleep[sleep == ""] <- NA

# Lets remove all 'Do not know' and 'Prefer not to answer' values

# Remove rows where any column contains 'Do not know' or 'Prefer not to answer'
sleep<- sleep[!apply(sleep, 1, 
                     function(row) 
                       any(row %in% c('Prefer not to answer'))), ]


# check out NAs

na_counts <- data.frame(Column = names(sleep ), 
                        NA_Count = colSums(is.na(sleep)))
na_counts

# get rid of NAs on Insomnia as they didn't answer anything 

sleep <- sleep%>%
  filter(!is.na(Sleeplessness...insomnia...Instance.0))

na_counts <- data.frame(Column = names(sleep ), 
                        NA_Count = colSums(is.na(sleep)))
na_counts


write.csv(sleep,'sleep_reg.csv')

# Now dx upload sleep_reg.csv to Project 

# Use dx download sleep_reg.csv to pick up where left off :) 

sleep_reg <- read.csv('sleep_reg.csv')


##### Fixing the tables

# Women-cognitive and death

cog <- read.csv('Women_cognitive_outcomes_participant.csv')

cog[cog == ""] <- NA

# Lets remove all 'Do not know' and 'Prefer not to answer' values

# Remove rows where any column contains 'Do not know' or 'Prefer not to answer'
cog<- cog[!apply(cog, 1, 
                     function(row) 
                       any(row %in% c('Do not know', 
                                      'Prefer not to answer'))), ]


# check out NAs


na_counts <- data.frame(Column = names(cog ), 
                        NA_Count = colSums(is.na(cog)))
na_counts

# Let's look at death 

death <- read.csv('Women_cognitive_outcomes_death.csv')


# Let's join the death date column onto the cog data 

cog <- cog %>%
  left_join(death %>%
              select(Participant.ID, Date.of.death), 
            by = "Participant.ID")


# Create subset 

unique(cog$Present.in.OMOP.dataset)

cog_reg <- cog %>%
  select(-Present.in.OMOP.dataset)


na_counts <- data.frame(Column = names(cog_reg ), 
                        NA_Count = colSums(is.na(cog_reg)))
na_counts

write.csv(cog_reg,'cognitive_outcomes_reg.csv')

# Now dx upload cognitive_outcomes_reg.csv to Project 

# Use dx download cog_reg.csv to pick up where left off :) 

cog_reg <- read.csv('cognitive_outcomes_reg.csv')


##### Fixing the tables

# Women-demographics

demo <- read.csv('Women_demographics_participant.csv')

demo[demo == ""] <- NA

# Lets remove all 'Do not know' and 'Prefer not to answer' values

# Remove rows where any column contains 'Do not know' or 'Prefer not to answer'
demo<- demo[!apply(demo, 1, 
                     function(row) 
                       any(row %in% c('Do not know', 
                                      'Prefer not to answer'))), ]


# check out NAs


na_counts <- data.frame(Column = names(demo ), 
                        NA_Count = colSums(is.na(demo)))
na_counts


# Look into lost to follow up

unique(demo$Reason.lost.to.follow.up)

demo%>%
  group_by(Date.lost.to.follow.up)%>%
  summarise(count=n_distinct(Participant.ID))


# Now let's remove based on lost to follow up 

demo <- demo%>%
  filter(is.na(Reason.lost.to.follow.up))

na_counts <- data.frame(Column = names(demo ), 
                        NA_Count = colSums(is.na(demo)))
na_counts

unique(demo$Salad...raw.vegetable.intake...Instance.0)

# Merge BMI 

# If it's NA, use 'Body.mass.index..BMI....Instance.0.participant...p23104_i0.'
demo <- demo %>%
  mutate(
    merged_BMI_column = coalesce(
      Body.mass.index..BMI....Instance.0.participant...p21001_i0.,
      Body.mass.index..BMI....Instance.0.participant...p23104_i0.
    )
  )


# Create diet score
demo <- demo %>%
  
  mutate(Salad = ifelse(Salad...raw.vegetable.intake...Instance.0== "Less than one" , 0, Salad...raw.vegetable.intake...Instance.0))

unique(demo$Salad)


demo <- demo %>%
  
  mutate(Veg = ifelse(Cooked.vegetable.intake...Instance.0== "Less than one" , 0, Cooked.vegetable.intake...Instance.0))

unique(demo$Veg)


demo <- demo %>%
  
  mutate(Fresh.Fruit = ifelse(Fresh.fruit.intake...Instance.0== "Less than one" , 0, Fresh.fruit.intake...Instance.0))

unique(demo$Fresh.Fruit)


demo <- demo %>%
  
  mutate(Dried.Fruit = ifelse(Dried.fruit.intake...Instance.0== "Less than one" , 0, Dried.fruit.intake...Instance.0))

unique(demo$Dried.Fruit)

#Ensure all are numeric
demo <- demo %>%
  mutate(
    Fresh.Fruit = as.numeric(Fresh.Fruit),
    Salad = as.numeric(Salad),
    Veg = as.numeric(Veg),
    Dried.Fruit = as.numeric(Dried.Fruit)
  )

# Calculate the Diet Score by summing Fresh, Salad, Cooked, and Dried
demo <- demo %>%
  rowwise() %>%
  mutate(DietScore = sum(Fresh.Fruit, Salad, Veg, Dried.Fruit, na.rm = TRUE))


demo <- demo1
# Create Quals

# Create a Qualifications score with 5 highest and 0 lowest quals

demo$Qualifications <- ifelse(
  grepl("College or University degree", demo$Qualifications...Instance.0), 5,
  ifelse(grepl("A levels/AS levels or equivalent", demo$Qualifications...Instance.0), 4,
         ifelse(grepl("NVQ or HND or HNC or equivalent", demo$Qualifications...Instance.0), 3,
                ifelse(grepl("O levels/GCSEs or equivalent", demo$Qualifications...Instance.0), 2,
                       ifelse(grepl("CSEs or equivalent", demo$Qualifications...Instance.0), 1,
                              ifelse(grepl("Other professional qualifications eg: nursing, teaching", demo$Qualifications...Instance.0), 5,
                                     ifelse(grepl("None of the above", demo$Qualifications...Instance.0), 0, demo$Qualifications...Instance.0)
                              )
                       )
                )
         )
  )
)

# Ethnicity 

unique(demo$Ethnic.background...Instance.0)

t <- demo%>%
  group_by(Ethnic.background...Instance.0)%>%
  summarise(count=n_distinct(Participant.ID))

# Ethnicity Categories:
# White33821
# British471931
# Irish13986
# Any other white background17009
# 
# Mixed264
# White and Black Caribbean648
# White and Black African449
# White and Asian863
# Any other mixed background1077
# 
# Asian or Asian British451
# Indian6104
# Pakistani1901
# Bangladeshi240
# Any other Asian background1853
# 
# Black or Black British301
# Caribbean4600
# African3457
# Any other Black background125
# 
# Chinese1741
# 
# Other ethnic group4876


demo$Ethnicity <- ifelse(grepl("^(Mixed|White and Black Caribbean|White and Black African|White and Asian|Any other mixed background)$", demo$Ethnic.background...Instance.0), 'Mixed',
                         ifelse(grepl("^(White|British|Irish|Any other white background)$", demo$Ethnic.background...Instance.0), 'White',
                                ifelse(grepl("^(Asian or Asian British|Indian|Pakistani|Bangladeshi|Any other Asian background)$", demo$Ethnic.background...Instance.0), "Asian",
                                       ifelse(grepl("^(Black or Black British|Caribbean|African|Any other Black background)$", demo$Ethnic.background...Instance.0), "Black",
                                              ifelse(grepl("^Chinese$", demo$Ethnic.background...Instance.0), "Chinese",
                                                     ifelse(grepl("^Other ethnic group$", demo$Ethnic.background...Instance.0), "Other ethnic group", demo$Ethnic.background...Instance.0)
                                              )
                                       )
                                )
                         )
)



t <- demo%>%
  group_by(Ethnicity, Ethnic.background...Instance.0)%>%
  summarise(count=n_distinct(Participant.ID))


# Attendance.disability.mobility.allowance...Instance.0


unique(demo$Attendance.disability.mobility.allowance...Instance.0)

demo$Disability.benefits <- ifelse(grepl("None of the above", demo$Attendance.disability.mobility.allowance...Instance.0),"N", "Y")


# Now create subset 

na_counts <- data.frame(Column = names(demo ), 
                        NA_Count = colSums(is.na(demo)))
na_counts

# Now let's remove based on lost to follow up 

demo <- demo%>%
  filter(!is.na(Attendance.disability.mobility.allowance...Instance.0))

demo_reg <- demo %>%
  select(Participant.ID,
         Townsend.deprivation.index.at.recruitment,
         Age.when.attended.assessment.centre...Instance.0,
         Ever.smoked...Instance.0,
         Alcohol.drinker.status...Instance.0,
         merged_BMI_column,
         DietScore,
         Qualifications,
         Ethnicity,
         Disability.benefits)
         
         
write.csv(demo_reg,'demo_reg.csv')

# Now dx upload demo_reg.csv to Project 

# Use dx download demo_reg.csv to pick up where left off :) 

demo_reg <- read.csv('demo_reg.csv') 

#### Fixing tables 

# Get APOE

# Download APOE 
# dx download gen_apoe.csv

apoe <- read.csv("gen_apoe.csv")

# Work out APOE status using ifelse statement 

apoe$APOE4 <- ifelse(
  (apoe$rs429358 == "CC" & (apoe$rs7412 == "CC" | apoe$rs7412 == "TC")) | 
    (apoe$rs429358 == "TC" & (apoe$rs7412 == "TC" | apoe$rs7412 == "CC")),
  "Y",
  "N"
)

# Need to make sure the number matches the one indicated by Jennifer Collister: 138,574 

apoe%>%
  group_by(APOE4)%>%
  summarise(count=n_distinct(ID))

# Follow the processes above for joining APOE4 onto the demo table. 
demo_apoe <- merge(demo_reg, apoe[, c("ID", "APOE4")], by.x = "Participant.ID", by.y = "ID", all.x = TRUE)


# We also need to exclude withdrawn particpants- I'll send you over the current list via email. 

# Next steps can be to carry out the Cox regression on the meno only sample (while we're waiting for age-matching script)

###### Now join all datasets on Participant ID 

library(dplyr)

# Join the datasets step by step
fullcog_reg <- demo_apoe %>%
  left_join(reproductive_reg, by = "Participant.ID") %>%
  left_join(physical_reg, by = "Participant.ID") %>%
  left_join(psych_reg, by = "Participant.ID") %>%
  left_join(sleep, by = "Participant.ID") %>%
  left_join(cog_reg, by = "Participant.ID")


# Use dx download fullcog.csv to get the full set

##### Creating cognitive testing columns 

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

##### Prepping menopause table to identify new cases from baseline. 

meno <- read.csv("Women_menopause_participant.csv")

cat(colnames(meno), sep = "\n")

## Perimenopause

# Individuals in this study (https://www.frontiersin.org/journals/dementia/articles/10.3389/frdem.2023.1098693/full) were placed into the following groups: 
# (1) “premenopausal” (women who reported being premenopausal at all timepoints), 
# (2) “perimenopausal” (women who reported being premenopausal at baseline, and postmenopause in subsequent assessments)
# (3) “postmenopausal” (women who reported being postmenopausal at all timepoints).

# used HRT in instance 0,1,2,3 but said No to menopause instance 0, 1, 2, or 3 and didn't have a surgical menopause

# Identify women who were in premenopause, menopause, surgical menopause, perimenopause during instance 0

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


#Work out who transitioned through menopause so we can have a number: 
  
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


### Revisit smallest real difference as doesn't look right: 

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





# Assuming `reliability` is known for your tests (https://pmc.ncbi.nlm.nih.gov/articles/PMC7170235/)
reliability_processing <- 0.55 # example value
reliability_memory <- 0.41 # example value

# Compute SE_diff for each test
SE_diff_processing <- sqrt(2) * SD_within_processing * sqrt(1 - reliability_processing)
SE_diff_memory <- sqrt(2) * SD_within_memory * sqrt(1 - reliability_memory)

# Identify significant declines based on RCI
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

### Lets run regressions to explore how menopause impacts cogntive testing

na_counts <- data.frame(Column = names(fullcog_meno), 
                        NA_Count = colSums(is.na(fullcog_meno)))

cat(apply(na_counts, 1, paste, collapse = ": "), sep = "\n")

cat(colnames(fullcog_meno), sep = "\n")

fullcog_meno$Significant_Decline_Processing<- as.factor(fullcog_meno$Significant_Decline_Processing)
fullcog_meno%>%
  group_by(Hearing.difficulty.problems...Instance.0)%>%
  summarise(count=n_distinct(Participant.ID))

lm_processing <- glm(Significant_Decline_Processing ~ Transition_to_meno+
                       Menopausal_Status_Instance_0+
                 +Age.when.attended.assessment.centre...Instance.0+
                   Mean.time.to.correctly.identify.matches...Instance.0+
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
summary(lm_processing)

fullcog_meno$Significant_Decline_Visual_Memory <- as.factor(fullcog_meno$Significant_Decline_Visual_Memory_R)

lm_memory <- glm(Significant_Decline_Visual_Memory ~ Transition_to_meno+
                       Menopausal_Status_Instance_0+
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

### In BOTH cases going through menopause transition increased risks of signfiant decline.

write.csv(fullcog_meno, "fullcog_meno.csv")

fullcog_meno <- read.csv("fullcog_meno.csv")

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


# Assuming Significant_Decline_Processing, Significant_Decline_Memory, 
# Menopause_Transition, and Dementia_Diagnosis are variables in your dataset.

fullcog_meno$Transition_to_meno <- as.factor(fullcog_meno$Transition_to_meno)
fullcog_meno$Dementia_Diagnosis <- as.factor(fullcog_meno$Dementia_Diagnosis)

# Step 1: Model the effect of cognitive decline on menopause transition (mediator)
model_a <- glm(Transition_to_meno ~
                 Avg_Mean_Time +
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

# Step 2: Model the effect of cognitive decline and menopause transition on dementia diagnosis
model_b <- glm(Dementia_Diagnosis ~ 
                 Avg_Mean_Time + 
                 Transition_to_meno + 
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
                 Stroke + 
                 Avg_Mean_Time:Menopause_Status_Instance_0, 
               data = fullcog_meno, family = binomial)

summary(model_b)

# Step 3: Mediation analysis
mediation_result <- mediate(model_a, model_b, treat = "Avg_Mean_Time",
                            mediator = "Transition_to_meno", boot = TRUE, sims = 1000)

summary(mediation_result)  # Check direct, indirect, and total effects


write.csv(fullcog_meno,"fullcog_meno.csv")

fullcog_meno <- read.csv("fullcog_meno.csv")

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

#### get supplements in there 

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

filtered_meno_transition <- fullcog_meno %>%
  filter(Transition_to_meno == 'Y' | !grepl('Not Sure|Premenopausal', Menopausal_Status_Instance_0))


# Step 2: Model the effect of cognitive decline and menopause transition on dementia diagnosis
model_b <- glm(Dementia_Diagnosis ~ 
                 Avg_Mean_Time+
                 Avg_Pairs_Score+
                 Surgical_Menopause_YN+
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
                 Frequency.of.tiredness...lethargy.in.last.2.weeks...Instance.0+
                 Frequency.of.depressed.mood.in.last.2.weeks...Instance.0+
                 Sleep_duration_num+
                 Long.standing.illness..disability.or.infirmity...Instance.0 + 
                 Serious_illness_injury_assault_to_yourself + 
                 Serious_illness_injury_assault_of_close_relative + 
                 Death_of_spouse_or_partner + 
                 Death_of_close_relative + 
                 Financial_difficulties + 
                 Marital_separation_divorce + 
                 Hearing.difficulty.problems...Instance.0+
                 Seen.doctor..GP..for.nerves..anxiety..tension.or.depression...Instance.0 + 
                 Diabetes.diagnosed.by.doctor...Instance.0 + 
                 High_blood_pressure + 
                 Angina + 
                 Heart_attack + 
                 Stroke
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
               +Loneliness..isolation...Instance.0+
                 Qualifications+ 
               Fish_oil_combined+
                 Zinc+
                 Selenium+
                 #Garlic,
                 #Ginkgo+
                 Iron+
                 Calcium+
                 Glucosamine+
                 `Folic acid or Folate (Vit B9)`+
                 `Multivitamins +/- minerals`+
                 Iron+
                 Calcium+
                 `Vitamin E`+
                 `Vitamin C`+
                 #`Evening primrose oil`+
                 `Vitamin B`+
                 `Vitamin D`+
                 `Vitamin A`,
               data =filtered_meno_transition, family = binomial)

+
  Glucosamine+
  `Folic acid or Folate (Vit B9)`+
  `Multivitamins +/- minerals`+
  Iron+
  Calcium+
  `Vitamin E`+
  `Vitamin C`+
  `Evening primrose oil`+
  `Vitamin B`+
  `Vitamin D`+
  `Vitamin A`+
  Zinc+
  Selenium+
  Garlic+
  Ginkgo
`Evening primrose oil`+

# Create date of event (dementia first, then death, or last update in UKBB-2023-01-01)
summary(model_b)

exp(coef(model_b)["LOE"])

vif(model_b)

tbl_regression(model_b, exponentiate = TRUE)

write.csv(fullcog_meno,"fullcog_meno.csv")




# Create age at event from date of event

# Remove any minus ages (as suggest dementia/ death occurred before UKBB participation)

# Use age at assessment and age at event to create time to event 

# Fill in NAs in LOE using age at assessment-age at menarche (????)


  class(fullcog_meno$Avg_Mean_time_Perimenopausal)
  Avg_Mean_time_Not_Sure+
  Avg_Mean_time_Postmenopausal+
  Avg_Mean_time_Premenopausal+
  Avg_Mean_time_Surgical_Menopause+

