
install.packages("gtsummary")
install.packages("broom.mixed")
library(gtsummary)
library(broom.mixed)
library(data.table)
install.packages("tidyverse")
library(tidyverse)




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


# Create binary demntia column (include death reason in there)
# Create age at dementia column 

# Create age at death 


# Create date of event (dementia first, then death, or last dementia diagnosis in UKBB-2023-01-01)

# Create age at event

# Remove any minus ages (as suggest dementia/ death occurred before UKBB)

# Use age at assessment and age at event to create time to event 

# Fill in NAs in LOE using age at assessment-age at menarche (????)

# Fill in blanks in using age at assessment in the NA values 

