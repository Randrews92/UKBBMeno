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

# Next a Women-physical comorbidites cohort

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
