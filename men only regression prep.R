#Making matching table with men for the regression control.

outcomes <- read.csv('Key_outcomes_table_withdeath.csv')

baseline <- read.csv('Lifestyle_table_new.csv')

#Remove columns not linked to men:
keyword <- "contraceptive"
matching_columns <- grep(keyword, names(baseline), value = TRUE)
print(matching_columns)

men_only_regression  <- men_only_regression  %>%
  select(-"...1",
         -"X")

men_only_regression  <- baseline  %>%
  select(-"...1",
         -"...2",
         -"X",
         -"Number.of.live.births",
         -"Age.at.first.live.birth...Instance.0",
         -"Age.at.last.live.birth", 
         -"menopauseAge",
         -"Had.menopause",
         -"menarcheAge",
         -"bilateral_oophorectomyAge",
         -"Ever.used.hormone.replacement.therapy..HRT.",
         -"Ever.taken.oral.contraceptive.pill")

#Add dementia columns
dementia_columns <- outcomes %>%
  select('Participant.ID',
         'Date.F00.first.reported..dementia.in.alzheimer.s.disease.', 
         'Date.F02.first.reported..dementia.in.other.diseases.classified.elsewhere.', 
         'Date.F03.first.reported..unspecified.dementia.',
         'Date.G30.first.reported..alzheimer.s.disease.')
men_only_regression = left_join(men_only_regression, dementia_columns, by = "Participant.ID")

#filter out women
men_only_regression <- men_only_regression %>% 
  filter(Sex == "Male")

#Combine and format dementia columns
men_only_regression$dementia_diagnosis <- NA

men_only_regression$dementia_diagnosis <- pmin(as.Date(men_only_regression$Date.F00.first.reported..dementia.in.alzheimer.s.disease., origin = "1970-01-01"),
                                         as.Date(men_only_regression$Date.F02.first.reported..dementia.in.other.diseases.classified.elsewhere., origin = "1970-01-01"),
                                         as.Date(men_only_regression$Date.F03.first.reported..unspecified.dementia., origin = "1970-01-01"),
                                         as.Date(men_only_regression$Date.G30.first.reported..alzheimer.s.disease., origin = "1970-01-01"),
                                         na.rm = TRUE)

men_only_regression$dementia_diagnosis <- as.Date(men_only_regression$dementia_diagnosis, origin = "1970-01-01")
men_only_regression$Date.of.attending.assessment.centre...Instance.0 <- as.Date(men_only_regression$Date.of.attending.assessment.centre...Instance.0, origin = "1970-01-01")

men_only_regression$dementia_time_distance <- ifelse(is.na(men_only_regression$dementia_diagnosis) | is.na(men_only_regression$Date.of.attending.assessment.centre...Instance.0), 
                                               NA, 
                                               men_only_regression$dementia_diagnosis - men_only_regression$Date.of.attending.assessment.centre...Instance.0)

men_only_regression$Had_Dementia <- "N"
men_only_regression$Had_Dementia <- ifelse(!is.na(men_only_regression$dementia_diagnosis), "Y", "N")

#Adding death columns
death_table <- read.csv('Death_table_participant.csv')

Death_table_participant$Death_date = Death_table_participant$`Date of death | Instance 0`
Death_table_participant$Participant.ID = Death_table_participant$`Participant ID`
men_only_regression <- merge(men_only_regression, Death_table_participant[, c("Participant.ID", "Death_date")], by = "Participant.ID", all.x = TRUE)


#Altering date layout on dementia diagnosis. 

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

#Quality Control - removing outliers
men_only_filtered <- men_only_regression %>%
  filter(Participant.ID != 1671399)
men_only_filtered <- men_only_filtered %>%
  filter(Participant.ID != 3630528)
#potential: 3925392, 3130794

ids_to_remove <- c(2062700, 2542951, 3144677, 3325607, 3486895, 3828146, 4098093, 4284920, 4482316, 5079250, 5397385, 5441538)
men_only_filtered <- men_only_filtered %>%
  filter(!Participant.ID %in% ids_to_remove)