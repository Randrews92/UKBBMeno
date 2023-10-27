install.packages('tidyverse')
install.packages('dplyr')
install.packages('data.table')
install.packages('gtsummary')
install.packages('ggplot2')

library('tidyverse')
library('dplyr')
library('data.table')
library('gtsummary')
library('ggplot2')

vits <- Vitamins_and_supplements_participant
n_distinct(vits$`Participant ID`)

#average age of all IDs at recruitment

mean(vits$`Age at recruitment`)


vitsonly <- vits %>% 
  filter(if_any(starts_with("Vitamin supplement"), ~grepl("Yes", .)))

#average age at recruitment of all IDs who said they used supps

mean(vitsonly$`Age at recruitment`)

# Age range of supp users

range(vitsonly$`Age at recruitment`)
