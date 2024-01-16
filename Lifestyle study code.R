install.packages('tidyverse')
install.packages('data.table')
install.packages('broom.helpers')
install.packages('gtsummary')

library('tidyverse')
library('data.table')
library('gtsummary')

Jlong <- J %>%
  pivot_longer(
    cols = 5:8,
    names_to = "Qualifications-instance",
    values_to = "Qualifications type"
  )

Jlong <- Jlong %>%
  pivot_longer(
    cols = 5:8,
    names_to = "Ethnic background-instance",
    values_to = "Ethnic background type"
  )