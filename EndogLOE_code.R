install.packages('tidyverse')
install.packages('data.table')
install.packages('lme4')
install.packages('ggplot2')

library('tidyverse')
library('data.table')
library('lme4')
library('ggplot2')
library('tidyr')
library('dplyr')

install.packages('cli')
library('cli')
install.packages(c("cli", "pillar"))

library("cli")
library("pillar")


mutated_data <- EndogenousLOE_DeathAge_participant %>%
  mutate(across(starts_with("Age when periods started (menarche)"), as.character),
         across(starts_with("Number of live births"), as.character),
         across(starts_with("Age at death"), as.character))


long_data <- mutated_data %>%
  pivot_longer(
    cols = starts_with("Had menopause") |
      starts_with("Age at menopause") |
      starts_with("Age when periods started (menarche)") |
      starts_with("Number of live births") |
      starts_with("Age at death"),
    names_to = c(".value", "Instance"),
    names_pattern = "(.*) \\| Instance ([0-9]+)"
  )


#filter by women:
women_only <- long_data %>%
  filter(str_detect(Sex, "(?i)female"))


#Had menopause
menopausal_data <- women_only %>%
  filter(`Had menopause` == "Yes")

#Remove NAs
cleaned_data <- menopausal_data %>%
  filter(!is.na(`Age at menopause (last menstrual period)`))
cleaned_data2 <- cleaned_data %>%
  filter(!is.na(`Age when periods started (menarche)`))

#NAs --> 0 in live births
cleaned_data3 <- cleaned_data2 %>%
  mutate(`Number of live births` = ifelse(is.na(`Number of live births`), 0, `Number of live births`))


# Remove Don't knows/prefer not to answer
cdata4 <- cleaned_data3 %>%
  filter(`Age at menopause (last menstrual period)` != "Do not know")
cdata5 <- cdata4 %>%
  filter(`Age when periods started (menarche)` != "Do not know")
cdata6 <- cdata5 %>%
  filter(`Age when periods started (menarche)` != "Prefer not to answer")
cdata7 <- cdata6 %>%
  filter(`Age at menopause (last menstrual period)` != "Prefer not to answer")
cdata8 <- cdata7 %>%
  filter(`Number of live births` != "Prefer not to answer")

# Convert to numeric form
numeric_data <- cdata8 %>%
  mutate(
    `Age at menopause (last menstrual period)` = as.numeric(`Age at menopause (last menstrual period)`),
    `Age when periods started (menarche)` = as.numeric(`Age when periods started (menarche)`),
    `Number of live births` = as.numeric(`Number of live births`),
    `Age at death` = as.numeric(`Age at death`)
  )

#Max values:
n_distinct(Df6$ParticipantID)

Cohort <- numeric_data %>%
  group_by(`Participant ID`) %>%
  summarise(across(c('Number of live births', 'Age at death', 'Age at menopause (last menstrual period)', 'Age when periods started (menarche)'), max, na.rm = TRUE))

numeric_data <- arrange(numeric_data, `Participant ID`)

# Group and summarize

Cohort <- numeric_data[, lapply(.SD, max, na.rm = TRUE), by = 'Participant ID', .SDcols = c('Number of live births', 'Age at death', 'Age at menopause (last menstrual period)', 'Age when periods started (menarche)')]

library(data.table)

numeric_data <- as.data.table(numeric_data) 

Cohort <- numeric_data[, lapply(.SD, max, na.rm = TRUE), by = 'Participant ID', .SDcols = c('Number of live births', 'Age at death', 'Age at menopause (last menstrual period)', 'Age when periods started (menarche)')]


#filter by death:
Cohort$`Age at death`[Cohort$`Age at death` == -Inf] <- NA
Cohort <- Cohort[!is.na(Cohort$`Age at death`), ]


#Endogenous LOE:
Cohort$reproLifespan = Cohort$`Age at menopause (last menstrual period)` - Cohort$`Age when periods started (menarche)`
Cohort$endLOE = Cohort$reproLifespan + Cohort$`Number of live births`


#Regression

endLOE_model <- lm(endLOE ~ `Age at death`, data = Cohort)

summary(endLOE_model)
#Plot

LOE_Death = ggplot(Cohort, aes(x = `Age at death`, y = endLOE)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot with Linear Regression Line",
       x = "Age at Death",
       y = "endLOE")

# Save the scatter plot as an image file (e.g., PNG)

ggsave("LOE_Death.png", LOE_Death, width = 6, height = 4, dpi = 300)




