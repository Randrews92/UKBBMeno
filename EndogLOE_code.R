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

pivoted_data <- mutated_data %>%
  pivot_longer(
    cols = starts_with("Had menopause"),
    names_to = "Had menopause-instance",
    values_to = "Had menopause"
  ) %>%
  pivot_longer(
    cols = starts_with("Age at menopause"),
    names_to = "Age at menopause-instance",
    values_to = "Age at menopause"
  ) %>%
  pivot_longer(
    cols = starts_with("Age when periods started"),
    names_to = "Age at Menarche instance",
    values_to = "Age at Menarche"
  ) %>%
  pivot_longer(
    cols = starts_with("Number of live births"),
    names_to = "Number of live births instance",
    values_to = "Number of live births"
  ) %>%
  pivot_longer(
    cols = starts_with("Age at death"),
    names_to = "Age at death-instance",
    values_to = "Age at death"
  )


#filter by women:
women_only <- pivoted_data %>%
  filter(str_detect(Sex, "(?i)female"))

#filter by death:
deceased_data <- women_only %>%
  filter(!is.na(`Age at death`))

#Had menopause
menopausal_data <- deceased_data %>%
  filter(`Had menopause` == "Yes")

#Remove NAs
cleaned_data <- menopausal_data %>%
  filter(!is.na(`Age at menopause`))
cleaned_data2 <- cleaned_data %>%
  filter(!is.na(`Age at Menarche`))

#NAs --> 0 in live births
cleaned_data3 <- cleaned_data2 %>%
  mutate(`Number of live births` = ifelse(is.na(`Number of live births`), 0, `Number of live births`))

#issue is that if they have answered in instance 1 the other instances are NA and are now 0. 

# Remove Don't knows/prefer not to answer
cdata4 <- cleaned_data3 %>%
  filter(`Age at menopause` != "Do not know")
cdata5 <- cdata4 %>%
  filter(`Age at Menarche` != "Do not know")
cdata6 <- cdata5 %>%
  filter(`Age at Menarche` != "Prefer not to answer")
cdata7 <- cdata6 %>%
  filter(`Age at menopause` != "Prefer not to answer")
cdata8 <- cdata7 %>%
  filter(`Number of live births` != "Prefer not to answer")

# Convert to numeric form
numeric_data <- cdata8 %>%
  mutate(
    `Age at menopause` = as.numeric(`Age at menopause`),
    `Age at Menarche` = as.numeric(`Age at Menarche`),
    `Number of live births` = as.numeric(`Number of live births`),
    `Age at death` = as.numeric(`Age at death`)
  )

#Max values:
Cohort <- numeric_data %>%
  group_by(participantID) %>%
  summarise(across(c(`Number of live births`, Ageatdeath, Ageatmenopause, AgeAtMenarche), max, na.rm = TRUE))

n_distinct(Df6$ParticipantID)

Cohort <- numeric_data %>%
  group_by(participantID) %>%
  summarise(across(c(`Number of live births`, 'Age at death', 'Age at menopause', 'Age at Menarche'), ~max(.), na.rm = TRUE))

#Endogenous LOE:
Cohort$reproLifespan = Cohort$Age at menopause – Cohort$Age at Menarche
Cohort$endLOE = Cohort$reproLifespan + Cohort$Number of live births

#Regression
your_model <- lm(endLOE ~ ageAtDeath, data = Cohort)

#Plot
ggplot(your_data, aes(x = ageAtDeath, y = endLOE)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot with Linear Regression Line",
       x = "Age at Death",
       y = "endLOE")



#Melt function attempt

setDT(mutated_data)  # Convert to data.table

# Specify ID variables
id_vars <- c("Participant ID", "Sex")

# Melt data for "Had menopause" instances
melted_had_menopause <- melt(mutated_data,
                             id.vars = id_vars,
                             measure.vars = patterns("^Had menopause"),
                             variable.name = "Had menopause-instance")
melted_had_menopause[, Instance := gsub(".*_(\\d+)", "\\1", `Had menopause-instance`)]

# Melt data for "Age at menopause" instances
melted_age_at_menopause <- melt(mutated_data,
                                id.vars = id_vars,
                                measure.vars = patterns("^Age at menopause"),
                                variable.name = "Age at menopause-instance")
melted_age_at_menopause[, Instance := gsub(".*_(\\d+)", "\\1", `Age at menopause-instance`)]

# Melt data for "Age when periods started" instances
melted_age_at_menarche <- melt(mutated_data,
                               id.vars = id_vars,
                               measure.vars = patterns("^Age when periods started"),
                               variable.name = "Age at Menarche instance")
melted_age_at_menarche[, Instance := gsub(".*_(\\d+)", "\\1", `Age at Menarche instance`)]

# Melt data for "Number of live births" instances
melted_live_births <- melt(mutated_data,
                           id.vars = id_vars,
                           measure.vars = patterns("^Number of live births"),
                           variable.name = "Number of live births instance")
melted_live_births[, Instance := gsub(".*_(\\d+)", "\\1", `Number of live births instance`)]

# Melt data for "Age at death" instances
melted_age_at_death <- melt(mutated_data,
                            id.vars = id_vars,
                            measure.vars = patterns("^Age at death"),
                            variable.name = "Age at death-instance")
melted_age_at_death[, Instance := gsub(".*_(\\d+)", "\\1", `Age at death-instance`)]

# combine tables
combined_data <- rbindlist(list(melted_had_menopause,
                                melted_age_at_menopause,
                                melted_age_at_menarche,
                                melted_live_births,
                                melted_age_at_death),
                           use.names = TRUE, fill = TRUE)

