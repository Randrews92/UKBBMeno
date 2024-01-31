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


EndogLong <- EndogenousLOE_DeathAge_participant %>%
  pivot_longer(
    cols = 3:6,
    names_to = "Had menopause-instance",
    values_to = "Had menopause"
  )

EndogLong1 <- EndogLong %>%
  pivot_longer(
    cols = 3:6,
    names_to = "Age at menopause-instance",
    values_to = "Age at menopause"
  )
EndogLong2 <- EndogLong1 %>%
  pivot_longer(
    cols = 3:6,
    names_to = "Age at menarche-instance",
    values_to = "Age at menarche"
  )

EndoLong1 <- EndogLong1 %>%
  mutate(
    `Age when periods started (menarche) | Instance 1` = as.character(`Age when periods started (menarche) | Instance 1`),
    `Age when periods started (menarche) | Instance 3` = as.character(`Age when periods started (menarche) | Instance 3`)
  ) %>%
  pivot_longer(cols = c(`Age when periods started (menarche) | Instance 0`, `Age when periods started (menarche) | Instance 1`, `Age when periods started (menarche) | Instance 2`, `Age when periods started (menarche) | Instance 3`), 
               names_to = "Age at Menarche instance", 
               values_to = "Age at Menarche")

EndogLong2 <- EndoLong1 %>%
  mutate(
    `Number of live births | Instance 1` = as.character(`Number of live births | Instance 1`),
    `Number of live births | Instance 2` = as.character(`Number of live births | Instance 2`),
    `Number of live births | Instance 3` = as.character(`Number of live births | Instance 3`)
  ) %>%
  pivot_longer(cols = c(`Number of live births | Instance 0`, `Number of live births | Instance 1`, `Number of live births | Instance 2`, `Number of live births | Instance 3`), 
               names_to = "Number of live births instance", 
               values_to = "Number of live births")

EndogLong3 <- EndogLong2 %>%
  pivot_longer(
    cols = 3:4,
    names_to = "Age at death-instance",
    values_to = "Age at death"
  )

#filter by women:
EndolongFemale= EndogLong3Long%>%filter(grepl("female", Sex))
EndolongFem= EndolongFemale %>% filter(!is.na(Sex))

#filter by death:
DF1= EndolongFem %>% filter(!is.na(ageAtDeath))
DF2= DF1 %>% filter(ageAtDeath,<0)

#Had menopause
DF3= DF2%>%filter(grepl("Yes", HadMenopause))

#Remove NAs
DF4= DF3 %>% filter(!is.na(ageAtMenopause))
DF5= DF4 %>% filter(!is.na(ageAtMenarche))

#NAs --> 0 in live births
Df6<- Df5%>%
  mutate(livebirths1 = ifelse(is.na(livebirths), 0, livebirths))

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

library(data.table)


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

