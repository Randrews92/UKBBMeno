install.packages('tidyverse')
install.packages('data.table')
install.packages('lme4')
install.packages('ggplot2')

library('tidyverse')
library('data.table')
library('lme4')
library('ggplot2')

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