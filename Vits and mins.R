
#   git config --global user.email "DouglasEA@cardiff.ac.uk"
#   git config --global user.name "Eve"
# You should have been able to clone the Git repository: https://github.com/Randrews92/UKBBMeno.git and you should now be able to see the GIT button above.

# First go into Terminal and type: dx download Vitamins_and_supplements_participant.csv

# The file should be loaded in the lower right hand panel under the Files tab. 
# Click on the file and select Import Dataset. The file should populate in Environment in the upper right panel.

# Once you've created your branch on Git you'll need to select the Git button above and select Pull Branches.

# To switch to your own branch, select the Terminal tab below and type: git checkout branchname (where branchname is the name of your branch)

#when you start each new session:

library('tidyverse')
library('dplyr')
library('data.table')
library('gtsummary')
library('ggplot2')

# Use ?<package> so learn what the packages do, for example:

?gtsummary

# Rename your dataset so it's shorter:

vits <- Vitamins_and_supplements_participant
n_distinct(vits$`Participant ID`)
mean(vits$`Age at recruitment`)

# Create a cohort of women who have selected Yes to taking a vitamin/ supplement in any instance.
# In this analysis we will use the 'Vitamin supplement user' columns by selecting all columns beginning with "Vitamin supplement", and filtering all 'Yes' answers

vitsonly <- vits %>% 
  filter(if_any(starts_with("Vitamin supplement"), ~grepl("Yes", .)))

mean(vitsonly$`Age at recruitment`)

range(vitsonly$`Age at recruitment`)


# Next we want to explore supplement prevalence, regardless of which instance they reported using supplements
# We need to reshape data from wide to long format, so the instances are stacked into one column and the supplement type is stacked in a second column

vitslong <- vitsonly %>% 
  pivot_longer(
    cols = 14:18, # To do this we'll combine answers to columns 14-18 (Vitamin and/or mineral supplement use)
    names_to = "Vitamin and mineral supplements-instance", #reshape data from wide to long format, so the instances are stacked into one column
    values_to = "Supplement type"  #so supplement type is stacked in another column according to the instance they reported it. 
  )

# Now we can count combinations of supplement use by ID:

vitslong %>% 
  group_by(`Supplement type`) %>% 
  summarise (count=n_distinct(`Participant ID`))
 
# This gives over 9000 combinations of supplements usage, which isn't ideal. 

# Instead we can explore the counts by ID for each unique supplement: 
# First we can separate the supplements into separate rows by splitting by the delimiter "|"

x <- vitslong %>% 
separate_longer_delim(`Supplement type`, delim = "|")

x

# Now these are split we can summarise total unique supps by ID to get supplement prevalence:

T <-x %>% 
  group_by(`Supplement type`) %>% 
  summarise (count=n_distinct(`Participant ID`))

# You can also explore supplement prevalence by instance:

T <-x %>% 
  group_by(`Supplement type`, `Vitamin and mineral supplements-instance`) %>% 
  summarise (count=n_distinct(`Participant ID`))


#My code
Menopause <- vits %>% 
  filter(if_any(starts_with("Had menopause"), ~grepl("Yes", .)))
n_distinct(vitsonly$`Participant ID`)

vitCalcium <- vitsonly %>% 
  filter(if_any(starts_with("Vitamin and/or"), ~grepl("Calcium", .)))
n_distinct(vitCalcium$`Participant ID`)
mean(vitCalcium$`Age at recruitment`)
range(vitCalcium$`Age at recruitment`)

vitChrom <- vitsonly %>% 
  filter(if_any(starts_with("Vitamin and/or"), ~grepl("Chromium", .)))
n_distinct(vitChrom$`Participant ID`)
mean(vitChrom$`Age at recruitment`)
range(vitChrom$`Age at recruitment`)

vitsonlyMeno <- vitsonly %>% 
  filter(if_any(starts_with("Had menopause"), ~grepl("Yes", .)))
n_distinct(vitsonlyMeno$`Participant ID`)
range(vitsonlyMeno$`Age at recruitment`)

MenoZinc <- vitsonlyMeno %>% 
  filter(if_any(starts_with("Vitamin and/or"), ~grepl("Zinc", .)))
n_distinct(MenoZinc$`Participant ID`)
mean(MenoZinc$`Age at recruitment`)

vitsonlyHRT <- vitsonlyMeno %>% 
  filter(if_any(starts_with("Ever used hormone-replacement"), ~grepl("Yes", .)))
n_distinct(vitsonlyHRT$`Participant ID`)

vitslongA <- vitsonly %>% 
  pivot_longer(
    cols = 23:26, 
    names_to = "Age at menopause-instance", 
    values_to = "age" 
  )
vitslongB <- vitslongA[!is.na(as.numeric(vitslongA$age)),]
vitslongB         
vitslongB$age2=as.numeric(vitslongB$age)
mean(vitslongB$`age2`)

Totalagelong <- vits %>% 
  pivot_longer(
    cols = 23:26, 
    names_to = "Age at menopause-instance", 
    values_to = "age" 
  )
TotalagelongB <- Totalagelong[!is.na(as.numeric(Totalagelong$age)),]
TotalagelongB         
TotalagelongB$age2=as.numeric(TotalagelongB$age)
mean(TotalagelongB$`age2`)

vitslongBzinc <- vitslongB %>% 
  filter(if_any(starts_with("Vitamin and/or"), ~grepl("Zinc", .)))
mean(vitslongBzinc$`age2`)
