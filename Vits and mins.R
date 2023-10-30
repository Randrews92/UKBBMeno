

## You should have been able to clone the Git repository: https://github.com/Randrews92/UKBBMeno.git and you should now be able to see the GIT button above.

# First go into terminal and type: dx download Vitamins_and_supplements_participant.csv

# The file should be loaded in the lower right hand panel under the Files tab. 
# Click on the file and select Import Dataset. The file should populate in Environment in the upper right panel.

# Next install these packages:

install.packages('tidyverse')
install.packages('dplyr')
install.packages('data.table')
install.packages('broom.helpers') #needed to install gtsummary
#If you have issues installing broom.helpers one go into Terminal and type the following lines one after the other: 
#sudo apt-get update -y 
#sudo apt-get update
#sudo apt-get install cmake
install.packages('gtsummary')
install.packages('ggplot2') #good for creating graphs and data visualising 


# Once all have been installed successfully remove the above code, going forward load the installed packages using library when you start each new session:

library('tidyverse')
library('dplyr')
library('data.table')
library('gtsummary')
library('ggplot2')

# Use ?<package> so learn what the packages do, for example:

?gtsummary

# Rename your dataset so it's shorter:

vits <- Vitamins_and_supplements_participant

# Count total participants in dataset:

n_distinct(vits$`Participant ID`)

#average age of all IDs at recruitment

mean(vits$`Age at recruitment`)

# Create a cohort of women who have selected Yes to taking a vitamin/ supplement in any instance.
# In this analysis we will use the 'Vitamin supplement user' columns by selecting all columns beginning with "Vitamin supplement", and filtering all 'Yes' answers

vitsonly <- vits %>% 
  filter(if_any(starts_with("Vitamin supplement"), ~grepl("Yes", .)))

#get the average age at recruitment of all IDs who said they used supps

mean(vitsonly$`Age at recruitment`)

# Age range of supp users

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


## Next steps will be to conduct some descriptive analysis of the data. Use this as a means of getting used to and learning R.
# Look up youtube tutorials on Tidyverse and R basics.
# You can practice conducting analysis to examine average age (at onboarding) per supplement type used. 
# You can also create a cohort of women who reported menopause using the methods above. 
# You can then look at supplement prevalence in a cohort of menopausal women. 
# Don't worry too much about things being perfect or final-the important thing is to learn and practice analysing the data. 
# You can also explore using package gtSummary to analyse the data and create nice tables: https://www.danieldsjoberg.com/gtsummary/

## When you've finished working on this, save it, then select the GIT button above and select Commit. 
## Commit your changes then Push your changes (use the PAT I sent you if it requests a password).
## Make sure you Quit and Terminate the session once you're finished, closing the window will not terminate and they'll continue to charge us. 

saveRDS(vits, "vits.rds")

#
