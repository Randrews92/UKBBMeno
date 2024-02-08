
# run dx donwload to get the Lifestyle_table.csv 

install.packages('tidyverse')
install.packages('data.table')
install.packages('gtsummary')
install.packages('ggplot2')
install.packages('tidyr')

library('tidyverse')
library('dplyr')
library('data.table')
library('gtsummary')
library('ggplot2')
library('tidyr')

# Unload packages with issues 

if ("cli" %in% search()) detach("package:tidyverse", unload = TRUE)

if ("dplyr" %in% search()) detach("package:dplyr", unload = TRUE)

install.packages('pacman') # This is a great package which apparently checks whether things need to installed before loadi

library(pacman)
pacman::p_load(tidyverse, ggplot2, data.table, gtsummary)

Lifestyle_table <- read.csv("Lifestyle_table.csv")

# Explore the table
str(Lifestyle_table)
head(Lifestyle_table)

# There are loads of columns that can be merged, this will reduce the size of the table. 

# the first thing we will need to do is convert columns into factors:
# Let's start with merging Qualifications, starting with columns with the same keyword:

# Specify the keyword
keyword <- "Qualifications"

# Get column names containing the keyword
matching_columns <- grep(keyword, names(Lifestyle_table), value = TRUE)

# Print the column names
print(matching_columns)

columns_to_convert <- c("Qualifications | Instance 0", 
                        "Qualifications | Instance 1", 
                        "Qualifications | Instance 2",
                        "Qualifications | Instance 3")

# Use lapply to apply factor() to selected columns
Lifestyle_table[columns_to_convert] <- lapply(Lifestyle_table[columns_to_convert], factor)

# Then we will find the levels in these columns:

# Get levels of each factor column
levels(Lifestyle_table$`Qualifications | Instance 0`)
levels(Lifestyle_table$`Qualifications | Instance 1`)
levels(Lifestyle_table$`Qualifications | Instance 2`)
levels(Lifestyle_table$`Qualifications | Instance 3`)

# All have the same values

# Create a data frame with all Qualifications and instances in long format:
# First subset ID and qualifications data
subset_data <- Lifestyle_table %>%
  select(`Participant ID`,
         `Qualifications | Instance 0`,
         `Qualifications | Instance 1`,
         `Qualifications | Instance 2`,
         `Qualifications | Instance 3`)

long_quals <- subset_data %>%
  pivot_longer(
    cols = starts_with("Qualifications"),
    names_to = c(".value", "Instance"),
    names_pattern = "(.*) \\| Instance ([0-9]+)")

# Get levels 
levels(long_quals$`Qualifications`)

# All quals for each participant is on one row, we want them on multiple rows so we can score them:
long_quals<- long_quals %>%
  separate_rows(Qualifications, sep = "\\|") %>%
  mutate(Qualifications = trimws(Qualifications))

#

# Create a Qualifications score with 5 highest and 0 lowest quals

long_quals$Qualifications <- ifelse(
  grepl("College or University degree", long_quals$Qualifications), 5,
  ifelse(grepl("A levels/AS levels or equivalent", long_quals$Qualifications), 4,
         ifelse(grepl("NVQ or HND or HNC or equivalent", long_quals$Qualifications), 3,
                ifelse(grepl("O levels/GCSEs or equivalent", long_quals$Qualifications), 2,
                       ifelse(grepl("CSEs or equivalent", long_quals$Qualifications), 1,
                              ifelse(grepl("Other professional qualifications eg: nursing, teaching", long_quals$Qualifications), 5,
                                     ifelse(grepl("None of the above", long_quals$Qualifications), 0, long_quals$Qualifications)
                              )
                       )
                )
         )
  )
)

# Get the Max qualification score per ID:

max_qual_scores <- long_quals %>%
  group_by(`Participant ID`) %>%
  summarise(Qualifications = max(Qualifications, na.rm = TRUE))

# Format 'Prefer not to answer as NA'

max_qual_scores <- max_qual_scores %>%
  mutate(Qualifications = ifelse(Qualifications == "Prefer not to answer", NA, Qualifications))

# Format Qualifciations as a number

max_qual_scores$Qualifications <- as.numeric(max_qual_scores$Qualifications)

#Rename to Qual_score

max_qual_scores$QualScore <- max_qual_scores$Qualifications

# Join QualScore back onto main dataset:

Lifestyle_table <- setDT(Lifestyle_table)[setDT(max_qual_scores), QualScore := i.QualScore, on=c("`Participant ID`" )]

# Remove the old qual variables:

Lifestyle_table1  <- Lifestyle_table  %>%
  select(-"Qualifications | Instance 0", 
         -"Qualifications | Instance 1", 
         -"Qualifications | Instance 2",
         -"Qualifications | Instance 3")

# Remove all ethnicity columns except instance 0

# Specify the keyword
keyword <- "Ethnic"

# Get column names containing the keyword
matching_columns <- grep(keyword, names(Lifestyle_table1), value = TRUE)

# Print the column names
print(matching_columns)

Lifestyle_table2  <- Lifestyle_table1  %>%
  select( 
    -"Ethnic background | Instance 1", 
    -"Ethnic background | Instance 2",
    -"Ethnic background | Instance 3")

# Remove all former alcohol variables except for Alcohol intake frequency

# Specify the keyword
keyword <- "Alcohol"

# Get column names containing the keyword
matching_columns <- grep(keyword, names(Lifestyle_table2), value = TRUE)

# Print the column names
print(matching_columns)

Lifestyle_table3  <- Lifestyle_table2  %>%
  select(-"Alcohol drinker status | Instance 0",
         -"Alcohol drinker status | Instance 1",   
         -"Alcohol drinker status | Instance 2",
         -"Alcohol drinker status | Instance 3" )

# Specify the keyword
keyword <- "alcohol"

# Get column names containing the keyword
matching_columns <- grep(keyword, names(Lifestyle_table2), value = TRUE)

# Print the column names
print(matching_columns)

Lifestyle_table4  <- Lifestyle_table3  %>%
  select(-"Former alcohol drinker | Instance 0",
         -"Former alcohol drinker | Instance 1",
         -"Former alcohol drinker | Instance 2", 
         -"Former alcohol drinker | Instance 3" )

# Remove all smoking variables except smoking status

# Specify the keyword
keyword <- "smoking"

# Get column names containing the keyword
matching_columns <- grep(keyword, names(Lifestyle_table), value = TRUE)

# Print the column names
print(matching_columns)

Lifestyle_table5  <- Lifestyle_table4  %>%
  select(-"Past tobacco smoking | Instance 0",                                                            
         -"Past tobacco smoking | Instance 1",                                                            
         -"Past tobacco smoking | Instance 2",                                                            
         -"Past tobacco smoking | Instance 3",                                                            
         -"Current tobacco smoking | Instance 0",                                                         
         -"Current tobacco smoking | Instance 2",                                                         
         -"Current tobacco smoking | Instance 1",                                                         
         -"Current tobacco smoking | Instance 3",                                                         
         -"Age started smoking in current smokers | Instance 0",                                          
         -"Age started smoking in current smokers | Instance 1",                                          
         -"Age started smoking in current smokers | Instance 2",                                          
         -"Age started smoking in current smokers | Instance 3",                                          
         - "Age stopped smoking cigarettes (current cigar/pipe or previous cigarette smoker) | Instance 0",
         -"Age stopped smoking cigarettes (current cigar/pipe or previous cigarette smoker) | Instance 1",
         -"Age stopped smoking cigarettes (current cigar/pipe or previous cigarette smoker) | Instance 2",
         -"Age stopped smoking cigarettes (current cigar/pipe or previous cigarette smoker) | Instance 3")


# Specify the keyword
keyword <- "Smoking"

# Get column names containing the keyword
matching_columns <- grep(keyword, names(Lifestyle_table), value = TRUE)

# Print the column names
print(matching_columns)

Lifestyle_table6  <- Lifestyle_table5  %>%
  select(-"Smoking/smokers in household | Instance 0",
         -"Smoking/smokers in household | Instance 1",
         -"Smoking/smokers in household | Instance 2",
         -"Smoking/smokers in household | Instance 3")

# Convert alcohol and smoking status at baseline to fcators

# Check the unqique values
unique(Lifestyle_table6$`Smoking status | Instance 0`)
unique(Lifestyle_table6$`Alcohol intake frequency. | Instance 0`)
# Convert Smoking and alcohol status instance 0 to factors and rename

Lifestyle_table6$SmokingBaseline <- as.factor(Lifestyle_table6$`Smoking status | Instance 0`)
Lifestyle_table6$AlcoholBaseline <- as.factor(Lifestyle_table6$`Alcohol intake frequency. | Instance 0`)

#### Calculating diet #####
# Using the methods above we will begin to create a Diet Score using Instance 0 for the following:
# Diet score is based on this study's method: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC10540665/
# "Fresh fruit intake | Instance 0"
# "Salad / raw vegetable intake | Instance 0"
# "Cooked vegetable intake | Instance 0"

# Recode Do not know and prefer not to answer

Lifestyle_table7 <- Lifestyle_table6 %>%
  mutate('Fresh fruit intake | Instance 0' = ifelse('Fresh fruit intake | Instance 0' == "Prefer not to answer", NA, 'Fresh fruit intake | Instance 0'))

Lifestyle_table7 <- Lifestyle_table6 %>%
  mutate(Fresh.fruit.intake...Instance.0 = ifelse(Fresh.fruit.intake...Instance.0 == "Do not know" , NA, Fresh.fruit.intake...Instance.0))

Lifestyle_table7 <- Lifestyle_table6 %>%
  mutate(Fresh.fruit.intake...Instance.0 = ifelse(Fresh.fruit.intake...Instance.0 == "Less than one" , NA, Fresh.fruit.intake...Instance.0))

# Do this for:
# "Salad / raw vegetable intake | Instance 0"
# "Cooked vegetable intake | Instance 0"

# Once you've done this convert the variables to numeric

# Add them together using this code (or similar depending on your variable names):

Lifestyle_tableX<- your_data %>%
  group_by(Participant.ID) %>%
  mutate(DietScore = rowSums(select(., Fresh.fruit.intake...Instance.0, Salad...raw.vegetable.intake...Instance.0,
                                    Cooked.vegetable.intake...Instance.0), na.rm = TRUE))


# Remove all other diet variables from the table using the methods above and just keep the DietScore

### LOE ###
# Using the methods described on this script, and the methods used in EndogLOE_code.R merge columns on:
# age at menopause (merge all instances so there's one age at menopause column)
# age when periods started
# Number of children born (get max value before merge as shown in Qualifications section above)
# age at bilateral oophorectomy 