# run dx download to get the Lifestyle_table.csv 

install.packages('tidyverse')
install.packages('data.table')
install.packages('gtsummary')
install.packages('ggplot2')
install.packages('tidyr')

library('tidyverse')
library('data.table')
library('gtsummary')
library('dplyr')
library('tidyr')
library('ggplot2')

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
#Fresh fruit
Lifestyle_table7 <- Lifestyle_table6 %>%
  
  mutate(Fresh = ifelse(`Fresh fruit intake | Instance 0` == "Prefer not to answer", NA, `Fresh fruit intake | Instance 0` ))

Lifestyle_table8 <- Lifestyle_table7 %>%
  
  mutate(Fresh = ifelse(Fresh== "Do not know" , NA, Fresh ))

Lifestyle_table9 <- Lifestyle_table8 %>%
  
  mutate(Fresh= ifelse(Fresh == "Less than one" , NA, Fresh ))

#Salad
Lifestyle_table10 <- Lifestyle_table9%>%
  
  mutate(Salad= ifelse(`Salad / raw vegetable intake | Instance 0` == "Prefer not to answer", NA, `Salad / raw vegetable intake | Instance 0`))

Lifestyle_table11 <- Lifestyle_table10 %>%
  
  mutate(Salad = ifelse(Salad== "Do not know" , NA, Salad ))

Lifestyle_table12 <- Lifestyle_table11 %>%
  
  mutate(Salad= ifelse(Salad == "Less than one" , NA, Salad ))

#Cooked
Lifestyle_table13 <- Lifestyle_table12%>%
  
  mutate(Cooked= ifelse(`Cooked vegetable intake | Instance 0` == "Prefer not to answer", NA, `Cooked vegetable intake | Instance 0`))

Lifestyle_table14 <- Lifestyle_table13 %>%
  
  mutate(Cooked = ifelse(Cooked== "Do not know" , NA, Cooked ))

Lifestyle_table15 <- Lifestyle_table14 %>%
  
  mutate(Cooked= ifelse(Cooked == "Less than one" , NA, Cooked ))

#Dried fruit
Lifestyle_table16 <- Lifestyle_table15%>%
  
  mutate(Dried= ifelse(`Dried fruit intake | Instance 0` == "Prefer not to answer", NA, `Dried fruit intake | Instance 0`))

Lifestyle_table17 <- Lifestyle_table16 %>%
  
  mutate(Dried = ifelse(Dried== "Do not know" , NA, Dried ))

Lifestyle_table18 <- Lifestyle_table17 %>%
  
  mutate(Dried= ifelse(Dried == "Less than one" , NA, Dried ))


# Once you've done this convert the variables to numeric
Lifestyle_table19 <- Lifestyle_table18 %>%
  mutate(across(c(Fresh, Salad, Cooked, Dried), as.numeric))

# Add them together using this code (or similar depending on your variable names):

library(dplyr)

# Group the data by Participant ID
grouped_data <- Lifestyle_table19 %>%
  group_by(`Participant ID`)

#Ensure all are numeric
grouped_data <- grouped_data %>%
  mutate(
    Fresh = as.numeric(Fresh),
    Salad = as.numeric(Salad),
    Cooked = as.numeric(Cooked),
    Dried = as.numeric(Dried)
  )

# Calculate the Diet Score by summing Fresh, Salad, Cooked, and Dried
diet_score <- grouped_data %>%
  rowwise() %>%
  mutate(DietScore = sum(Fresh, Salad, Cooked, Dried, na.rm = TRUE))

# Check for non-numeric values in Fresh, Salad, Cooked, and Dried columns
non_numeric_values <- grouped_data %>%
  summarise(
    Fresh = any(!is.na(Fresh) & !is.numeric(Fresh)),
    Salad = any(!is.na(Salad) & !is.numeric(Salad)),
    Cooked = any(!is.na(Cooked) & !is.numeric(Cooked)),
    Dried = any(!is.na(Dried) & !is.numeric(Dried))
  )

# Print non-numeric values
print(non_numeric_values)


# Remove all other diet variables from the table using the methods above and just keep the DietScore
keyword <- "fish"
matching_columns <- grep(keyword, names(diet_score), value = TRUE)
print(matching_columns)

Lifestyle_tableA  <- diet_score  %>%
  select(-"Oily fish intake | Instance 0",
         -"Oily fish intake | Instance 1",
         -"Oily fish intake | Instance 2", 
         -"Oily fish intake | Instance 3",
         -"Non-oily fish intake | Instance 0",
         -"Non-oily fish intake | Instance 1",
         -"Non-oily fish intake | Instance 2")
keyword <- "meat"
matching_columns <- grep(keyword, names(Lifestyle_tableA), value = TRUE)
print(matching_columns)

Lifestyle_tableB  <- Lifestyle_tableA  %>%
  select(-"Processed meat intake | Instance 0.x",
         -"Processed meat intake | Instance 0.y",
         -"Processed meat intake | Instance 1", 
         -"Processed meat intake | Instance 2",
         -"Processed meat intake | Instance 3") 
keyword <- "intake"
matching_columns <- grep(keyword, names(Lifestyle_tableB), value = TRUE)
print(matching_columns)

Lifestyle_tableC  <- Lifestyle_tableB  %>%
  select(-"Cereal intake | Instance 0",
         -"Cereal intake | Instance 1",
         -"Cereal intake | Instance 2", 
         -"Cereal intake | Instance 3",
         -"Bread intake | Instance 0.x",
         -"Bread intake | Instance 0.y",
         -"Bread intake | Instance 1",
         -"Bread intake | Instance 2",
         -"Bread intake | Instance 3",
         -"Cheese intake | Instance 0",
         -"Cheese intake | Instance 1",
         -"Cheese intake | Instance 2",
         -"Cheese intake | Instance 3")

keyword <- "intake"
matching_columns <- grep(keyword, names(Lifestyle_tableC), value = TRUE)
print(matching_columns)

Lifestyle_tableD  <- Lifestyle_tableC  %>%
  select(-"Pork intake | Instance 0",
         -"Pork intake | Instance 1",
         -"Pork intake | Instance 2", 
         -"Pork intake | Instance 3",
         -"Lamb/mutton intake | Instance 0",
         -"Lamb/mutton intake | Instance 1",
         -"Lamb/mutton intake | Instance 2",
         -"Lamb/mutton intake | Instance 3",
         -"Beef intake | Instance 0",
         -"Beef intake | Instance 1",
         -"Beef intake | Instance 2",
         -"Beef intake | Instance 3",
         -"Poultry intake | Instance 0",
         -"Poultry intake | Instance 1",
         -"Poultry intake | Instance 2",
         -"Poultry intake | Instance 3",
         -"Dried fruit intake | Instance 0",
         -"Dried fruit intake | Instance 1",
         -"Dried fruit intake | Instance 2",
         -"Dried fruit intake | Instance 3",
         -"Fresh fruit intake | Instance 0",
         -"Fresh fruit intake | Instance 1",
         -"Fresh fruit intake | Instance 2",
         -"Fresh fruit intake | Instance 3",
         -"Salad / raw vegetable intake | Instance 0",
         -"Salad / raw vegetable intake | Instance 1",
         -"Salad / raw vegetable intake | Instance 2",
         -"Salad / raw vegetable intake | Instance 3",
         -"Cooked vegetable intake | Instance 0",
         -"Cooked vegetable intake | Instance 1",
         -"Cooked vegetable intake | Instance 2",
         -"Cooked vegetable intake | Instance 3")

keyword <- "type"
matching_columns <- grep(keyword, names(Lifestyle_tableD), value = TRUE)
print(matching_columns)

Lifestyle_tableE  <- Lifestyle_tableD  %>%
  select(-"Milk type used | Instance 0",
         -"Milk type used | Instance 1",
         -"Milk type used | Instance 2", 
         -"Bread type | Instance 0",
         -"Bread type | Instance 1",
         -"Bread type | Instance 2",
         -"Bread type | Instance 3",
         -"Cereal type | Instance 0",
         -"Cereal type | Instance 1",
         -"Cereal type | Instance 2",
         -"Cereal type | Instance 3")

keyword <- "Variation"
matching_columns <- grep(keyword, names(Lifestyle_tableE), value = TRUE)
print(matching_columns)

Lifestyle_tableF  <- Lifestyle_tableE  %>%
  select(-"Variation in diet | Instance 0",
         -"Variation in diet | Instance 1",
         -"Variation in diet | Instance 2", 
         -"Variation in diet | Instance 3")

Lifestyle_tableG  <- Lifestyle_tableF  %>%
  select(-"Fresh",
         -"Cooked",
         -"Dried", 
         -"Salad")
### LOE ###
# Using the methods described on this script, and the methods used in EndogLOE_code.R merge columns on:

# age at menopause (merge all instances so there's one age at menopause column)
mutated_data <- Lifestyle_tableG %>%
  mutate(across(starts_with("Age at menopause"), as.numeric))
Lifestyle_table_meno <- mutated_data %>%
  mutate(menopauseAge = rowMeans(select(., starts_with("Age at menopause")), na.rm = TRUE))

n_distinct(Lifestyle_table_meno$`Participant ID`)

keyword <- "Age at menopause"
matching_columns <- grep(keyword, names(Lifestyle_table_meno), value = TRUE)
print(matching_columns)

Lifestyle_tableH  <- Lifestyle_table_meno  %>%
  select(-"Age at menopause (last menstrual period) | Instance 0",
         -"Age at menopause (last menstrual period) | Instance 1",
         -"Age at menopause (last menstrual period) | Instance 2",
         -"Age at menopause (last menstrual period) | Instance 3")

# age when periods started
mutated_data2 <- Lifestyle_tableH %>%
  mutate(across(starts_with("Age when periods started (menarche)"), as.numeric))
Lifestyle_tableI <- mutated_data2 %>%
  mutate(menarcheAge = rowMeans(select(., starts_with("Age when periods started (menarche)")), na.rm = TRUE))

n_distinct(Lifestyle_table_meno$`Participant ID`)

keyword <- "Age when periods"
matching_columns <- grep(keyword, names(Lifestyle_tableI), value = TRUE)
print(matching_columns)

Lifestyle_tableJ  <- Lifestyle_tableI  %>%
  select(-"Age when periods started (menarche) | Instance 0",
         -"Age when periods started (menarche) | Instance 1",
         -"Age when periods started (menarche) | Instance 2",
         -"Age when periods started (menarche) | Instance 3")

# Number of children born (get max value before merge as shown in Qualifications section above)
keyword <- "Number of live births"
matching_columns <- grep(keyword, names(Lifestyle_tableJ), value = TRUE)
print(matching_columns)

columns_to_convert <- c("Number of live births | Instance 0", 
                        "Number of live births | Instance 1", 
                        "Number of live births | Instance 2",
                        "Number of live births | Instance 3")

# Use lapply to apply factor() to selected columns
Lifestyle_tableJ[columns_to_convert] <- lapply(Lifestyle_tableJ[columns_to_convert], factor)

# Then we will find the levels in these columns:

# Get levels of each factor column
levels(Lifestyle_tableJ$`Number of live births | Instance 0`)
levels(Lifestyle_tableJ$`Number of live births | Instance 1`)
levels(Lifestyle_tableJ$`Number of live births | Instance 2`)
levels(Lifestyle_tableJ$`Number of live births | Instance 3`)

# All have the same values

# Create a data frame with all instances in long format:
# First subset ID and birth data
subset_data <- Lifestyle_tableJ %>%
  select(`Participant ID`,
         `Number of live births | Instance 0`,
         `Number of live births | Instance 1`,
         `Number of live births | Instance 2`,
         `Number of live births | Instance 3`)

long_births <- subset_data %>%
  pivot_longer(
    cols = starts_with("Number of live births"),
    names_to = c(".value", "Instance"),
    names_pattern = "(.*) \\| Instance ([0-9]+)")

# Get levels 
levels(long_births$`Number of live births`)

# All births for each participant is on one row, we want them on multiple rows so we can score them:
long_births<- long_births %>%
  separate_rows(`Number of live births`, sep = "\\|") %>%
  mutate(`Number of live births` = trimws(`Number of live births`))

# Get the Max per ID:
max_birth_scores <- long_births %>%
  group_by(`Participant ID`) %>%
  summarise(`Number of live births` = max(`Number of live births`, na.rm = TRUE))


# Format 'Prefer not to answer as NA'
max_birth_scores2 <- max_birth_scores %>%
  mutate(`Number of live births` = ifelse(`Number of live births` == "Prefer not to answer", NA, `Number of live births`))

# Format as a number

max_birth_scores2$'Number of live births' <- as.numeric(max_birth_scores2$'Number of live births')

# Join back onto main dataset:

Lifestyle_tableJ <- setDT(Lifestyle_tableJ)[setDT(max_birth_scores2), `Number of live births` := `Number of live births`, on = .(`Participant ID`)]

# Remove the old variables:

Lifestyle_tableK  <- Lifestyle_tableJ  %>%
  select(-`Number of live births | Instance 0`,
         -`Number of live births | Instance 1`,
         -`Number of live births | Instance 2`,
         -`Number of live births | Instance 3`)

# age at bilateral oophorectomy 
mutated_data <- Lifestyle_tableK %>%
  mutate(across(starts_with("Age at bilateral oophorectomy"), as.numeric))
Lifestyle_tableL <- mutated_data %>%
  mutate(bilateral_oophorectomyAge = rowMeans(select(., starts_with("Age at bilateral oophorectomy")), na.rm = TRUE))

n_distinct(Lifestyle_tableL$`Participant ID`)

keyword <- "oophorectomy"
matching_columns <- grep(keyword, names(Lifestyle_tableK), value = TRUE)
print(matching_columns)

Lifestyle_tableM  <- Lifestyle_tableL  %>%
  select(-"Age at bilateral oophorectomy (both ovaries removed) | Instance 0",
         -"Age at bilateral oophorectomy (both ovaries removed) | Instance 1",
         -"Age at bilateral oophorectomy (both ovaries removed) | Instance 2",
         -"Age at bilateral oophorectomy (both ovaries removed) | Instance 3")

#Removing more tables:
Lifestyle_tableN  <- Lifestyle_tableM  %>%
  select(-"...1",
         -"...2",
         -"...3",
         -"...4",
         -"...5",
         -"...6")

keyword <- "Smoking"
matching_columns <- grep(keyword, names(Lifestyle_tableN), value = TRUE)
print(matching_columns)

Lifestyle_tableO  <- Lifestyle_tableN  %>%
  select(-"Smoking status | Instance 0",
         -"Smoking status | Instance 1",
         -"Smoking status | Instance 2",
         -"Smoking status | Instance 3")

Lifestyle_tableP  <- Lifestyle_tableO  %>%
  select(-"Alcohol intake frequency. | Instance 0",
         -"Alcohol intake frequency. | Instance 1",
         -"Alcohol intake frequency. | Instance 2",
         -"Alcohol intake frequency. | Instance 3")

keyword <- "activity"
matching_columns <- grep(keyword, names(Lifestyle_tableP), value = TRUE)
print(matching_columns)

Lifestyle_tableQ  <- Lifestyle_tableP  %>%
  select(-"Duration of moderate activity | Instance 1",
         -"Duration of moderate activity | Instance 0",
         -"Duration of moderate activity | Instance 2",
         -"Duration of moderate activity | Instance 3",
         -"Duration of vigorous activity | Instance 0",
         -"Duration of vigorous activity | Instance 2",
         -"Duration of vigorous activity | Instance 3",
         -"Duration of vigorous physical activity (pilot) | Instance 0",
         -"Usual walking pace | Instance 0",
         -"Usual walking pace | Instance 1",
         -"Usual walking pace | Instance 2",
         -"Usual walking pace | Instance 3")

keyword <- "termination"
matching_columns <- grep(keyword, names(Lifestyle_tableQ), value = TRUE)
print(matching_columns)

Lifestyle_tableR  <- Lifestyle_tableQ  %>%
  select(-"Ever had stillbirth, spontaneous miscarriage or termination | Instance 0",
         -"Ever had stillbirth, spontaneous miscarriage or termination | Instance 1",
         -"Ever had stillbirth, spontaneous miscarriage or termination | Instance 2",
         -"Ever had stillbirth, spontaneous miscarriage or termination | Instance 3",
         -"Number of stillbirths | Instance 0",
         -"Number of stillbirths | Instance 1",
         -"Number of stillbirths | Instance 2",
         -"Number of stillbirths | Instance 3",
         -"Number of spontaneous miscarriages | Instance 0",
         -"Number of spontaneous miscarriages | Instance 1",
         -"Number of spontaneous miscarriages | Instance 2",
         -"Number of spontaneous miscarriages | Instance 3",
         -"Number of pregnancy terminations | Instance 0",
         -"Number of pregnancy terminations | Instance 1",
         -"Number of pregnancy terminations | Instance 2",
         -"Number of pregnancy terminations | Instance 3")

keyword <- "oophorectomy"
matching_columns <- grep(keyword, names(Lifestyle_tableP), value = TRUE)
print(matching_columns)

Lifestyle_tableS  <- Lifestyle_tableR  %>%
  select(-"Ever had hysterectomy (womb removed) | Instance 1",
         -"Ever had hysterectomy (womb removed) | Instance 0",
         -"Ever had hysterectomy (womb removed) | Instance 2",
         -"Ever had hysterectomy (womb removed) | Instance 3",
         -"Age at hysterectomy | Instance 0",
         -"Age at hysterectomy | Instance 2",
         -"Age at hysterectomy | Instance 3",
         -"Age at hysterectomy | Instance 1",
         -"Bilateral oophorectomy (both ovaries removed) | Instance 0",
         -"Bilateral oophorectomy (both ovaries removed) | Instance 1",
         -"Bilateral oophorectomy (both ovaries removed) | Instance 2",
         -"Bilateral oophorectomy (both ovaries removed) | Instance 3")

keyword <- "mass index"
matching_columns <- grep(keyword, names(Lifestyle_tableP), value = TRUE)
print(matching_columns)

Lifestyle_tableT  <- Lifestyle_tableS  %>%
  select(-"Body mass index (BMI) | Instance 1",
         -"Body mass index (BMI) | Instance 3",
         -"Body mass index (BMI) | Instance 2")

keyword <- "Nap"
matching_columns <- grep(keyword, names(Lifestyle_tableP), value = TRUE)
print(matching_columns)

Lifestyle_tableU  <- Lifestyle_tableT  %>%
  select(-"Sleep duration | Instance 1",
         -"Sleep duration | Instance 3",
         -"Sleep duration | Instance 2",
         -"Sleeplessness / insomnia | Instance 1",
         -"Sleeplessness / insomnia | Instance 3",
         -"Sleeplessness / insomnia | Instance 2",
         -"Daytime dozing / sleeping | Instance 0",
         -"Daytime dozing / sleeping | Instance 1",
         -"Daytime dozing / sleeping | Instance 2",
         -"Daytime dozing / sleeping | Instance 3",
         -"Morning/evening person (chronotype) | Instance 0",
         -"Morning/evening person (chronotype) | Instance 1",
         -"Morning/evening person (chronotype) | Instance 2",
         -"Morning/evening person (chronotype) | Instance 3",
         -"Getting up in morning | Instance 0",
         -"Getting up in morning | Instance 1",
         -"Getting up in morning | Instance 2",
         -"Getting up in morning | Instance 3",
         -"Nap during day | Instance 0",
         -"Nap during day | Instance 1",
         -"Nap during day | Instance 2",
         -"Nap during day | Instance 3")

keyword <- "Diastolic"
matching_columns <- grep(keyword, names(Lifestyle_tableP), value = TRUE)
print(matching_columns)

Lifestyle_tableV  <- Lifestyle_tableU  %>%
  select(-"Systolic blood pressure, automated reading | Instance 0 | Array 0",
         -"Systolic blood pressure, automated reading | Instance 0 | Array 1",
         -"Systolic blood pressure, automated reading | Instance 1 | Array 0",
         -"Systolic blood pressure, automated reading | Instance 1 | Array 1",
         -"Systolic blood pressure, automated reading | Instance 2 | Array 0",
         -"Systolic blood pressure, automated reading | Instance 2 | Array 1",
         -"Systolic blood pressure, automated reading | Instance 3 | Array 0",
         -"Systolic blood pressure, automated reading | Instance 3 | Array 1",
         -"Systolic blood pressure, manual reading | Instance 0 | Array 0",   
         -"Systolic blood pressure, manual reading | Instance 1 | Array 0",   
         -"Systolic blood pressure, manual reading | Instance 0 | Array 1",   
         -"Systolic blood pressure, manual reading | Instance 1 | Array 1",   
         -"Systolic blood pressure, manual reading | Instance 2 | Array 0",   
         -"Systolic blood pressure, manual reading | Instance 2 | Array 1",   
         -"Systolic blood pressure, manual reading | Instance 3 | Array 0",   
         -"Systolic blood pressure, manual reading | Instance 3 | Array 1",
         -"Diastolic blood pressure, automated reading | Instance 0 | Array 0",
         -"Diastolic blood pressure, automated reading | Instance 0 | Array 1",
         -"Diastolic blood pressure, automated reading | Instance 1 | Array 0",
         -"Diastolic blood pressure, automated reading | Instance 1 | Array 1",
         -"Diastolic blood pressure, automated reading | Instance 2 | Array 0",
         -"Diastolic blood pressure, automated reading | Instance 2 | Array 1",
         -"Diastolic blood pressure, automated reading | Instance 3 | Array 0",
         -"Diastolic blood pressure, automated reading | Instance 3 | Array 1",
         -"Diastolic blood pressure, manual reading | Instance 0 | Array 0",   
         -"Diastolic blood pressure, manual reading | Instance 1 | Array 0",   
         -"Diastolic blood pressure, manual reading | Instance 0 | Array 1",   
         -"Diastolic blood pressure, manual reading | Instance 1 | Array 1",   
         -"Diastolic blood pressure, manual reading | Instance 2 | Array 0",   
         -"Diastolic blood pressure, manual reading | Instance 2 | Array 1",   
         -"Diastolic blood pressure, manual reading | Instance 3 | Array 0",   
         -"Diastolic blood pressure, manual reading | Instance 3 | Array 1")

keyword <- "Number of treatment"
matching_columns <- grep(keyword, names(Lifestyle_tableP), value = TRUE)
print(matching_columns)

Lifestyle_tableW  <- Lifestyle_tableV  %>%
  select(-"Number of treatments/medications taken | Instance 1",
         -"Number of treatments/medications taken | Instance 3",
         -"Number of treatments/medications taken | Instance 2")

#Combining instances:
#Had menopause
library(dplyr)

New_table <- Lifestyle_tableW %>%
  select("Participant ID", starts_with("Had menopause |"))

New_table1 <- New_table %>%
  mutate_at(vars(starts_with("Had menopause")), ~replace(., . %in% c("No", "Prefer not to answer"), NA))

long_table <- New_table1 %>%
  pivot_longer(
    cols = starts_with("Had menopause"),
    names_to = c(".value", "Instance"),
    names_pattern = "(.*) \\| Instance ([0-9]+)")

long_table$score <- ifelse(
  grepl("Yes", long_table$'Had menopause'), 4,
  ifelse(grepl("Not sure - had a hysterectomy", long_table$'Had menopause'), 3,
         ifelse(grepl("Not sure - other reason", long_table$'Had menopause'), 2,
                ifelse(grepl("NA", long_table$'Had menopause'), 1, NA)
         )
  )
)

max_meno_scores <- long_table %>%
  group_by(`Participant ID`) %>%
  summarise(score = max(score, na.rm = TRUE))

hadmeno_table <- max_meno_scores %>%
  mutate(score = case_when(
    score == 4 ~ "Yes",
    score == 3 ~ "Not sure - had a hysterectomy",
    score == 2 ~ "Not sure - other reason",
    is.infinite(score) ~ NA_character_,
    TRUE ~ as.character(score)
  )) %>%
  rename("Had menopause" = score)

Lifestyle_tableX <- left_join(Lifestyle_tableW, hadmeno_table, by = "Participant ID")

keyword <- "Had menopause"
matching_columns <- grep(keyword, names(Lifestyle_tableX), value = TRUE)
print(matching_columns)

Lifestyle_tableX  <- Lifestyle_tableX  %>%
  select(-"Had menopause | Instance 0",
         -"Had menopause | Instance 1",
         -"Had menopause | Instance 2",
         -"Had menopause | Instance 3")

#Ever used the contraceptive pill
Contra_table <- Lifestyle_tableX %>%
  select("Participant ID", starts_with("Ever taken oral"))

Contra_table1 <- Contra_table %>%
  mutate_at(vars(starts_with("Ever taken oral")), ~replace(., . %in% c("No", "Prefer not to answer", "Do not know"), NA))

long_Contra <- Contra_table1 %>%
  pivot_longer(
    cols = starts_with("Ever taken oral"),
    names_to = c(".value", "Instance"),
    names_pattern = "(.*) \\| Instance ([0-9]+)")

long_Contra$score <- ifelse(
  grepl("Yes", long_Contra$'Ever taken oral contraceptive pill'), 2,
            ifelse(grepl("NA", long_Contra$'Ever taken oral contraceptive pill'), 1, NA)
         )

Contra_scores <- long_Contra %>%
  group_by(`Participant ID`) %>%
  summarise(score = max(score, na.rm = TRUE))

Contrapill_table <- Contra_scores %>%
  mutate(score = case_when(
    score == 2 ~ "Yes",
    is.infinite(score) ~ NA_character_,
    TRUE ~ as.character(score)
  )) %>%
  rename("Ever taken oral contraceptive pill" = score)

Lifestyle_tableY <- left_join(Lifestyle_tableX, Contrapill_table, by = "Participant ID")

Lifestyle_tableY  <- Lifestyle_tableY  %>%
  select(-"Ever taken oral contraceptive pill | Instance 0",
         -"Ever taken oral contraceptive pill | Instance 1",
         -"Ever taken oral contraceptive pill | Instance 2",
         -"Ever taken oral contraceptive pill | Instance 3")

#Ever used HRT
HRT_table <- Lifestyle_tableY %>%
  select("Participant ID", starts_with("Ever used hormone"))

HRT_table1 <- HRT_table %>%
  mutate_at(vars(starts_with("Ever used hormone")), ~replace(., . %in% c("No", "Prefer not to answer", "Do not know"), NA))

long_HRT <- HRT_table1 %>%
  pivot_longer(
    cols = starts_with("Ever used hormone"),
    names_to = c(".value", "Instance"),
    names_pattern = "(.*) \\| Instance ([0-9]+)")

long_HRT$score <- ifelse(
  grepl("Yes", long_HRT$'Ever used hormone-replacement therapy (HRT)'), 2,
  ifelse(grepl("NA", long_HRT$'Ever used hormone-replacement therapy (HRT)'), 1, NA)
)

HRT_scores <- long_HRT %>%
  group_by(`Participant ID`) %>%
  summarise(score = max(score, na.rm = TRUE))

HRT_use_table <- HRT_scores %>%
  mutate(score = case_when(
    score == 2 ~ "Yes",
    is.infinite(score) ~ NA_character_,
    TRUE ~ as.character(score)
  )) %>%
  rename("Ever used hormone-replacement therapy (HRT)" = score)

Lifestyle_tableZ <- left_join(Lifestyle_tableY, HRT_use_table, by = "Participant ID")

Lifestyle_tableZ  <- Lifestyle_tableZ  %>%
  select(-"Ever used hormone-replacement therapy (HRT) | Instance 0",
         -"Ever used hormone-replacement therapy (HRT) | Instance 1",
         -"Ever used hormone-replacement therapy (HRT) | Instance 2",
         -"Ever used hormone-replacement therapy (HRT) | Instance 3")

#Ever used supplements
Supplement_table <- Lifestyle_tableZ %>%
  select("Participant ID", starts_with("Vitamin supplement"))

long_supps <- Supplement_table %>%
  pivot_longer(
    cols = starts_with("Vitamin supplement"),
    names_to = c(".value", "Instance"),
    names_pattern = "(.*) \\| Instance ([0-9]+)")

long_supps$score <- ifelse(
  grepl("Yes", long_supps$'Vitamin supplement user'), 3,
  ifelse(grepl("No", long_supps$'Vitamin supplement user'), 2,
            ifelse(grepl("NA", long_supps$'Vitamin supplement user'), 1, NA)
         )
  )

supps_scores <- long_supps %>%
  group_by(`Participant ID`) %>%
  summarise(score = max(score, na.rm = TRUE))

supps_table <- supps_scores %>%
  mutate(score = case_when(
    score == 3 ~ "Yes",
    score == 2 ~ "No",
    is.infinite(score) ~ NA_character_,
    TRUE ~ as.character(score)
  )) %>%
  rename("Vitamin supplement user" = score)

Lifestyle_table_red <- left_join(Lifestyle_tableZ, supps_table, by = "Participant ID")

Lifestyle_table_red  <- Lifestyle_table_red  %>%
  select(-"Vitamin supplement user | Instance 0",
         -"Vitamin supplement user | Instance 1",
         -"Vitamin supplement user | Instance 2",
         -"Vitamin supplement user | Instance 3",
         -"Vitamin supplement user | Instance 4")

#More removals:
keyword <- "Treatment"
matching_columns <- grep(keyword, names(Lifestyle_table_red), value = TRUE)
print(matching_columns)

Lifestyle_table_red  <- Lifestyle_table_red  %>%
  select(-"Time since last menstrual period | Instance 0",
         -"Time since last menstrual period | Instance 1",
         -"Time since last menstrual period | Instance 2",
         -"Time since last menstrual period | Instance 3",
         -"Age at first live birth | Instance 1",
         -"Age at first live birth | Instance 2",
         -"Age at first live birth | Instance 3",
         -"Vascular/heart problems diagnosed by doctor | Instance 1",
         -"Vascular/heart problems diagnosed by doctor | Instance 2",
         -"Diabetes diagnosed by doctor | Instance 1",
         -"Diabetes diagnosed by doctor | Instance 2",
         -"Diabetes diagnosed by doctor | Instance 3",
         -"Medication for cholesterol, blood pressure, diabetes, or take exogenous hormones | Instance 0",
         -"Medication for cholesterol, blood pressure, diabetes, or take exogenous hormones | Instance 1",
         -"Medication for cholesterol, blood pressure, diabetes, or take exogenous hormones | Instance 2",
         -"Medication for cholesterol, blood pressure, diabetes, or take exogenous hormones | Instance 3",
         -"Medication for cholesterol, blood pressure or diabetes | Instance 1",
         -"Medication for cholesterol, blood pressure or diabetes | Instance 2",
         -"Medication for cholesterol, blood pressure or diabetes | Instance 3",
         -"Treatment/medication code | Instance 1.x",
         -"Treatment/medication code | Instance 2.x",
         -"Treatment/medication code | Instance 3.x",
         -"Treatment/medication code | Instance 1.y",
         -"Treatment/medication code | Instance 2.y",
         -"Treatment/medication code | Instance 3.y",
         -"Cancer diagnosed by doctor | Instance 1",
         -"Cancer diagnosed by doctor | Instance 2",
         -"Cancer diagnosed by doctor | Instance 3",
         -"Cancer code, self-reported | Instance 1",
         -"Cancer code, self-reported | Instance 2",
         -"Cancer code, self-reported | Instance 3",
         -"Vitamin and mineral supplements | Instance 1",
         -"Vitamin and mineral supplements | Instance 2",
         -"Vitamin and mineral supplements | Instance 3",
         -"Vitamin and/or mineral supplement use | Instance 1",
         -"Vitamin and/or mineral supplement use | Instance 2",
         -"Vitamin and/or mineral supplement use | Instance 3",
         -"Vitamin and/or mineral supplement use | Instance 4")
         
Lifestyle_table_red  <- Lifestyle_table_red  %>%
  select(-"Cancer year/age first occurred | Instance 0 | Array 0",
         -"Cancer year/age first occurred | Instance 0 | Array 1",
         -"Cancer year/age first occurred | Instance 0 | Array 2",
         -"Cancer year/age first occurred | Instance 0 | Array 3",
         -"Cancer year/age first occurred | Instance 0 | Array 4",
         -"Cancer year/age first occurred | Instance 0 | Array 5",
         -"Cancer year/age first occurred | Instance 1 | Array 0",
         -"Cancer year/age first occurred | Instance 1 | Array 1",
         -"Cancer year/age first occurred | Instance 1 | Array 2",
         -"Cancer year/age first occurred | Instance 1 | Array 3",
         -"Cancer year/age first occurred | Instance 1 | Array 4",
         -"Cancer year/age first occurred | Instance 1 | Array 5",
         -"Cancer year/age first occurred | Instance 2 | Array 0",
         -"Cancer year/age first occurred | Instance 2 | Array 1",
         -"Cancer year/age first occurred | Instance 2 | Array 2",
         -"Cancer year/age first occurred | Instance 2 | Array 3",
         -"Cancer year/age first occurred | Instance 2 | Array 4",
         -"Cancer year/age first occurred | Instance 2 | Array 5",
         -"Cancer year/age first occurred | Instance 3 | Array 0",
         -"Cancer year/age first occurred | Instance 3 | Array 1",
         -"Cancer year/age first occurred | Instance 3 | Array 2",
         -"Cancer year/age first occurred | Instance 3 | Array 3",
         -"Cancer year/age first occurred | Instance 3 | Array 4",
         -"Cancer year/age first occurred | Instance 3 | Array 5")

#Merging Age at last live births:
keyword <- "Number of live births"
matching_columns <- grep(keyword, names(Lifestyle_tableJ), value = TRUE)
print(matching_columns)

columns_to_convert <- c("Number of live births | Instance 0", 
                        "Number of live births | Instance 1", 
                        "Number of live births | Instance 2",
                        "Number of live births | Instance 3")

# Use lapply to apply factor() to selected columns
Lifestyle_tableJ[columns_to_convert] <- lapply(Lifestyle_tableJ[columns_to_convert], factor)

# Then we will find the levels in these columns:

# Get levels of each factor column
levels(Lifestyle_tableJ$`Number of live births | Instance 0`)
levels(Lifestyle_tableJ$`Number of live births | Instance 1`)
levels(Lifestyle_tableJ$`Number of live births | Instance 2`)
levels(Lifestyle_tableJ$`Number of live births | Instance 3`)

# All have the same values

# Create a data frame with all instances in long format:
# First subset ID and birth data
subset_data <- Lifestyle_table_red %>%
  select(`Participant ID`,
         `Age at last live birth | Instance 0`,
         `Age at last live birth | Instance 1`,
         `Age at last live birth | Instance 2`,
         `Age at last live birth | Instance 3`)

subset_data_cleaned <- subset_data %>%
  mutate_at(vars(starts_with("Age at last live birth")), ~replace(., . %in% c("Do not know", "Prefer not to answer", "Do not remember"), NA))

long_births <- subset_data_cleaned %>%
  mutate_at(vars(starts_with("Age at last live birth")), as.numeric) %>%
  pivot_longer(
    cols = starts_with("Age at last live birth"),
    names_to = c(".value", "Instance"),
    names_pattern = "(.*) \\| Instance ([0-9]+)"
  )

last_birth_age <- long_births %>%
  group_by(`Participant ID`) %>%
  summarise(`Age at last live birth` = max(`Age at last live birth`, na.rm = TRUE))

Lifestyle_table_new <- setDT(Lifestyle_table_red)[setDT(last_birth_age), `Age at last live birth` := `Age at last live birth`, on = .(`Participant ID`)]

Lifestyle_table_new <- Lifestyle_table_new %>%
  mutate(`Age at last live birth` = replace(`Age at last live birth`, is.infinite(`Age at last live birth`), NA))

Lifestyle_table_new  <- Lifestyle_table_new  %>%
  select(-`Age at last live birth | Instance 0`,
         -`Age at last live birth | Instance 1`,
         -`Age at last live birth | Instance 2`,
         -`Age at last live birth | Instance 3`)

Lifestyle_table_new  <- Lifestyle_table_new  %>%
  select(-'...1',
         -'...2',
         -'...3')

#Joining on assessment centre dates and ages:        
# dx download Assessment_centre_info_participant.csv
Lifestyle_table_new = left_join(Lifestyle_table_new, Assessment_centre_info_participant, by = "Participant ID")

#Separating out medications
library(tidyr)

Lifestyle_table_sep <- Lifestyle_table_new %>%
  separate_rows(`Treatment/medication code | Instance 0.y`, sep = "\\|")

unique(Lifestyle_table_sep$`Treatment/medication code | Instance 0.y`)

Lifestyle_table_sep2 <- Lifestyle_table_sep %>%
  separate_rows(`Treatment/medication code | Instance 0.x`, sep = "\\|")

write.csv(Lifestyle_table_sep2, file= 'Lifestyle_table_sep2.csv')
#dx upload Lifestyle_table_sep2.csv

