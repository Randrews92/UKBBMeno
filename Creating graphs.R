library(ggplot2)
library(dplyr)
library(tidyr)

# Load up your most recent lifestyle table:

T <- read.csv("Lifestyle_table_sep2.csv")

# filter to get just get women who've gone through menopause

meno <- T%>% filter(grepl("Yes", Had.menopause))


# Check the unqie values in medications:

unique(meno$Treatment.medication.code...Instance.0.x)

##### Create a frequency table of medications

# Assuming your data frame is named 'medication_data'
medication_freq <- table(meno$Participant.ID, meno$Treatment.medication.code...Instance.0.x)

# Convert the table to a data frame
medication_freq_df <- as.data.frame.matrix(medication_freq)

# Print or view the resulting frequency table
print(medication_freq_df)

# Assuming medication_freq_df is your frequency table data frame
medication_freq_sum <- colSums(medication_freq_df)

# Sort the medications by frequency in descending order
sorted_medication_freq <- sort(medication_freq_sum, decreasing = TRUE)

# Get a frequency tables of all medications
top_medication <- head(sorted_medication_freq, 2830)

# Create a data frame with medication names and frequencies
top_df <- data.frame(Medication = names(top_medication), Frequency = top_medication)

#### Vitamins 

# now we can have a look at vitamin supplements (this column will need to stacked before following next steps)

# Assuming your data frame is named 'medication_data'
vit_freq <- table(meno$Participant.ID, meno$Vitamin.and.mineral.supplements...Instance.0)

# Convert the table to a data frame
vit_freq_df <- as.data.frame.matrix(vit_freq)

# Print or view the resulting frequency table
print(vit_freq_df)

# Assuming vit_freq_df is your frequency table data frame
vit_freq_sum <- colSums(vit_freq_df)

# Sort the vits by frequency in descending order
sorted_vit_freq <- sort(vit_freq_sum, decreasing = TRUE)

# Get a frequency tables of all vits
top_vit <- head(sorted_vit_freq, 2830) # You can change 2830 to say 10 to just get the top 10 medications

# Create a data frame with vit names and frequencies
top_vit_df <- data.frame(vit = names(top_vit), Frequency = top_vit)

#### Pie chart looking at vitamin use prevalence

unique(meno$Vitamin.supplement.user)

# Recode NA to Unknown

meno$Vitamin.supplement.user<- ifelse(is.na(meno$Vitamin.supplement.user),"Unknown",  
                                      meno$Vitamin.supplement.user)

unique(meno$Vitamin.supplement.user)

meno$Vitamin.supplement.user1 <- as.factor(meno$Vitamin.supplement.user)

levels(meno$Vitamin.supplement.user1)

# Create a pie chart

meno %>%
  group_by(Vitamin.supplement.user1) %>%
  summarise(count = n_distinct(Participant.ID)) %>%
  ggplot(aes(x = "", y = count, fill = factor(Vitamin.supplement.user1))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Prevalence by Response",
       fill = "Response") +
  theme_minimal() +
  theme(legend.position = "right")


### Using these methods you can also look at other variables (I.e., create a pie chart of menopausal women compared to full sample of women)