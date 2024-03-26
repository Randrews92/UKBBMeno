
library(dplyr)

#Get outcomes from key_outcomes_with_death and baseline from lifestyle_table_new

outcomes <- read.csv('Key_outcomes_table_withdeath.csv')

baseline <- read.csv('Lifestyle_table_new.csv')

### Merge bilat/menopause ages so we have just one menopause age column 
# If data is entered in both just use the bilat oopherectomy data if it's smaller than meno age data:
#Amend this code below to include correct variable/ table names

baseline$mergedAge <- ifelse(!is.na(baseline$bilateral_oophorectomyAge) & baseline$bilateral_oophorectomyAge < baseline$menopauseAge, baseline$bilateral_oophorectomyAge, baseline$menopauseAge)

# Create a cohort who experienced menopause around the same time they were at instance 0 using the new merged meno age column
# Aim for within 5 years each way in women who did not have bilat ooph & from 0 years to 5 years in women who had bilat ooph.

baseline$yrsfromMeno <- baseline$menopauseAge-baseline$Age.at.recruitment

baseline$yrsfromBilat <- baseline$bilateral_oophorectomyAge-baseline$Age.at.recruitment

meno1 <- baseline%>%filter(between(yrsfromMeno, -5, 5))

meno2 <- baseline%>%filter(between(yrsfromBilat, 0, 5))

# This type of code will create a binary Y/N column for women who had a bilateral oopherectomy using the bilateral_oophorectomy_age column

baseline$Oophorectomy_Occurred <- ifelse(!is.na(baseline$bilateral_oophorectomyAge), "Y", "N")

# This code can be used for HRT, oral contraceptives and dementia once it's added. 
#HRT
baseline$HRT_Used <- ifelse(!is.na(baseline$Ever.used.hormone.replacement.therapy..HRT.), "Y", "N")
#contraceptives
baseline$Contraceptive_Used <- ifelse(!is.na(baseline$Ever.taken.oral.contraceptive.pill), "Y", "N")

# Identify all columns where someone reported vitamin/ mineral supplement use.
# Amend vitamin.supplement.user column so Yes for used supplements or No based on data from other vit/min columns. 
# Again this code below will need to be amended for the main meno cohort table you're using. 

baseline$recoded_vits1 <- ifelse(baseline$Vitamin.and.mineral.supplements...Instance.0 == "None of the above", "N",
                            ifelse(baseline$Vitamin.and.mineral.supplements...Instance.0 == "Prefer not to say", "Unknown", "Y"))

# Do similar to the above for any other vitamin/ mineral columns 

# Create a new column which checks if there's a Y in any of the vit/ min columns, if not it will input "Unknown" or "None of the above" from recoded_vits1
# Amended the code below using the tables/ column names you've used, you can also add additional columns if you've recoded more than one. 

baseline$recoded_vits3 <- ifelse(baseline$recoded_vits1== "Y" | baseline$recoded_vits2 == "Y", "Y", baseline$recoded_vits1)


# Next step is to identify dementia/ alzhiemers columns from outcomes (first occurrence dates should be fine)
# Then we can join those columns onto meno cohort on ID
# Future steps: we'll look at matching men and women, and look at controlling for APO-E. 


