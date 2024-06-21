#report info
library(dplyr)
mean_townsend <- mean(women_table$Townsend.deprivation.index.at.recruitment, na.rm = TRUE)
sd_townsend <- sd(women_table$Townsend.deprivation.index.at.recruitment, na.rm = TRUE)

cat("Average of Townsend.deprivation:", mean_townsend, "\n")
cat("Standard deviation of Townsend.deprivation:", sd_townsend, "\n")

#qual
qualscore_counts <- table(women_table$QualScore)
print(qualscore_counts)

#APOE
apoe_counts <- table(women_table$APOE4)
print(apoe_counts)

#Diet
dietscore_counts <- table(women_table$DietScore_binary)
print(dietscore_counts)

#ethnicity
ethnicity_counts <- table(women_table$Ethnic.background...Instance.0)
print(ethnicity_counts)

#Vitamin user
vits_counts <- table(women_table$Vitamin_or_Supplement_User)
print(vits_counts)

#smoking status
smoking_counts <- table(women_table$SmokingBaseline)
print(smoking_counts)

#alcohol consumption
alco_counts <- table(women_table$AlcoholBaseline)
print(alco_counts)

#MET
MET_counts <- table(women_table$MET_category)
print(MET_counts)

#Age range
AGE_range <- range(women_table$Year.of.birth, na.rm = TRUE)
AGE_range
