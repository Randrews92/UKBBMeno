# Download APOE, men, meno:  
# dx download gen_apoe.csv
# dx download file-GjjJVj0JxG2gyY9Y0gkKGGx8
# dx download file-GjjJVK0JxG2j2Z84Q2Y2G0J3

# Lib 
library(dplyr)
library(data.table)

# read in csvs 

apoe <- read.csv("gen_apoe.csv")
meno <- read.csv("meno_death_filtered.csv")
men <- read.csv("men_only_filtered.csv")

# Work out APOE status using ifelse statement 

apoe$APOE4 <- ifelse(
  (apoe$rs429358 == "CC" & (apoe$rs7412 == "CC" | apoe$rs7412 == "TC")) | 
    (apoe$rs429358 == "TC" & (apoe$rs7412 == "TC" | apoe$rs7412 == "CC")),
  "Y",
  "N"
)

# Need to make sure the number matches the one indicated by Jennifer Collister: 138,574 

apoe%>%
  group_by(APOE4)%>%
  summarise(count=n_distinct(ID))

# Join APOE status by ID
men_apoe <- merge(men, apoe[, c("ID", "APOE4")], by.x = "Participant.ID", by.y = "ID", all.x = TRUE)

# Follow the processes above for joining APOE4 onto the meno table. 

# May be worth excluding NAs. 

# We also need to exclude withdrawn particpants- I'll send you over the current list via email. 

# Next steps can be to carry out the Cox regression on the meno only sample (while we're waiting for age-matching script)


