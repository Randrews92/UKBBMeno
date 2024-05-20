library(dplyr)

# Initialize an empty data frame to store matches
matched_pairs <- data.frame(ID_woman = integer(), ID_man = integer(), Year.of.birth = integer())

# Loop through each woman to find a match
for (i in 1:nrow(women_apoe)) {
  potential_matches <- men_apoe %>%
    filter(Year.of.birth == women_apoe$Year.of.birth[i])
  
  # pick the first match
  if (nrow(potential_matches) > 0) {
    matched_man <- potential_matches[1, ]
    
    # Add the matched pair to the matched_pairs data frame
    matched_pairs <- rbind(matched_pairs, data.frame(
      ID_woman = women_apoe$Participant.ID[i],
      ID_man = matched_man$Participant.ID,
      Year.of.birth = women_apoe$Year.of.birth[i]
    ))
    
    # Remove the matched man from the men data frame
    men_apoe <- men_apoe %>%
      filter(Participant.ID != matched_man$Participant.ID)
  }
}


##option 2
library(dplyr)
library(data.table)

# Assume meno and men data frames are already fetched via sqlQuery
# meno <- sqlQuery(channel, ...)
# men <- sqlQuery(channel, ...)

# Convert men to a data.table keyed by year_wob
controlAllPeople.pool <- data.table(men_apoe, key = "year_wob")

# Initialize an empty data frame to store matches
matchedAge <- data.frame()

# Loop through each row of meno to find a match
for (i in 1:nrow(meno)) {
  # Find potential matches for the current meno row
  potential_matches <- controlAllPeople.pool[year_wob == meno$year_wob[i]]
  
  # If there are potential matches, pick the first one
  if (nrow(potential_matches) > 0) {
    matched_man <- potential_matches[1, ]
    
    # Combine the current meno row and the matched man into one data frame
    matched_pair <- cbind(meno[i, ], matched_man)
    
    # Add the matched pair to the matchedAge data frame
    matchedAge <- rbind(matchedAge, matched_pair)
    
    # Remove the matched man from controlAllPeople.pool
    controlAllPeople.pool <- controlAllPeople.pool[ALF_PE != matched_man$ALF_PE]
  }
  
  # Print progress every 100 iterations
  if (i %% 100 == 0) {
    print(i)
  }
}

# Select and rename columns to match the final output
controlcohortFinished <- matchedAge %>% select(ALF_PE.x, ALF_PE.y)
colnames(controlcohortFinished) <- c("ALF_PE", "control_ALF_PE")

# Create table in the database
SAILDButils::create_table_from_df(channel, 'SAILW1448v.controlcohort', controlcohortFinished)

# View the matched pairs
print(controlcohortFinished)
