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

print(matched_pairs)
