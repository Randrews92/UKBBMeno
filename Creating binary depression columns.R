install.packages('tidyverse') #dplyr and ggplot2 are already in tidyverse- causing issues so i removed them.
install.packages('data.table')

library('tidyverse')
library('data.table')

#dx download file-Gp3q7F8JyJPpyKvbpf9by1y1

depression <- read.csv("depression_participant.csv")

head(depression)

# Convert columns to Date format
depression$Date.F32.first.reported..depressive.episode. <- as.Date(depression$Date.F32.first.reported..depressive.episode.)
depression$Date.F33.first.reported..recurrent.depressive.disorder. <- as.Date(depression$Date.F33.first.reported..recurrent.depressive.disorder.)

# Combine the two date columns
depression$Combined.Date <- apply(depression, 1, function(row) {
  date1 <- row["Date.F32.first.reported..depressive.episode."]
  date2 <- row["Date.F33.first.reported..recurrent.depressive.disorder."]
  # Convert the dates from character to Date
  date1 <- as.Date(date1)
  date2 <- as.Date(date2)
  # Apply the logic to combine the dates
  if (is.na(date1) & is.na(date2)) {
    return(NA)
  } else if (is.na(date1)) {
    return(date2)
  } else if (is.na(date2)) {
    return(date1)
  } else {
    return(min(date1, date2))
  }
})

# Create a new column with "Y" or "N" based on Combined.Date
depression$Past_CurrentDepression <- ifelse(is.na(depression$Combined.Date), "N", "Y")

# View the updated dataset
print(depression)

# Subsetting columns using indexing
depression_columns <- depression[, c("Participant.ID", "Date.F32.first.reported..depressive.episode.","Date.F33.first.reported..recurrent.depressive.disorder.", "Combined.Date", "Has.Date")]


write.csv(depression_columns, file= 'Depression_columns.csv')
#dx upload Depression_columns.csv