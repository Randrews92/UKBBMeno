# Assuming your data frame is called 'data' and it contains columns 'had.menopause' and 'medication'

# Step 1: Create a table with counts
count_table <- table(data$had.menopause, is.na(data$medication))

# Step 2: Calculate percentages
percentage <- prop.table(count_table, margin = 1) * 100  # Convert to percentages

# Step 3: Create a data frame with percentages
percentage_df <- data.frame(
  had_menopause = rep(rownames(percentage), each = ncol(percentage)),
  takes_medication = rep(c("Takes Medication", "Doesn't Take Medication"), times = nrow(percentage)),
  percentage = c(percentage)
)

# Step 4: Plot the percentages
library(ggplot2)
ggplot(percentage_df, aes(x = had_menopause, y = percentage, fill = takes_medication)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Percentage of Participants Taking Medication",
       x = "Response to 'Had Menopause'",
       y = "Percentage") +
  scale_y_continuous(labels = scales::percent_format()) +  # Format y-axis as percentage
  scale_fill_manual(values = c("green", "red"),
                    labels = c("Takes Medication", "Doesn't Take Medication")) +
  theme_minimal()




#Number of medications and menopause status
library(ggplot2)

# Create a box plot
ggplot(data, aes(x = Had.menopause, y = `Number of medications`, fill = Had.menopause)) +
  geom_boxplot() +
  labs(title = "Number of Medications by Had Menopause",
       x = "Had Menopause",
       y = "Number of Medications") +
  theme_minimal()

#Sleeplessness and menpause
T$Treatment.Had.menopause[is.na(T$Had.menopause)] <- "No"

T_sleepless <- T[!T$`Sleeplessness...insomnia...Instance.0` %in% c("NA", "Prefer not to answer"), ]
T_sleepless <- T_sleepless[!T$`Had.menopause` %in% c("Not sure - other reason"), ]
T_sleepless$Had.menopause[is.na(T_sleepless$Had.menopause)] <- "No"

ggplot(T_sleepless, aes(x = Had.menopause, fill = `Sleeplessness...insomnia...Instance.0`)) +
  geom_bar(position = "dodge") +
  labs(title = "Sleeplessness by Menopause Status",
       x = "Menopause Status",
       y = "Frequency",
       fill = "Sleeplessness Level")

