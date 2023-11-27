## How to Table Export:

# In Terminal type in: dx run table-exporter

# You should be prompted to enter record ID, navigate to Projects in main UKBB platform, tick the cohort you want to export

# On the right-hand pain you'll see record ID, copy it, it should look like this: record-GY0PFzjJxG2Q3pgXq7QbVpfj

# This should give you optional parameters to change settings for your cohort.

# First type 0, and it will prompt you to name the dataset (it does not like spaces so use underscores) e.g. BigTable_1

# Then change the header-styles (otherwise they’re hard to read), type 3. 

# It will prompt you to choose from the following: "FIELD-NAME", "FIELD-TITLE", "UKB-FORMAT", "NONE"

# Type: FIELD-TITLE 

# It should show you the options with the changes you’ve made in green.

# Now press Enter and it will ask you to  “Confirm running the executable with this input [Y/n]:”

# Type Y and then it will ask you whether you want to watch it build your cohort, type Y

# It should start building your cohort (this takes a few minutes at least btw). 

# Once your cohort has been built (you will receive an email saying Table Exporter has finished) you’ll need to type this:
  
# dx download BigTable_1_participant.csv (or whatever the file is called). 

# Keep repeating this process until you have downloaded all the cohorts needed. 

# Make sure to follow the backup and restore instructions on the R workspace saving script (available in Git- Main branch). 

# This will ensure that all your downloaded cohorts are easily available when you log on, without you having to download them all again. 

# Later we will join these files by ID to create one big table in Rstudio, then we can start analysis. 

