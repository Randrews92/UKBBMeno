# Saving environments

# First save your environment in your R script (not terminal): save.image(file='myEnvironment.RData')

# In terminal put: dx-backup-folder

# It should say Folder . was successfully saved to 
# /.Backups/rstudio_workbench_ukbrap_trial.Randrews92.2023-11-23T11-45-01.tar.gz

# Now on the main UKBB platform go into Projects, and look for a folder called .Backups
# Look for the file you've backed up- it will have this name format matching the one above (but with your name and time):
# rstudio_workbench_ukbrap_trial.Randrews92.2023-11-23T11-45-01.tar.gz

#To test whether the backup is successful, Terminate session and restart a session. 

# To restore in Terminal put (or whatever the name of your backup file is): 
#dx-restore-folder /.Backups/rstudio_workbench_ukbrap_trial.Randrews92.2023-11-23T11-45-01.tar.gz

# This should restore your files. 

# In the Files tab you should have the Environment data e.g. myEnvironement.RData

# You can load it which will give you all your data files back in the top right Environment tab. 

# You can't save packages annoyingly, but i've tweaked the code (realised where the errors were coming from)
# Amend your Vits and mins.R file to have the following code for installing and loading packages:

install.packages('tidyverse') #dplyr and ggplot2 are already in tidyverse- causing issues so i removed them.
install.packages('data.table')
install.packages('broom.helpers') #this is needed to install gtsummary
install.packages('gtsummary')

# Always install before loading- as UKBB has some packages availble but not others, so loading first before installing will break it. 

# load packages with library:

library('tidyverse')
library('data.table')
library('gtsummary')

# When you've finished working on your script make sure to:
# 1. save.image(file='myEnvironment.RData')
# 2. dx-backup-folder
# 3. Check folder is backed up in the main platform in .Backups
# When you start a new session follow the instructions above to restore your files. 

