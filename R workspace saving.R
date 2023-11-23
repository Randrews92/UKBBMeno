# Saving environments

# First save your environment in your R script (not terminal): save.image(file='myEnvironment.RData')

# In terminal put: dx-backup-folder

# It should say Folder . was successfully saved to 
# /.Backups/rstudio_workbench_ukbrap_trial.Randrews92.2023-11-23T11-45-01.tar.gz

# Now on the main UKBB platform go into Projects, and look for a folder called .Backups
# Look for the file you've backed up- it will have this name format matching the one above:
# rstudio_workbench_ukbrap_trial.Randrews92.2023-11-23T11-45-01.tar.gz

#To test whether the backup is successful, Terminate session and restart a session. 

# To restore in Terminal put (or whatever the name of your file is): 
#dx-restore-folder /.Backups/rstudio_workbench_ukbrap_trial.Randrews92.2023-11-23T11-45-01.tar.gz

# This should restore your file. 

# In the Files tab you should have the Environement data e.g. myEnvironement.RDate

# You can load it which will give you all your data objects back in the top right Environment tab. 

