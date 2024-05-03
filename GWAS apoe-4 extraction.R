# Load necessary library

library(yaml)
library(data.table)
library(dplyr)
library(stringr)
library(foreign)
install.packages("kableExtra")
library(kableExtra)

## Swiss Army Knife to extract filtered SNPs:
# First step is to create the apoe.gen and snp-stats.txt files.
# This needs to be done using command line interface in the RAP
# Go to Swiss Army Knife in tools library and select your project as output (input won't be required)
# When you get to the Command Line input this code: echo "rs429358" > rsidlist.txt echo "rs7412" >> rsidlist.txt
# This will create rsidlist.txt
# Go into Project > Bulk >  Imputation > UKB Imputation from Genotype
# Select Start Analysis in top right corner 
# Input files must be ukb22828_c19_b0_v3.bgen, rsidlist.txt, ukb22828_c19_b0_v3.bgen.bgi, ukb22828_c19_b0_v3.sample
# Input this in the Command Line: bgenix -g ukb22828_c19_b0_v3.bgen -incl-rsids rsidlist.txt > apoe.bgen
# This will create apoe.bgen
# Go into Project > Bulk >  Imputation > UKB Imputation from Genotype
# Select Start Analysis in top right corner 
# Input files must be apoe.bgen
# Input this code in command line: qctool -g apoe.bgen -snp-stats -osnp snp-stats.txt
# This will create snp-stats.txt
# Go into Project > Bulk >  Imputation > UKB Imputation from Genotype
# Select Start Analysis in top right corner 
# Input files:snp-stats.txt, apoe.bgen
# Input this code into Command Line: qctool -g apoe.bgen -og apoe.gen
# This will create apoe.gen

# NOTE: file names in Bulk are project specific, so for example "ukb22828_c19_b0_v3.bgen" will differ from other projects/ UKBB applications

## The next steps can be completed in R studio using Jennifer Collister's R Markdown script: 

# In terminal run: 
# dx download apoe.gen
# dx download snp-stats.txt
# dx download ukb22828_c19_b0_v3.sample

# Define the file path to your apoe.gen file
file_path <- "apoe.gen"

# Load the data from the apoe.gen file using fread()
gen <- fread(file_path)

# Transpose the data so that it's two columns instead of two rows
gen <- t(gen)

# Define the file path to your .sample file
sample_file_path <- "ukb22828_c19_b0_v3.sample"

# Read in the sample file using fread()
sample <- fread(sample_file_path)[-1,]

# Label the columns with the rsID_ref_effect
colnames(gen) <- paste0(gen[2,], "_", gen[5,], "_", gen[6,])

# Drop the header (first 6 rows: chr, UKB alt_id, rsID, pos, allele1, allele2)
# In UKB data, allele1 is the reference allele
gen <- data.frame(gen[-c(1:6),])

# Add participant IDs - there are three rows per participant
# Each triple of rows is the trio of genotype probabilities for that participant
gen$ID <- rep(sample$ID_1, each=3)
gen$prob <- rep(seq(1,3,1),nrow(gen)/3) 

# Split the data into the two SNPs
# The first genotype probability is homozygous reference allele
# The second is heterozygous
# The third is homozygous effect allele
gen_rs7412 <- gen %>%
  filter(rs7412_C_T != 0) %>% 
  mutate(rs7412 = dplyr::case_when(
    prob==1 ~ "CC",
    prob==2 ~ "TC",
    prob==3 ~ "TT",
    TRUE ~ "Other")) %>%
  select(ID, rs7412)

gen_rs429358 <- gen %>% 
  filter(rs429358_T_C != 0) %>%
  mutate(rs429358 = dplyr::case_when(
    prob==1 ~ "TT",
    prob==2 ~ "TC",
    prob==3 ~ "CC",
    TRUE ~ "Other")) %>%
  select(ID, rs429358)

gen_apoe <- dplyr::inner_join(gen_rs429358, gen_rs7412, by="ID")

# Create a summary table to compare with Jennifer Collister's output


genotypes <- data.frame(APOE_genotype = factor(c("e4e4", "e3e4", 
                                                 "e2e4 / e1e3", "e2e4 / e1e3", 
                                                 "e3e3", "e2e3", "e2e2", 
                                                 "e1e4", "e1e2"),
                                               levels=c("e4e4", "e3e4", 
                                                        "e2e4 / e1e3",
                                                        "e3e3", "e2e3", "e2e2", 
                                                        "e1e4", "e1e2")),
                        rs429358 = c("CC", "TC", 
                                     "TC", "CT", 
                                     "TT", "TT", "TT", 
                                     "CC", "TC"),
                        rs7412 = c("CC", "CC", 
                                   "TC", "TC", 
                                   "CC", "TC", "TT", 
                                   "TC", "TT"))

genotypes <- dplyr::inner_join(apoe, genotypes, by=c("rs7412", "rs429358")) %>%
  mutate(e4 = str_count(APOE_genotype, "4"))

gen_count <- genotypes %>% 
  count(APOE_genotype, rs429358, rs7412)

kable(gen_count) %>%
  kable_styling(latex_options = "striped")

# Table matches perfectly with Jennifer Collister's output, confirming the data is sound. 

# write csv 
write.csv(gen_apoe, file = "gen_apoe.csv", row.names = FALSE)

# in Terminal type: dx upload gen_apoe.csv 
# Now the APOE participant list is saved in Projects and can be joined onto prepped cohorts. 

