# Load necessary library
install.packages('dbplyr')
library(dbplyr)
install.packages('bigsnpr')
library(bigsnpr)
install.packages('here')
library('here')
library(yaml)
install.packages('kableExtra')
library(kableExtra)
library(data.table)
library(dplyr)
library(stringr)
library(foreign)
install.packages('RSQLite')
library(RSQLite)
19_44908822_C_T, 19_44908684_T_C
# Define paths and SNP IDs
bgenfiles <- "ukb22828_c19_b0_v3.bgen"
backingfile <- "apoe"
list_snp_id <- list(c("19_44908684_T_C", "19_44908822_C_T"))  # Corrected SNP IDs


# Define paths and SNP IDs
bgenfiles <- "ukb22438_c19_b0_v2.bgen"
backingfile <- "apoe"
list_snp_id <- list(c("rs429358", "rs7412"))  # Modify SNP IDs to match the correct format


# Read BGEN files into a bigSNP
snp_readBGEN(
  bgenfiles = bgenfiles,
  backingfile = backingfile,
  list_snp_id = list_snp_id,
  read_as = "dosage"
)

# Compute SNP statistics
bigsnp_obj <- snp_attach(backingfile)
snp_stats <- snp_stats(bigsnp_obj)

# Save SNP statistics to file
write.table(snp_stats, "snp-stats.txt", sep = "\t", quote = FALSE, row.names = FALSE)

# Generate genotype data and save to apoe.gen
make_genotype(bigsnp_obj, "apoe.gen")



# Write rsids to rsidlist.txt
cat("rs429358", file = "rsidlist.txt", append = FALSE, sep = "\n")
cat("rs7412", file = "rsidlist.txt", append = TRUE, sep = "\n")

# Execute bgenix command
system("/dev/ -incl-rsids rsidlist.txt > apoe.bgen")

# Execute qctool command for SNP stats
system("/apps/well/qctool/2.0.1/qctool -g apoe.bgen -snp-stats -osnp snp-stats.txt")

# Execute qctool command to generate apoe.gen
system("/apps/well/qctool/2.0.1/qctool -g apoe.bgen -og apoe.gen")

stats <- read.delim(file.path(config$data$prs, "APOE", "snp-stats.txt"),
                    sep = " ", comment.char = "#")

kable(stats %>% 
        select(rsid, alleleA, alleleB, HW_exact_p_value, minor_allele_frequency, impute_info, missing_proportion) %>%
        rename(HWE_pval = HW_exact_p_value,
               MAF = minor_allele_frequency),
      caption="Summary stats") %>%
  kable_styling(latex_options = "striped")