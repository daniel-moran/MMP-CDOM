## 
# before using this script you must have already run CDOM processing script on individual analytical batches of CDOM data auto-exported from the AIMS WQ Shimadzu UV-1900 UV-Vis spectrophotometer
# input required to the current script are the entire complement of processed CDOM data batches for a water year, where each scan in each analytical batch has been cleaned, checked for drift and corrections applied
##

# Daniel Moran
# AIMS
# update 06/07/21 (Alex Macadam)
# update 31/05/2023 ?
# update 12/06/2025 import to github MMP-CDOM repo
# update: 17/06/2025 moved this script to github repo MMP-CDOM/main and renamed to remove now redundant version control (no longer required with move to github).
# update:28/04/2026 created new branch (data_extract) and adapted the merge_files script to generating a data extract for external analysis

###
### extract drift corrected data for specific MMP stations, including modeled and observed data outputs 
### Note the data needs to be already processed using the appropriate CDOM_1_processing script
### Note that R does not like '\' so you will need to replace all the '\' in the filepath with '/'

### Note that the script collates data from any file with "model443_drift_corr" in the filename, from all subdirectories. If there are any precious processing batches in 

###  sub-directories make sure these are hidden by zipping
###



#set the wd
rm(list=ls())
setwd("R:/Lagoon_WQ/results/CDOM/2025-26 report/WQR") # set your working directory
wd <- getwd()

#load packages
library(tidyverse)

# create list of all files in the wd with model443 and Rsquared data fields
file_list_Acdom <- list.files(pattern="Acdom_drift_corr.csv", full.names=FALSE, recursive = TRUE) # recursive = TRUE allows list.files function to look in all subdirectories 
file_list_Acdom #check that the listed files matches your expectations!
## import all the Acdom .CSVs in the wd to a single dataframe
## create a "Filename" column containing the file name associated with each row 

for (x in file_list_Acdom){
  # create a dataframe for the merged data
  if (!exists("Acdom_all")){
    Acdom_all<- tibble(Wavelength_nm = seq(from = 250.0, to = 750.0, by = 0.5))
    }
  
  # now append all the data from the different csv files to this dataframe
  if (exists("Acdom_all")){
    temp_dataset <-ldply(x, read.csv, header=TRUE, skip=0, sep=',')
    Acdom_all <- Acdom_all  |> full_join(temp_dataset)
    rm(temp_dataset)
  }
  
}

View(CDOM_443_all)
#order by Sample name
sapply(CDOM_443_all, class)
CDOM_443_all$model_443 <- as.numeric(CDOM_443_all$model_443)
CDOM_443_all$Sample <- as.character(CDOM_443_all$Sample)
CDOM_443_all$r_squared <- as.numeric(CDOM_443_all$r_squared)
CDOM_443_all <- CDOM_443_all[order(CDOM_443_all$Sample),]
View(CDOM_443_all)

# if there are non-target samples from some of the analytical batches then remove them.
library(tidyverse)
CDOM_443_all <- CDOM_443_all |> filter(str_detect(Sample,"JCZ"))
View(CDOM_443_all)

#print the data
#path <- file.path(wd)
write.csv(CDOM_443_all,file="2024-25_JCZ_CDOM443.csv",row.names=F)
