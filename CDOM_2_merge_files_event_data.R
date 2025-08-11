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

###
### Combine model_443 and Rsquared outputs from each batch into a single dataframe and save as a .csv file
### Note that R does not like '\' so you will need to replace all the '\' in the filepath with '/'
### Note that the script collates data from any file with "model443_drift_corr" in the filename, from all subdirectories. If there are any precious processing batches in 
###  sub-directories make sure these are hidden by zipping
###



#collate JCU event data from the 24-25 season
#R:\Lagoon_WQ\results\CDOM\2024-25 report\JCF

#set the wd
rm(list=ls())
setwd("R:/Lagoon_WQ/results/CDOM/2024-25 report/JCF") # set your working directory
wd <- getwd()

# create list of all files in the wd with model443 and Rsquared data fields
# include both drift corrected data (AIMS lab) and non-corrected data (JCU)
file_list <- list.files(pattern="model443*", full.names=FALSE, recursive = TRUE) # recursive = TRUE allows list.files function to look in all subdirectories 

## import all the model443 .CSVs in the wd to a single dataframe
## create a "Filename" column containing the file name associated with each row 
library(plyr)
for (x in file_list){
  # create a dataframe for the merged data
  if (!exists("CDOM_443_all")){
    CDOM_443_all<-c()
    }
  
  # now append all the data from the different csv files to this dataframe
  if (exists("CDOM_443_all")){
    temp_dataset <-ldply(x, read.csv, header=TRUE, skip=0, sep=',',col.names=c('Sample','model_443','r_squared'))
    CDOM_443_all <- rbind(CDOM_443_all, temp_dataset)
    rm(temp_dataset)
  }
  
}

#order by Sample name
sapply(CDOM_443_all, class)
CDOM_443_all$model_443 <- as.numeric(CDOM_443_all$model_443)
CDOM_443_all$Sample <- as.character(CDOM_443_all$Sample)
CDOM_443_all$r_squared <- as.numeric(CDOM_443_all$r_squared)
CDOM_443_all <- CDOM_443_all[order(CDOM_443_all$Sample),]

# there are non-JCF samples from one of the AIMS batches. remove these
library(tidyverse)
CDOM_443_all <- CDOM_443_all |> filter(str_detect(Sample,"JCF"))


#print the data
#path <- file.path(wd)
write.csv(CDOM_443_all,file="2024-25_JCF_CDOM443.csv",row.names=F)
