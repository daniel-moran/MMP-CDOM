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
library(plyr)
library(tidyverse)
library(car)

# create list of all files in the wd with model443 and Rsquared data fields
file_list_Acdom <- list.files(pattern="Acdom_drift_corr.csv", full.names=FALSE, recursive = TRUE) # recursive = TRUE allows list.files function to look in all subdirectories 
file_list_Acdom #check that the listed files matches your expectations!

## import all the Acdom .CSVs in the wd to a single dataframe

for (x in file_list_Acdom){
  # create a dataframe for the merged data
  if (!exists("Acdom_all")){
    Acdom_all<- tibble(Wavelength_nm = seq(from = 250.0, to = 750.0, by = 0.5))
    }
  
  # now append all the data from the different csv files to this dataframe
  if (exists("Acdom_all")){
    temp_dataset <- ldply(x, read.csv, header=TRUE, skip=0, sep=',')
    Acdom_all <- Acdom_all  |> full_join(temp_dataset, by = join_by(Wavelength_nm))
    rm(temp_dataset)
  }
  
}

#filter the data to be extracted
names(Acdom_all)
all_stations <- names(Acdom_all)
target_list <- all_stations[grepl("Wav|WQR405_D0|WQR406_D0", all_stations)]
Acdom_subset <- Acdom_all |> select(all_of(target_list)) |> as_tibble() 
names(Acdom_subset)
Acdom_subset <- dplyr::rename(Acdom_subset, WQR405_D0_1 = WQR405_D0, WQR406_D0_1 = WQR406_D0)
names(Acdom_subset)
stn_order <- names(Acdom_subset)[order(names(Acdom_subset))]
stn_order <- c("Wavelength_nm", "WQR405_D0_1", "WQR405_D0_2", "WQR405_D0_3", "WQR405_D0_4", "WQR405_D0_5", "WQR405_D0_6", "WQR405_D0_7", 
               "WQR405_D0_8", "WQR405_D0_9", "WQR405_D0_10", "WQR405_D0_11",  "WQR405_D0_12", "WQR406_D0_1", "WQR406_D0_2", "WQR406_D0_3", 
               "WQR406_D0_4", "WQR406_D0_5", "WQR406_D0_6", "WQR406_D0_7", "WQR406_D0_8", "WQR406_D0_9", "WQR406_D0_10", "WQR406_D0_11", 
               "WQR406_D0_12")
Acdom_subset <- Acdom_subset |> select(all_of(stn_order))

#print the data
#path <- file.path(wd)
write.csv(Acdom_subset,file="CDOM_holding_time_ACDOM_driftcorr.csv",row.names=F)

#Start by having a look at all the scans plotted together
#convert to long format and arrange in order
names(Acdom_subset)
Acdom_subset_long <- Acdom_subset |> pivot_longer(WQR405_D0_1:WQR406_D0_12, names_to = "sample_id", values_to = "Acdom") 
names(Acdom_subset_long)
Acdom_subset_long <- Acdom_subset_long |> dplyr::rename(sample_id_x = sample_id)
Acdom_subset_long <- Acdom_subset_long |> mutate(sample_id_rep = str_sub(sample_id_x, start = 11))
Acdom_subset_long <- Acdom_subset_long |> mutate(sample_id = str_sub(sample_id_x, start = 1, end = 10))
Acdom_subset_long$sample_id_rep <- str_pad(Acdom_subset_long$sample_id_rep, width = 2, pad = "0") 
Acdom_subset_long <- Acdom_subset_long |> mutate(sample_id = paste0(sample_id, sample_id_rep))
Acdom_subset_long <- Acdom_subset_long |> mutate(station = str_sub(sample_id_x, start = 1, end = 6))
Acdom_subset_long$sample_id_x <- NULL
Acdom_subset_long <- Acdom_subset_long |> relocate(sample_id, .after = Wavelength_nm)
Acdom_subset_long <- Acdom_subset_long |> 
  mutate(analysis_time = case_when(
    sample_id_rep %in% c("01", "02", "03", "04") ~ "batch_1",
    sample_id_rep %in% c("05", "06", "07", "08") ~ "batch_2",
    sample_id_rep %in% c("09", "10", "11", "12") ~ "batch_3"))
Acdom_subset_long <- Acdom_subset_long |> group_by(analysis_time)

#Acdom_subset_long$sample_id_rep <- NULL



## plot the data organised by samples, to visually compare

plot1 <- ggplot(Acdom_subset_long, aes(x = Wavelength_nm, y = Acdom, group = sample_id, colour = sample_id)) + 
    geom_line(linewidth = 0.1) +
    ylab('Acdom') +
    xlab('Wavelength_nm') +
    #scale_y_continuous(minor_breaks = NULL, breaks=seq(-0.02,0.02,0.01),limits=c(-0.02,0.02)) +
    #scale_x_continuous(minor_breaks = NULL, breaks=seq(250,750,50), limits=c(250,750)) +
    theme_gray() 
plot1

WQR405_dat <- Acdom_subset_long  |> filter(grepl("WQR405", sample_id))
plot_WQR405 <- ggplot(WQR405_dat, aes(x = Wavelength_nm, y = Acdom, group = sample_id, colour = analysis_time)) + 
  geom_line(linewidth = 0.1) +
  ylab('Acdom') +
  xlab('Wavelength_nm') +
  #scale_y_continuous(minor_breaks = NULL, breaks=seq(-0.02,0.02,0.01),limits=c(-0.02,0.02)) +
  #scale_x_continuous(minor_breaks = NULL, breaks=seq(250,750,50), limits=c(250,750)) +
  theme_gray() 
plot_WQR405

plot_WQR405_2 <- ggplot(WQR405_dat, aes(x = Wavelength_nm, y = Acdom, group = sample_id, colour = analysis_time)) + 
  geom_line(linewidth = 0.1) +
  ylab('Acdom') +
  xlab('Wavelength_nm') +
  scale_y_continuous(minor_breaks = NULL, breaks=seq(-0,1,0.1),limits=c(-0,0.6)) +
  scale_x_continuous(minor_breaks = NULL, breaks=seq(250,750,50), limits=c(300,400)) +
  theme_gray() 
plot_WQR405_2

plot_WQR405_3 <- ggplot(WQR405_dat, aes(x = Wavelength_nm, y = Acdom, group = sample_id, colour = analysis_time)) + 
  geom_line(linewidth = 0.1) +
  ylab('Acdom') +
  xlab('Wavelength_nm') +
  scale_y_continuous(minor_breaks = NULL, breaks=seq(-0,1,0.1),limits=c(-0,0.3)) +
  scale_x_continuous(minor_breaks = NULL, breaks=seq(250,750,50), limits=c(400,500)) +
  theme_gray() 
plot_WQR405_3

WQR406_dat <- Acdom_subset_long  |> filter(grepl("WQR406", sample_id))
plot_WQR406 <- ggplot(WQR406_dat, aes(x = Wavelength_nm, y = Acdom, group = sample_id, colour = analysis_time)) + 
  geom_line(linewidth = 0.1) +
  ylab('Acdom') +
  xlab('Wavelength_nm') +
  #scale_y_continuous(minor_breaks = NULL, breaks=seq(-0.02,0.02,0.01),limits=c(-0.02,0.02)) +
  #scale_x_continuous(minor_breaks = NULL, breaks=seq(250,750,50), limits=c(250,750)) +
  theme_gray() 
plot_WQR406

plot_WQR406_2 <- ggplot(WQR406_dat, aes(x = Wavelength_nm, y = Acdom, group = sample_id, colour = analysis_time)) + 
  geom_line(linewidth = 0.1) +
  ylab('Acdom') +
  xlab('Wavelength_nm') +
  scale_y_continuous(minor_breaks = NULL, breaks=seq(-0,11,1),limits=c(-0,6)) +
  scale_x_continuous(minor_breaks = NULL, breaks=seq(250,750,50), limits=c(300,400)) +
  theme_gray() 
plot_WQR406_2

plot_WQR406_3 <- ggplot(WQR406_dat, aes(x = Wavelength_nm, y = Acdom, group = sample_id, colour = analysis_time)) + 
  geom_line(linewidth = 0.1) +
  ylab('Acdom') +
  xlab('Wavelength_nm') +
  scale_y_continuous(minor_breaks = NULL, breaks=seq(-0,11,1),limits=c(-0,1.2)) +
  scale_x_continuous(minor_breaks = NULL, breaks=seq(250,750,50), limits=c(400,500)) +
  theme_gray() 
plot_WQR406_3

#save the plots as reqd

#extract Acdom_443, compare using multivariate analysis 

#subset the data
Acdom_443 <- Acdom_subset |> filter(Wavelength_nm == 443.0)

#make the data long/tidy, arrange, and create factor groups
names(Acdom_443)
Acdom_443_long <- Acdom_443 |> pivot_longer(WQR405_D0_1:WQR406_D0_12, names_to = "sample_id", values_to = "Acdom") 
names(Acdom_443_long)
Acdom_443_long <- Acdom_443_long |> dplyr::rename(sample_id_x = sample_id)
Acdom_443_long <- Acdom_443_long |> mutate(sample_id_rep = str_sub(sample_id_x, start = 11))
Acdom_443_long <- Acdom_443_long |> mutate(sample_id = str_sub(sample_id_x, start = 1, end = 10))
Acdom_443_long$sample_id_rep <- str_pad(Acdom_443_long$sample_id_rep, width = 2, pad = "0") 
Acdom_443_long <- Acdom_443_long |> mutate(sample_id = paste0(sample_id, sample_id_rep))
Acdom_443_long <- Acdom_443_long |> mutate(station = str_sub(sample_id_x, start = 1, end = 6))
Acdom_443_long$sample_id_x <- NULL
Acdom_443_long <- Acdom_443_long |> relocate(sample_id, .after = Wavelength_nm)
Acdom_443_long <- Acdom_443_long |> 
  mutate(analysis_time = case_when(
    sample_id_rep %in% c("01", "02", "03", "04") ~ "batch_1",
    sample_id_rep %in% c("05", "06", "07", "08") ~ "batch_2",
    sample_id_rep %in% c("09", "10", "11", "12") ~ "batch_3"))
Acdom_443_long <- Acdom_443_long |> group_by(station, analysis_time)
#Acdom_subset_long$sample_id_rep <- NULL

group_keys(Acdom_443_long)

Acdom_443_long <- Acdom_443_long |> 
  mutate(
    station = factor(station),
    analysis_time = factor(analysis_time, levels = c("batch_1", "batch_2", "batch_3")))
levels(Acdom_443_long$station)
levels(Acdom_443_long$analysis_time)

#2-way ANOVA
#guidance taken from https://statsandr.com/blog/two-way-anova-in-r/

#outline hypotheses
## Main effect of station on Absorption coefficient:
##H0: Acdom is equal between stations (open coastal vs mid shelf)
##H1: Acdom is different between stations (open coastal vs mid shelf)
## Main effect of timing of the analysis on Absorption coefficient:
##H0: Acdom is equal between all 3 time points
##H1: Acdom is different for at least one time point
## Interaction between station and timing of the analysis:
##H0: there is no interaction between station and timing of the analysis, meaning that the relationship between timing of the analysis and Acdom is the same for open coastal and mid shelf (similarly, the relationship between station and Acdom is the same for all 3 analysis time points)
##H1: there is an interaction between station and analysis timing, meaning that the relationship between analysis timing and Acdom is different for open coastal than for mid shelf (similarly, the relationship between station and Acdom depends on the timing of the analysis)

#test assumptions
#
#variable types
##Acdom absorption at 443nm is the dependant variable and is quantitative
##station is categorical variable with two levels based on the station: WQR405 and WQR 406. note that the stations were chosen to represent different water bodies (WQR405 = mid shelf; WQR406 = open coastal), but also represent CDOM load (WQR405 = low; WQR406 = high) and reactivity (WQR405 = more refractory carbon; WQR406 = more labile carbon) which typically vary between water body's
##analysis_time is categorical variable with three levels based on timing of the analysis of the replicates. batch_1 were analysed asap (within 2 days - document actual timing); batch_2 were analysed after ~ 1 month; batch_3 were analysed after ~  months. 
#
#Independence
##twelve seperate replicates were subsampled from a single niskin bottle collected at each station. each replicate was individually filtered following stnadard MMP methods (ref). four seperate replicates were analysed and then discarded for each batch of analysis. No samples were analysed more than 1 time. The replicate samples are be independent between groups and within each group.
#
#Normality
##set up the Anova 1st
#two-way ANOVA
names(Acdom_443_long)
ANOVA_443 <- aov(Acdom ~ station * analysis_time, data = Acdom_443_long) #
#
#QQ plot of the residuals
car::qqPlot(ANOVA_443$residuals,
       id = FALSE # remove point identification
)
##points appear to vary some around the line (Henry's line) but are within the confidence interval...review w Renee or maybe Angus or Murray...
#
# histogram of the residuals should show normal distribution
hist(ANOVA_443$residuals) #its kind of skewed to the right...review
#
#Shapiro-Wilk normality test
shapiro.test(ANOVA_443$residuals)
##data:  ANOVA_443$residuals
##W = 0.9693, p-value = 0.6498
## We do not reject the null hypothesis that the residuals follow a normal distribution (p-value = 0.6498)
# normality tests confirm normality but I do have some questions esp re the histogram. the dataset is somewhat small though. review and discuss potential/merit of data transformation
#
# Homogeneity of variances
#check homogeneity of variances or homoscedasticity by visualisation
plot(ANOVA_443, which = 3)
#seems ok to me
# confirm with Levene’s test
car::leveneTest(ANOVA_443)
#Levene's Test for Homogeneity of Variance (center = median)
#      Df F value Pr(>F)
#group  5  0.3218 0.8933
#      18       
# We do not reject the null hypothesis that the variances are equal (p-value = 0.8933)

#visualise using boxplots, check for extreme outliers
WQR405_plot443 <- Acdom_443_long |> filter(station == "WQR405") |> 
  ggplot(aes(x = analysis_time, y = Acdom)) + 
  geom_boxplot() +
  ylab('Acdom') +
  xlab('analysis_timing')
WQR405_plot443

WQR406_plot443 <- Acdom_443_long |> filter(station == "WQR406") |> 
  ggplot(aes(x = analysis_time, y = Acdom)) + 
  geom_boxplot() +
  ylab('Acdom') +
  xlab('analysis_timing')
WQR406_plot443

plot443 <- ggplot(Acdom_443_long, aes(x = analysis_time, y = Acdom)) + 
  geom_boxplot() +
  ylab('Acdom') +
  xlab('analysis_timing') + 
  facet_wrap(~station, scales = "free_y")
plot443
# there appears to be some outliers but I dont think they are extreme so suggest accepting them
# there appears to be an interaction, based purely on the visualisation

#run Anova summary
summary(ANOVA_443)
#                        Df Sum Sq Mean Sq   F value   Pr(>F)    
#  station                1 1.4786  1.4786 50981.129  < 2e-16 ***
#  analysis_time          2 0.0003  0.0002     5.541 0.013330 *  
#  station:analysis_time  2 0.0007  0.0003    11.402 0.000633 ***
#  Residuals             18 0.0005  0.0000                       
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Run Tukey post-hoc test
TukeyHSD(ANOVA_443) 

#Tukey multiple comparisons of means
#95% family-wise confidence level
#
#Fit: aov(formula = Acdom ~ station * analysis_time, data = Acdom_443_long)
#
#$station
#diff      lwr      upr p adj
#WQR406-WQR405 0.496415 0.491796 0.501034     0
#
#$analysis_time
#diff          lwr           upr     p adj
#batch_2-batch_1 -0.006268892 -0.013141068  0.0006032835 0.0772906
#batch_3-batch_1 -0.008683173 -0.015555348 -0.0018109970 0.0124203
#batch_3-batch_2 -0.002414281 -0.009286456  0.0044578952 0.6492768
#
#$`station:analysis_time`
#diff          lwr           upr     p adj
#WQR406:batch_1-WQR405:batch_1  0.505860808  0.493758743  0.5179628728 0.0000000
#WQR405:batch_2-WQR405:batch_1 -0.004144847 -0.016246912  0.0079572181 0.8796842
#WQR406:batch_2-WQR405:batch_1  0.497467870  0.485365805  0.5095699353 0.0000000
#WQR405:batch_3-WQR405:batch_1  0.003361464 -0.008740601  0.0154635288 0.9458824
#WQR406:batch_3-WQR405:batch_1  0.485132999  0.473030934  0.4972350636 0.0000000
#WQR405:batch_2-WQR406:batch_1 -0.510005655 -0.522107720 -0.4979035897 0.0000000
#WQR406:batch_2-WQR406:batch_1 -0.008392937 -0.020495003  0.0037091275 0.2832486
#WQR405:batch_3-WQR406:batch_1 -0.502499344 -0.514601409 -0.4903972790 0.0000000
#WQR406:batch_3-WQR406:batch_1 -0.020727809 -0.032829874 -0.0086257442 0.0004416
#WQR406:batch_2-WQR405:batch_2  0.501612717  0.489510652  0.5137147822 0.0000000
#WQR405:batch_3-WQR405:batch_2  0.007506311 -0.004595754  0.0196083757 0.3948281
#WQR406:batch_3-WQR405:batch_2  0.489277846  0.477175780  0.5013799105 0.0000000
#WQR405:batch_3-WQR406:batch_2 -0.494106407 -0.506208472 -0.4820043415 0.0000000
#WQR406:batch_3-WQR406:batch_2 -0.012334872 -0.024436937 -0.0002328067 0.0442879
#WQR406:batch_3-WQR405:batch_3  0.481771535  0.469669470  0.4938735999 0.0000000

#Tukey test again but with only the facotr with 3 levels - analysis_time
TukeyHSD(ANOVA_443, which = "analysis_time") # run the tukey test on the factor analysis_time since there are only 2 levels of station so its not required there
#there is a significant difference between batch_1 and batch_3
#Tukey multiple comparisons of means
#95% family-wise confidence level
#
#Fit: aov(formula = Acdom ~ station * analysis_time, data = Acdom_443_long)
#
#$analysis_time
#diff          lwr           upr     p adj
#batch_2-batch_1 -0.006268892 -0.013141068  0.0006032835 0.0772906
#batch_3-batch_1 -0.008683173 -0.015555348 -0.0018109970 0.0124203
#batch_3-batch_2 -0.002414281 -0.009286456  0.0044578952 0.6492768


#Tukey test on the interaction only - analysis_time
TukeyHSD(ANOVA_443, which = "station:analysis_time") # run the tukey test on the interaction 
#there are some meaningless pairwise comparisons so we should ignore those (review).
#meaningful outputs:
#$`station:analysis_time`
#                                  diff          lwr           upr     p adj
#WQR405:batch_2-WQR405:batch_1 -0.004144847 -0.016246912  0.0079572181 0.8796842
#WQR405:batch_3-WQR405:batch_1  0.003361464 -0.008740601  0.0154635288 0.9458824
#WQR406:batch_2-WQR406:batch_1 -0.008392937 -0.020495003  0.0037091275 0.2832486
#WQR406:batch_3-WQR406:batch_1 -0.020727809 -0.032829874 -0.0086257442 0.0004416

##To do


#review analysis options - look at hierarchical design...

#pick apart the interaction between station and analysis_time more - from the tukey test the relevent comparisons are:
##  `station:analysis_time`       diff          lwr          upr          p adj
##  WQR405:batch_2-WQR405:batch_1 -0.004144847 -0.016246912  0.0079572181 0.8796842 
##  WQR405:batch_3-WQR405:batch_1  0.003361464 -0.008740601  0.0154635288 0.9458824
##  WQR406:batch_2-WQR406:batch_1 -0.008392937 -0.020495003  0.0037091275 0.2832486
##  WQR406:batch_3-WQR406:batch_1 -0.020727809 -0.032829874 -0.0086257442 0.0004416

#migrate to Rmd

#explore other dependent variables including 
##  extract other wavelengths (which?), compare using visualisation and multivariate analysis
##  model and extract S, compare using visualisation and multivariate analysis 


