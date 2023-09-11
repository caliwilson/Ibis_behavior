#Urban Natural Comparison Statistics

#Load libraries
library(readxl)
library(tidyverse)
library(magrittr)
library(gridExtra)
library(viridis)
library(factoextra)
library(rstatix)
library(glmmTMB)
library(DHARMa)
library(bbmle) 
library(gridExtra)
library(lubridate)
library(coin)

#Load raw data
flockscans <- read_excel("data/Spring 2021 Field Data.xlsx", 
                         sheet = "Flock Scans")

#Data wrangling#####
#urban parks = DP, ICP, JB; natural wetlands = GC, WAK, WW
fs <- flockscans
fs$time <-substr(fs$time, 12,20)
fs$notes <- NULL
fs[is.na(fs)] <- 0
types<- flockscans %>% dplyr::select(site, site_type) #pull site type data out 
types <- unique(types)

#most observations were done with birds on the ground but some were in trees too so combined ground + tree
fs$groom_tot <- fs$preening+fs$preening_tree + fs$bathing #combine preening and bathing into grooming
fs$vig_tot <- fs$vigilant+fs$vigilant_tree
fs$rest_tot <- fs$resting+fs$resting_tree
fs %<>% dplyr::select(date,site,site_type,time,foraging,groom_tot,vig_tot,rest_tot,walking) #simplify dataset 

fs %<>%
  mutate(Total = dplyr::select(., foraging:walking) %>% rowSums(na.rm = TRUE))

#calculate proportion of birds doing each behavior
fs$prop_forage<- fs$foraging/fs$Total
fs$prop_groom<- fs$groom_tot/fs$Total 
fs$prop_vig<- fs$vig_tot/fs$Total
fs$prop_rest<- fs$rest_tot/fs$Total
fs$prop_walk<- fs$walking/fs$Total


fs$prop_forage+fs$prop_groom+fs$prop_vig+fs$prop_rest+fs$prop_walk #check if proportions add to 1

fs %<>% dplyr::select(date,site,site_type,time,prop_forage,prop_groom,prop_vig,prop_rest,prop_walk,flock_size=Total) #simplify dataset again

fs_obs<- fs #new dataframe for summarizing data
fs_obs <- fs_obs %>% dplyr::select(date,site,prop_forage,prop_groom,prop_vig,prop_rest,prop_walk,flock_size)  %>% group_by(site,date) %>% summarise_all(mean) #calculate mean of each proportion by observation period (site/date combo)
fs_obs<-merge(x = fs_obs, y = types, by = "site", all = TRUE) #add back in site type

#Now compare means for each site type

#Does flock size differ between site types?
#Do an unpaired two-samples t-test or unpaired two-samples wilcoxon test to compare the mean of two independent groups.


# Shapiro-Wilk normality test for urban flock size
with(fs_obs, shapiro.test(flock_size[site_type == "urban"])) #p=0.058
# Shapiro-Wilk normality test for urban flock size
with(fs_obs, shapiro.test(flock_size[site_type == "natural"]))#p=0.22 

#F-test to test for homogeneity in variances
res.ftest <- var.test(flock_size ~ site_type, data = fs_obs)
res.ftest #p=0.002 so variances are NOT equal

#Using 'wilcox_test' from 'coin' package because it can get exact p-values when their are tied values and also automatically computes Z scores unlike the default R 'wilcox.test' 
wilcox_test(flock_size ~ as.factor(site_type), data = fs_obs) #p=0.25, Z=-1.1547



