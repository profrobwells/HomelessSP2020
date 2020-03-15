#Homeless Children Data Analysis
#March 14, 2019
#Advanced Reporting, School of Journalism
#Rob Wells, PhD
#
#We will analyze homeless children data over time
# Compare previous years of homeless children data
# --Take the 2018-2019 data and compare to the 2014-2015 data
# --Following the same workflow in the Census exercise, import the two datasets, 
#rename the columns, clean the district names, calculate percentage change in homeless
# 1) --Produce a table with statewide changes in homelessness and the special needs index from 2018-2019 vs 2014-2015. 
# 2) --Produce a smaller chart with all of the districts you have contacted 
#
library(tidyverse)
library(rio)
library(janitor)

#Part 1: import the two datasets, rename the columns, clean the district names
#Load ChildrenAR 2018-19 data
ChildAR2019 <- rio::import("https://github.com/profrobwells/HomelessSP2020/blob/master/Data/2018-2019-demographic-data-for-districts.xlsx?raw=true", which = "Data", skip=8)
ChildAR2019 <- janitor::clean_names(ChildAR2019)
#Names 
colnames(ChildAR2019)[3] <- c("enroll_2019")
colnames(ChildAR2019)[5:6] <- c("special_ed_pct_2019", "homeless_pct_2019")

#Load ChildrenAR 2014-15 data
ChildAR2015 <- rio::import("https://github.com/profrobwells/HomelessSP2020/blob/master/Data/2014-2015-demographics-for-districts.xlsx?raw=true", which = "Data", skip=8)
ChildAR2015 <- janitor::clean_names(ChildAR2015)
#Names 
colnames(ChildAR2015)[3] <- c("enroll_2015")
colnames(ChildAR2015)[5:6] <- c("special_ed_pct_2015", "homeless_pct_2015")

#SELECT SUBSET WITH DISTRICT NAME POVERTY SPECIAL NEEDS ENROLLMENT AND MERGE


#Merge Census Data

temp <- ChildAR2019 %>%
  inner_join(ChildAR2015, by=("district_lea"))
#Boil is down

Child2019_15 <- temp %>% 
  select(district_name.x, enroll_2019, enroll_2015, special_ed_pct_2019, special_ed_pct_2015,
                       homeless_pct_2019, homeless_pct_2015, district_lea)
colnames(Child2019_15)[1] <- c("district")

#Simplify the school district names?
#Backup distict name
Child2019_15$district_bak <- Child2019_15$district
Child2019_15$district <- gsub("SCHOOL", "", Child2019_15$district) 
Child2019_15$district  <- gsub("DISTRICT", "", Child2019_15$district)
Child2019_15$district <- gsub("SCHOOL", "", Child2019_15$district) 
Child2019_15$district <- gsub(" ", "", Child2019_15$district) 

#Part 2: Calculate calculate percentage change in homeless


#how has homeless changed from 2015-19

Child2019_15 <- Child2019_15 %>%
  #select(Place1,PerCap_Income_2017, PerCap_Income_2012) %>%
  mutate(homeless_pct_diff = (homeless_pct_2019-homeless_pct_2015)/homeless_pct_2015) %>%
  arrange(desc(homeless_pct_diff))

library(formattable)

Child2019_15$homeless_pct_diff <- percent(Child2019_15$homeless_pct_diff)

#how has special needs changed from 2015-19

Child2019_15 <- Child2019_15 %>%
  mutate(special_pct_diff = (special_ed_pct_2019-special_ed_pct_2015)/special_ed_pct_2015) %>%
  arrange(desc(special_pct_diff))

library(formattable)

Child2019_15$special_pct_diff <- percent(Child2019_15$special_pct_diff)

#1) --Produce a table with statewide changes in homelessness and the special needs index from 2018-2019 vs 2014-1025. 

Child2019_15_summary <- Child2019_15 %>% 
  select(district, homeless_pct_diff, special_pct_diff)

#2) Your schools
Child2019_15 %>% 
filter(district=="BERRYVILLE" | district=="JASPER" | district=="LEADHILL")

