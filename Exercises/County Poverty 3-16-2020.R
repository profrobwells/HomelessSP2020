#County Poverty - Homeless Children Data Analysis
#March 16, 2019
#Advanced Reporting, School of Journalism
#Rob Wells, PhD
# The best way to compare poverty in Arkansas counties to state and national rates is to examine the 2017 Census data.
# Download this file and examine the headings. Search for "poverty"
# https://raw.githubusercontent.com/profrobwells/HomelessSP2020/master/Data/Ark%20Poverty%202017%20ACS%20DP03/ACS_17_5YR_DP03_metadata.csv
# This is a large spreadsheet with a number of different fields and ways of measuring poverty. I want you to briefly examine the different ways it is measured - by "all families" and "families with female householder, no husband present" and so forth.
# We will settle on the column HC03_VC161 Percent; PERCENTAGE OF FAMILIES AND PEOPLE WHOSE INCOME IN THE PAST 12 MONTHS IS BELOW THE POVERTY LEVEL – All families
#Here is the data we will use. A huge dataset with 551 columns
#https://raw.githubusercontent.com/profrobwells/HomelessSP2020/master/Data/Ark%20Poverty%202017%20ACS%20DP03/ACS_17_5YR_DP03_with_ann.csv

# Open R and import the data


library(tidyverse)
library(rio)
library(janitor)

CountyPovertyAll <- rio::import("https://raw.githubusercontent.com/profrobwells/HomelessSP2020/master/Data/Ark%20Poverty%202017%20ACS%20DP03/ACS_17_5YR_DP03_with_ann.csv")
CountyPovertyAll <- janitor::clean_names(CountyPovertyAll)
glimpse(CountyPovertyAll)

#Let's shave down this big boy to the key columns. Import just the geo_id columns, the county name and the hc03_vc161

#Your answer:

#Rename the appropriate columns to "county" and "poverty_pct"

#Your Answer

#Drop the first row of text
CountyPoverty = CountyPoverty[-c(1),]

#Convert to numeric data

#Your answer

#Cut "County, Arkansas"
#Your answer


#Part 2: Join with district homeless data
#This dataset tells us the county names and ids, which we link to the Census Data and the Homeless children data

Link <- rio::import("https://github.com/profrobwells/HomelessSP2020/blob/master/Data/ncesdata_22D2A61.xlsx?raw=true", skip=14)
Link <- janitor::clean_names(Link)

#Clean District ID column - eliminate "AR-"
Link$state_district_id_bak <- Link$state_district_id
Link$county_name_bak <- Link$county_name 
Link$state_district_id <- gsub("AR-", "", Link$state_district_id) 
Link$county_name <- gsub(" County", "", Link$county_name) 
Link$county_name <- gsub(" ", "", Link$county_name) 

newtable <- right_join(CountyPoverty, Link, by=c("county"="county_name"))
newtable$state_district_id <- as.numeric(newtable$state_district_id)

#And finally, joining to the Child Poverty table you created over the weekend

ChildPoverty <- inner_join(newtable, Child2019_15, by=c("state_district_id"="district_lea"))

#Summary table
ChildPoverty_sum <- ChildPoverty %>% 
  select(district_name, county, students, teachers, student_teacher_ratio, county_poverty_pct, homeless_pct_2019, homeless_pct_diff)

#clean
ChildPoverty_sum$homeless_pct_2019 <- as.numeric(ChildPoverty_sum$homeless_pct_2019)
ChildPoverty_sum$county_poverty_pct <- as.numeric(ChildPoverty_sum$county_poverty_pct)
ChildPoverty_sum$homeless_pct_diff <- as.numeric(ChildPoverty_sum$homeless_pct_diff)
#convert numbers to same format
ChildPoverty_sum <- ChildPoverty_sum %>% 
  mutate (county_poverty_pct1 = (county_poverty_pct *.01))

#Table of school districts with a higher student homeless rate than the schools
#This uses the greater than operator
Homeless <- ChildPoverty_sum %>% filter(homeless_pct_2019 > county_poverty_pct1)


#With this table, answer the following questions:

#1) Produce a summary table from the ChildPoverty table with the school districts you examined, 
# the county poverty and homeless rate.
# Make a screenshot of the table and upload it to this discussion post.

#2)Examine the homeless data carefully and do some research to determine the discrepancies, if any, between 
#homeless student rate and the county poverty rate.






