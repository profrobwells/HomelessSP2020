#School homeless data, Exercise 1 
#Feb 25, 2020
#Rob Wells, University of Arkansas
#JOUR 405V/JOUR 5003 REPORTING ON HOMELESSNESS
#
#Question #1: Load software
#
#Answer:

#
#Load data
#
ChildrenAR <- rio::import("2018-2019-demographic-data-for-districts.xlsx", which = "Data", skip=8)
#
#Wells version
#ChildrenAR <- rio::import("./Data/2018-2019-demographic-data-for-districts.xlsx", which = "Data", skip=8)
#
# Clean up column names to they are R friendly
ChildrenAR <- janitor::clean_names(ChildrenAR)
#
#Question: Run a summary of the homeless data. 
#What is the median homeless percentage? 
#Answer:
#What is the average (mean)?
#Answer:
#What is the maximum?
#Answer:
#
#Question #3: Create a table with all school districts with more than 10% homeless
#HINT: SEE THE R_Intro_Jan15, the code for "Build a table with all counties above that median income"
#
#Answer:
#
#----------------------------------------------------------
# <:-/ !!!!! REVEL IN YOUR NERD POWERS !!!!!!! <:-/
#----------------------------------------------------------
#
#------------------------------------
# FOR WEDNESDAY'S CLASS
#------------------------------------
#For your reference!
https://profrobwells.github.io/HomelessSP2020/SF_311_Calls_UofA.html

#------------------------------------
# Change Over Time
#------------------------------------

#Here's the 2013-14 Data
https://github.com/profrobwells/HomelessSP2020/blob/master/Data/2013-2014-demographics-data-for-districts-2.xlsx?raw=true

#Question: Import it into a dataframe, call it ChildrenAR2013, clean the labels

#Answer:

# Clean up column names to they are R friendly

#Answer:
#
#Question: Create a table with just the district name and the homeless data for 2018-19
#Call it table2018

#Answer:
#
#Here's how to rename a column name
#
colnames(Children10)[2] <- "Pct_Homeless_2018"
#It takes the second column in Children10 and assigns the new name "Pct_Homeless_2018"
names(Children10)
#
#Question: Rename the appropriate table2018 column to "Pct_Homeless_2018"

#Answer:

#Follow the same steps for the 2013 data: 
#Create table2013
#Rename the appropriate table2013 column to "Pct_Homeless_2013"
#
#Answer:
#
#Joining Tables
newtable <- table2018 %>% 
  inner_join(table2013, by=("district_name"))
#
#This matches the tables by name. They are joined by values in district_name
#Question: how many records were left out?
#
#Math!
#This creates a new column with the percentage point difference
newtable <- newtable %>% 
  mutate(difference = Pct_Homeless_2018-Pct_Homeless_2013)
#
#Question: Produce a table with top 20 schools with the greatest increase in 
#student homelessness since 2013, measured by
#percentage point difference
#
#Answer:
#
#-------------------------------------------------
# Bonus Task if you have finished everything....
#-------------------------------------------------

# Elias and Whitney made graphs. Improve on this code:
# Fix the labels, add numbers to the bars

TopTenPct <- ChildrenAR %>%
  filter(district_percent_homeless > 0.1) %>%
  arrange(desc(district_percent_homeless))
#

TopTenPct %>% 
  ggplot(aes(x = reorder(district_name, district_percent_homeless), y = district_percent_homeless, fill=district_percent_hispanic)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(title = "Arkansas School Districts by Homelessness", 
       caption = "Graphic by Weiss",
       y="Percentage of Students Experiencing Homelessness",
       x="District")
