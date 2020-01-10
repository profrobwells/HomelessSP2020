# "Introduction to R - Spring 20202"
# Rob Wells, PhD
# 1/9/2020

# ------- Get Organized --------- #  

###Set Working Directory. My directory "~/Dropbox/Classes/Data-Analysis-Class-Jour-405v-5003" is an example

getwd()
setwd("~/Dropbox/Classes/Data-Analysis-Class-Jour-405v-5003")

#  Orientation about R Studio  
#  There are four main windows:  

# Script writing, R Markdown, Table Viewer: Upper Left  
# Environment - data loaded in R: Upper Right  
# Console - write commands in R: Lower Left  
# File Manager and Html table viewer: Bottom Right  

#  In the Console window, type:
demo()
help()
help.start()


### Create a folder for this project on your Desktop  

#Create a script to put all your code -- top left window. 
#File > New File > R Script  

# Download Census data into that folder

#Install software to grab data
#tidyverse installs 8 separate software packages to perform
#data import, tidying, manipulation, visualisation, and programming
install.packages("tidyverse")

#I like the rio package for its easy importing features and janitor for data cleaning
##rio handles more than two dozen formats including tab-separated data (with the extension .tsv), 
install.packages("rio") 
install.packages("janitor")


#After you install a package on your hard drive, you can call it back up by summoning a library
#Libraries are bits of software you will have to load each time into R to make things run. 
library(tidyverse)
library(rio)
library(janitor)
#
#Check to see  what's installed by clicking on "Packages" tab in File Manager, lower right pane

#

#Check out Census data
https://learn.uark.edu/bbcswebdav/courses/MASTER-1203-THEUA-JOUR-405V-SEC007/USArk_Counties_Poverty_ACS_16_5YR_DP03_Jan_24.xlsx

#
#Four Corners Test!
# Number Columns
# Number Rows
# Text, Numeric Data

#Import Data - Cleaned Version!
ArkCensus <- rio::import("/Users/rswells/Dropbox/Classes/Homeless Adv Reporting Spring 2020/Homeless Data Analysis Jour-405v-5003/Data/USArk_Counties_Poverty_ACS_16_5YR_DP03_Jan_24.xlsx", which = "Poverty")
#
#Look at the table
View(ArkCensus)

#What happened?
#R grabbed the spreadsheet from the URL
#We told R to grab the first sheet, RealMediaSalaries2

# How many rows?  
nrow(ArkCensus)

# How many columns?
ncol(ArkCensus)

# Let's look at the first six rows
head(ArkCensus)

#Check data types
glimpse(ArkCensus)

#Here is a quick way to view the range of your data  
summary(ArkCensus$Median_Househ_Income)

#summary(ArkCensus$Median_Househ_Income)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#25724   34031   36377   37798   40011   59016 

#Summarize the Income_to25k
#Summaruze the Pct_Income_to_25k


#Build a simple summary table by County
RichPoor <- ArkCensus %>% 
  select(Geography, Median_Househ_Income) %>% 
  group_by(Geography) %>% 
  arrange(desc(Median_Househ_Income))

#Filter
#
#What is the Median Household Income for United States
ArkCensus %>% 
  select(Geography, Median_Househ_Income) %>% 
  filter(Geography =="United States")

#
#What is the Median Household Income for Washington County, Arkansas
ArkCensus %>% 
  select(Geography, Median_Househ_Income) %>% 
  filter(Geography =="Washington County, Arkansas")
#
#Filter for Benton County, Arkansas
#Filter for Pulaski County, Arkansas
#Filter for Arkansas
#
#What is the Statewide Median Income?
#
#Build a table with all counties above that median income

#Create a table with this 
AboveIncome <- ArkCensus %>% 
  select(Geography, Median_Househ_Income) %>% 
  filter(Median_Househ_Income > 42336) %>% 
  arrange(desc(Median_Househ_Income))
#
#Create a table of counties with the lowest quartile in income

#Cut for wells
AboveIncome <- ArkCensus %>% 
   select(Geography, Median_Househ_Income) %>% 
  filter(Median_Househ_Income < 34031) %>% 
  arrange(Median_Househ_Income)



#Find all Counties with 20% more earning $25,000 or less
ArkCensus %>% 
  select(Geography, Pct_Income_to_25k) %>% 
  filter(Pct_Income_to_25k >20) %>% 
  arrange(desc(Pct_Income_to_25k))

#Summary of the low income distribution
summary(ArkCensus$Pct_Income_to_25k)

#
#Which County is lowest?
ArkCensus %>% 
  select(Geography, Pct_Income_to_25k) %>% 
  filter(Pct_Income_to_25k==8.80) %>% 
  arrange(desc(Pct_Income_to_25k))

#Which County is the highest?


#Table with the lowest quartile



#My chart
ggplot(AboveIncome, aes(x = Geography, y = Median_Househ_Income)) + 
  geom_bar(stat = "identity") +
  coord_flip() +    #this makes it a horizontal bar chart instead of vertical
  labs(title = "Communities with high median income", 
       subtitle = "2016",
       caption = "Graphic by Wells",
       y="Place",
       x="Median Income")


#--30--