#Rob Wells, PhD. Jan 29, 2020
#San Francisco 311 Call for Service Data, Exercise #1

#Load software
library(tidyverse)
library(janitor)

#Import data
SF <- rio::import("https://github.com/profrobwells/HomelessSP2020/blob/master/Data/SF_311_Jan29.xlsx?raw=true", which = "SF Police_Department_Calls_for_")

#Clean names
SF <- janitor::clean_names(SF)
#
#Question #1: How many rows? Columns? Provide the code and answers below. Supply a list of the column names
#

#Skim this background on the lubridate package. Many packages have vignettes in R to explain what they do
vignette("lubridate")
#
install.packages("lubridate")
library(lubridate)

#Process dates
SF$call_date2 <- ymd(SF$call_date)
SF$year <- year(SF$call_date2)
#
str(SF)
#Examine how we have created a new date and year column and how they are formatted differently than the rest
#We can now perform date and year calculations

Days <- SF %>% 
  count(call_date2) %>% 
  group_by(call_date2) %>% 
  arrange(desc(n))

#Question #2: Using the summary() function, describe the minimum, maximum, median and mean of calls in the Days table
#paste the code and answer here. Write two sentences here that put these trends in context
#
#Question #3: Which day had the most calls? Which day had the least? Provide this answer with code using the filter statement
#
#Examine the types of events

Types <- SF %>% count(original_crime_type_name) %>% 
  group_by(original_crime_type_name) %>% 
    arrange(desc(n))

#Question #4: What are the top five complaints in this data and provide the number of complaints. 
#Create a filter statement to produce a table with just the top five results from the Types table.
#Put your code and paste the table below.




