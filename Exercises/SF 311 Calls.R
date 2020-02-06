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
#

#-----------------------------------------
#Part 2: San Francisco 311 Call for Service Data
#Feb 5, 2020
#Filtering 
#Building Tables
#Basic charts



#Exercise: Number of complaints per year
#Recall from line 48 the table that counted by days, or call_date2
# Days <- SF %>% 
#   count(call_date2) %>% 
#   group_by(call_date2) %>% 
#   arrange(desc(n))

#How to export a table into a spreadsheet (csv is a comma separated file)
write.csv(Days,"Days.csv")

#QUESTION #5: Now build a similar table to the Days table totalling the number of complaints by year
#WRITE YOUR ANSWER HERE AND SUPPLY THE CODE
#Export the finished version to a spreadsheet


#------------------------------
#EXERCISE: Grouping by disposition
#Look at the Radio Codes spreadsheet under dispositions
#https://data.sfgov.org/api/views/hz9m-tj6z/files/b60ee24c-ae7e-4f0b-a8d5-8f4bd29bf1de?download=true&filename=Radio%20Codes%202016.xlsx
#Total by disposition

Action <- SF %>% 
  count(disposition) %>% 
  arrange(desc(n))

#QUESTION #6: Describe in one paragraph some of the common trends illustrated in the disposition data and any questions you may have for the police about this data
#WRITE YOUR ANSWER HERE 

#------------------------------
#EXERCISE: Create a table with serious infractions described in disposition

#Example: Here's a table filtering the dispositions column to show "no disposition" or "gone on arrival"
Nopeople <- SF %>% 
  filter(disposition == "ND" | disposition == "GOA")

#QUESTION #7: Create a table with the serious actions including citations and arrests police took in the dispositions


#------------------------------
#EXERCISE - A Basic chart of the crime data
ggplot(Years, aes(x = year, y = n)) + 
  geom_bar(stat = "identity") +
  #coord_flip() +    #this makes it a horizontal bar chart instead of vertical
  labs(title = "Homeless Calls Per Year, San Francisco", 
       subtitle = "311 Call Data, 3/2016-11/2019",
       caption = "Graphic by Wells",
       y="Number of Calls",
       x="Year")

#A chart using a dplyr filtering language
Years %>% 
  filter(year >= 2017) %>% 
  ggplot(aes(x = year, y = n)) +
  geom_bar(stat = "identity") +
  #coord_flip() +    #this makes it a horizontal bar chart instead of vertical
  labs(title = "Homeless Calls After 2017, San Francisco", 
       subtitle = "311 Call Data, 2017-2019",
       caption = "Graphic by Wells",
       y="Number of Calls",
       x="Year")

#A more complex filter
SF %>% 
  filter(!is.na(common_location)) %>% 
  count(common_location) %>% 
  top_n(10, n) %>% 
  ggplot(aes(x = common_location, y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +    #this makes it a horizontal bar chart instead of vertical
  labs(title = "Popular Spots for Homeless, San Francisco", 
       subtitle = "311 Call Data, 2016-2019",
       caption = "Graphic by Wells",
       y="Number of Calls",
       x="Places")

#QUESTION #8: Chart the total dispositions. Filter for at least 100 actions. Add color, export image to Blackboard.




