#San Francisco 311 Call for Service Data


library(tidyverse)
library(janitor)
library(lubridate)

# SEE BELOW TO FOLLOW TUTORIAL FROM STEP 1


#Import data
SF <- rio::import("https://github.com/profrobwells/HomelessSP2020/blob/master/Data/SF_311_Jan29.xlsx?raw=true", which = "SF Police_Department_Calls_for_")
#Clean names
SF <- janitor::clean_names(SF)
#Process dates
SF$call_date2 <- ymd(SF$call_date)
SF$year <- year(SF$call_date2)
#
#DAYS TABLE
Days <- SF %>% 
  count(call_date2) %>% 
  group_by(call_date2) %>% 
  arrange(desc(n))
#
#TYPES TABLE
Types <- SF %>% count(original_crime_type_name) %>% 
  group_by(original_crime_type_name) %>% 
  arrange(desc(n))
#
#YEARS TABLE
Years <- SF %>% 
  count(year) %>% 
  group_by(year) %>% 
  arrange(desc(year))
#
#ACTION TABLE
Action <- SF %>% 
  count(disposition) %>% 
  arrange(desc(n))


#------------------------------------------------------------

#Import data
SF <- rio::import("https://github.com/profrobwells/HomelessSP2020/blob/master/Data/SF_311_Jan29.xlsx?raw=true", which = "SF Police_Department_Calls_for_")
#Wells offline download
#SF <- rio::import("./Data/SF_311_Jan29.xlsx", which = "SF Police_Department_Calls_for_")


#Clean names
SF <- janitor::clean_names(SF)
#
#Question #1: How many rows? Columns? Supply a list of the column names

#wells cut
# nrow(SF)
# [1] 157237
# > ncol(SF)
# [1] 14
# > colnames(SF)
# [1] "crime_id"                 "original_crime_type_name" "report_date"             
# [4] "call_date"                "offense_date"             "call_time"               
# [7] "call_date_time"           "disposition"              "address"                 
# [10] "city"                     "state"                    "agency_id"               
# [13] "address_type"             "common_location"         
# >
nrow(SF)
ncol(SF)
colnames(SF)

#Skim this background on the lubridate package. Many packages have vignettes in R to explain what they do
vignette("lubridate")
#
#install.packages("lubridate")
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


#Wells answer below
summary(Days)
summary(Days)
# call_date2               n        
# Min.   :2016-03-31   Min.   : 10.0  
# 1st Qu.:2017-02-28   1st Qu.: 86.0  
# Median :2018-01-29   Median :119.0  
# Mean   :2018-01-29   Mean   :117.3  
# 3rd Qu.:2018-12-30   3rd Qu.:148.0  
# Max.   :2019-11-30   Max.   :232.0  

#Between March 31, 2016 and Nov. 30, 2019, San Francisco residents placed an average 117 calls to police complaining about homeless people.

#Question #3: Which day had the most calls? Which day had the least? Provide this answer with code using the filter statement

#Wells cut
Days %>% 
  filter(n == 232) 
# 1 2019-08-15   232

Days %>% 
  filter(n == 10)
# 1 2016-03-31    10
#Examine the types of events

Types <- SF %>% count(original_crime_type_name) %>% 
  group_by(original_crime_type_name) %>% 
  arrange(desc(n))

#Question #4: What are the top five complaints in this data and provide the number of complaints

Types <- SF %>% count(original_crime_type_name) %>% 
  group_by(original_crime_type_name) %>% 
  arrange(desc(n))


#Wells ANSWER
#Create separate table with just the top five counties' crime rate: dplyr has a "top_n" function that i find handy
Types <- SF %>% count(original_crime_type_name) %>% 
  group_by(original_crime_type_name) %>% 
  arrange(desc(n))

Types <- SF %>% 
  count(original_crime_type_name) %>% 
  top_n(5, n) %>% 
  arrange(desc(n))

# original_crime_type_name      n
# <chr>                     <int>
#   1 Homeless Complaint       142780
# 2 915                        9283
# 3 919                        2879
# 4 915 Sleeper                 290
# 5 920                         226


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

#QUESTION #5: Now build a similar table totalling the number of complaints by year
#WRITE YOUR ANSWER HERE AND SUPPLY THE CODE
#Export the finished version to a spreadsheet

# WELLS ANSWER 

Years <- SF %>% 
  count(year) %>% 
  group_by(year) %>% 
  arrange(desc(year))

#------------------------------
#EXERCISE: Grouping by disposition
#Look at the Radio Codes spreadsheet under dispositions
#https://data.sfgov.org/api/views/hz9m-tj6z/files/b60ee24c-ae7e-4f0b-a8d5-8f4bd29bf1de?download=true&filename=Radio%20Codes%202016.xlsx
#Total by disposition

Action <- SF %>% 
  count(disposition) %>% 
  arrange(desc(n))

#QUESTION #6: Describe some of the common trends illustrated in the disposition data and any questions you may have for the police about this data
#WRITE YOUR ANSWER HERE 

#------------------------------
#EXERCISE: Create a table with serious infractions described in disposition

#Example: Here's a table filtering the dispositions column to show "no disposition" or "gone on arrival"
Nothing <- SF %>% 
  filter(disposition == "ND" | disposition == "GOA")

#QUESTION #7: Create a table with the serious actions including citations and arrests police took in the dispositions

WELLS ANSWER BELOW
#-------------------
#Here is my filter
#Arrest, Cited, Criminal Activation, SF Fire Dept Medical Staff engaged
Busted <- SF %>% 
  filter(disposition == "ARR" | disposition == "CIT" | disposition == "CRM" | disposition == "SFD")

Busted1 <- SF %>% 
  filter(disposition == "ARR" | disposition == "CIT" | disposition == "CRM" | disposition == "SFD") %>% 
  count(disposition) %>% 
  arrange(desc(n))
#-------------------


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

WELLS ANSWER BELOW

Action %>% 
  filter(n > 100) %>% 
  ggplot(aes(x = disposition, y = n, fill=n)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +    #this makes it a horizontal bar chart instead of vertical
  labs(title = "Action on Homeless Calls, San Francisco", 
       subtitle = "311 Call Data, 3/2016-11/2019",
       caption = "Graphic by Wells",
       y="Number of Calls",
       x="Action")


Action %>% 
  filter(n > 100) %>% 
  ggplot(aes(x = reorder(disposition, n), y = n, fill=n)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +    #this makes it a horizontal bar chart instead of vertical
  labs(title = "Action on Homeless Calls, San Francisco", 
       subtitle = "311 Call Data, 3/2016-11/2019",
       caption = "Graphic by Wells",
       y="Number of Calls",
       x="Action")

#------------------------------
#Part 3, SF 311 Call Data
#
#Making our charts less ugly
#
#The disposition column is in cop-speak. We need to clean it up
#
#Step #1: Duplicate the column you want to mess with
SF$disposition1 <- SF$disposition
#
#Rename specific strings. Example:
#str_replace_all(test.vector, pattern=fixed('-'), replacement=fixed(':') )
#https://dereksonderegger.github.io/570L/13-string-manipulation.html
#We can do this to replace ABA with "Abated"
SF$disposition1 <- str_replace_all(SF$disposition1, pattern=fixed('ABA'), replacement=fixed('Abated') )
#Again with ADM
SF$disposition1 <- str_replace_all(SF$disposition1, pattern=fixed('ADM'), replacement=fixed('Admonished') )
#
#We can do that 19 times. OR....
#Look at this example using a lookup table to replace all the values
#https://stackoverflow.com/questions/50615116/renaming-character-variables-in-a-column-in-data-frame-r
#
#Build a table to translate the Cop Speak to English:
dispo_lkup <- c(ABA="Abated", ADM="Admonish", ADV="Advised", ARR="Arrest", CAN="Cancel", CSA="CPSA", 
                CIT="Cited", CRM="Criminal", GOA="Gone", HAN="Handled", NCR="No_Criminal", ND="No_Dispo", 
                NOM="No_Merit", PAS="PlaceSecure", REP="Report", SFD="Medical", UTL="Unfound", VAS="Vehicle_Secure", '22'="Cancel")

#22="Cancel" was handled differently because it is a numeric value: '22'="Cancel"
#
#This scans "disposition", finds ABA and replaces with Abated, finds ARR, replaces with Arrest, etc
SF$disposition1 <- as.character(dispo_lkup[SF$disposition])

#
#Rerun Action with disposition1
Action <- SF %>% 
  count(disposition1) %>% 
  arrange(desc(n))
#
View(Action)
#What is wrong with this picture?
#
#Compare our renamed variables to the original disposition
Action <- SF %>% 
  count(disposition1, disposition) %>% 
  arrange(desc(n))
#
#We have codes not listed on the sheet
NA	Not recorded	4339
#
#Get rid of the space
SF$disposition <- gsub("Not recorded", "Not_Recorded", SF$disposition)
#
#Add to the list
dispo_lkup <- c(ABA="Abated", ADM="Admonish", ADV="Advised", ARR="Arrest", CAN="Cancel", CSA="CPSA", 
                CIT="Cited", CRM="Criminal", GOA="Gone", HAN="Handled", NCR="No_Criminal", ND="No_Dispo", 
                NOM="No_Merit", PAS="PlaceSecure", REP="Report", SFD="Medical", UTL="Unfound", 
                VAS="Vehicle_Secure", '22'="Cancel", Not_Recorded="NotRecorded")
#Rerun
SF$disposition1 <- as.character(dispo_lkup[SF$disposition])
#
Action <- SF %>% 
  count(disposition1) %>% 
  arrange(desc(n))
#
View(Action)
#
#Chart Time
#
Action %>% 
  filter(n > 100) %>% 
  ggplot(aes(x = reorder(disposition1, n), y = n, fill=n)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +    #this makes it a horizontal bar chart instead of vertical
  labs(title = "Action on Homeless Calls, San Francisco", 
       subtitle = "311 Call Data, 3/2016-11/2019",
       caption = "Graphic by Wells",
       y="Number of Calls",
       x="Action")
#
#Parse out police codes from narrative: original_crime_type_name
#Look at the Types table: some columns have one code, some have two.
#919	2879
#915 Sleeper	290
#
#Some are separated by a slash
#915/919	161
#
#We need to unpack that
#convert all text to lowercase
SF$crime1 <- tolower(SF$original_crime_type_name)
#Replace / with a space
SF$crime1 <- gsub("/", " ", SF$crime1)
#Replace '
SF$crime1 <- gsub("'", "", SF$crime1)
#fix space in homeless complaint
SF$crime1 <- gsub("homeless complaint", "homeless_complaint", SF$crime1)
#split data into two columns
SF <- separate(data = SF, col = crime1, into = c("crime2", "crime3", "crime4"), sep = " ", extra = "merge", fill = "right")

#Look at the categories now
Types2 <- SF %>% count(crime2) %>% 
  group_by(crime2) %>% 
  arrange(desc(n))
#
#OPTIONAL Exercise to count the NAs
Types3 <- Types2
Types4 <- Types3 %>% 
  filter(n <=30)
sum(Types4$n)

#Question #9
#Take the top 10 crime categories from Type2
#Relabel them from the numeric radio codes into English
#Using the technique earlier in "Build a table to translate the Cop Speak to English"
#Relabel the offenses
#
# WELLS ANSWER BELOW

# clean <- c('915'="homeless_call", '915x'="homeless_call", '915s'="homeless_call", '915.aggressive'="homeless_call",
#            +            '919'="sit_lying", '920'="aggress_solicit", '601'="trespasser", '811'="intoxicated",
#            +            homeless_complaint="homeless_complaint")

clean <- c(homeless_complaint="homeless_complaint", '915'="homeless_call", '919'="sit_lying", '920'="aggress_solicit",
           '915s'="homeless_call", '915x'="homeless_call", drugs="drugs", '601'="trespasser",
           poss="poss", aggressive="aggressive", '811'="intoxicated")
#
SF$crime2 <- as.character(clean[SF$crime2])
#
#Look at the categories now
Types2 <- SF %>% count(crime2) %>% 
  group_by(crime2) %>% 
  arrange(desc(n))

#Question #10
#Make a chart from your cleaned data

# WELLS ANSWER BELOW

#basic chart but with a messed up x axis
Types2 %>% 
  ggplot(aes(x = crime2, y = n, fill=n)) + 
  geom_bar(stat = "identity") +
  coord_flip() +    #this makes it a horizontal bar chart instead of vertical
  labs(title = "Top 10 Homeless Complaints, San Francisco", 
       subtitle = "311 Call Data, 3/2016-11/2019",
       caption = "Graphic by Wells",
       y="Number of Calls",
       x="Complaint")

#chart with a fixed x axis scale
#no values filtered out
#labels added to bars
Types2 %>% 
  filter(!is.na(crime2)) %>% 
  # filter(crime2!=" ") %>%  - a crude alternative to previous line!
  ggplot(aes(x = reorder(crime2, n), y = n, fill=n)) + #reorder sorts the bars
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label = n), hjust = -.1, size = 3) +
  scale_y_continuous(limits=c(0, 175000)) + #fixes scientific notation
  coord_flip() +    #this makes it a horizontal bar chart instead of vertical
  labs(title = "Top 10 Homeless Complaints, San Francisco", 
       subtitle = "311 Call Data, 3/2016-11/2019",
       caption = "Graphic by Wells",
       y="Number of Calls",
       x="Complaint")

#------------------------------------------------
#311 Call Data, Part 4
#Using Mutate, Percentage Calculations, Further Data Cleaning
#------------------------------------------------

#mutate - Create new column(s) in the data, or change existing column(s).
#mutate() adds new variables and preserves existing;
# Newly created variables are available immediately

#An example:
mtcars <- as.data.frame(mtcars)
View(mtcars)

mtcars2 <- mtcars %>% as_tibble() %>% mutate(
  cyl2 = cyl * 2,
  cyl4 = cyl2 * 2
)

#Chart the number of calls by year and month
#Process dates using lubidate
library(lubridate)

SF <- SF %>% 
  mutate(yearmo = format(call_date, "%Y-%m"))

SF %>% 
  count(yearmo) %>% 
  group_by(yearmo) %>% 
  ggplot(aes(x = yearmo, y = n, fill=n)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle=90)) +
  #Changes angle of x axis labels
  #coord_flip() +    #this makes it a horizontal bar chart instead of vertical
  labs(title = "Homeless Calls After 2017, San Francisco", 
       subtitle = "311 Call Data by Month 2017-2019",
       caption = "Graphic by Wells",
       y="Number of Calls",
       x="Year")

#Percentage change per month
PCT_CHG_CALLS <- SF %>% 
  select(original_crime_type_name, disposition, address, call_date2, yearmo) %>% 
  count(yearmo) %>% 
  mutate(difference = (n-lag(n))) %>% 
  mutate(pct_change = (difference/abs(lag(n)))*100)


#----------------------------------------------
# OPTION 1: Creating a matrix, Tabulating
#----------------------------------------------
#https://stackoverflow.com/questions/58550095/tabulate-using-tabyl-by-grouping-variable-using-group-split-and-group-map 
#tabulate one column at a time
# iris %>% 
#   tabyl(Petal.Width)
#We need to unpack that
#convert all text to lowercase
SF$crime1 <- tolower(SF$original_crime_type_name)
SF$crime1 <- gsub("/", " ", SF$crime1)
SF$crime1 <- gsub("'", "", SF$crime1)
SF$crime1 <- gsub("homeless complaint", "homeless_complaint", SF$crime1)
SF <- separate(data = SF, col = crime1, into = c("crime2", "crime3", "crime4"), sep = " ", extra = "merge", fill = "right")
#
#New df
SF1 <- SF %>% 
  select(crime_id, original_crime_type_name, call_date, disposition, crime2, crime3, crime4) 

# SF_table <- SF1 %>% 
#   tabyl(crime2)

#tabulate multiple columns at once using map
#iris %>% 
#  select(Petal.Width, Petal.Length) %>% 
#  map(tabyl)

ver1 <- SF1 %>% 
  select(crime2, crime3, crime4) %>% 
  map(tabyl)  
#this works
SF_table <- dplyr::bind_rows(ver1)
#
SF_table <- SF_table[ -c(3,4) ]
#
colnames(SF_table)[1] <- "complaint"
#
Total_Calls <- SF_table %>% 
  select(complaint, n) %>% 
  group_by(complaint) %>% 
  summarise(total = sum(n)) %>% 
  arrange(desc(total))
write_csv(Total_Calls, "Total_Calls.csv")

#------------------------------------------------
# OPTION 2: Calculate three columns separately, recombine
#------------------------------------------------

#Clean the new crime columns

SF$crime2 <- gsub("[[:punct:]]", "", SF$crime2)
SF$crime3 <- gsub("[[:punct:]]", "", SF$crime3)
SF$crime4 <- gsub("[[:punct:]]", "", SF$crime4)

#test matrix to see totals
ver2 <- SF %>% count(crime2, crime3, crime4) %>% 
  group_by(crime2, crime3, crime4) %>% 
  arrange(desc(n))

#compile table
#Remove space
SF$crime2 <- gsub(" ", "", SF$crime2)

table1 <- SF %>% count(crime2) %>% 
  group_by(crime2) %>% 
  arrange(desc(n))

SF$crime3 <- gsub(" ", "", SF$crime3)

table2 <- SF %>% count(crime3) %>% 
  group_by(crime3) %>% 
  arrange(desc(n))

SF$crime4 <- gsub(" ", "", SF$crime4)

table3 <-SF %>% count(crime4) %>% 
  group_by(crime4) %>% 
  arrange(desc(n))

#Combine tables
#First, consistent names
colnames(table1)[1] <- "crime"
colnames(table2)[1] <- "crime"
colnames(table3)[1] <- "crime"
#Second, consistent format
table1 <- as.data.frame(table1)
table2 <- as.data.frame(table2)
table3 <- as.data.frame(table3)
#
#Use rbind to combine
Calls_Table <- rbind(table1, table2, table3)
# strips out all punctuation gsub("[[:punct:]]", "", c)
Calls_Table$crime <- gsub("[[:punct:]]", "", Calls_Table$crime)

Calls_Table_Sum <- Calls_Table %>% 
  select(crime, n) %>% 
  group_by(crime) %>% 
  summarise(total = sum(n)) %>% 
  arrange(desc(total))

write_csv(Calls_Table_Sum, "Calls_Table_Sum.csv")


#----------------------------------------------
# OPTION 3: The most accurate: Using grepl to search and tabulate
#----------------------------------------------
#grep and grepl
??grep
#http://www.endmemo.com/program/R/grepl.php

x915 <- SF %>% 
  filter(grepl ("915", original_crime_type_name)) %>% 
  mutate(cleaned = "915")

x919 <- SF %>% 
  filter(grepl ("919", original_crime_type_name)) %>% 
  mutate(cleaned = "919")

xsleep <- SF %>% 
  filter(grepl ("sleep", original_crime_type_name)) %>% 
  mutate(cleaned = "sleep")

xaggr <- SF %>% 
  filter(grepl ("aggr", original_crime_type_name)) %>% 
  mutate(cleaned = "aggressive")

xdrug <- SF %>% 
  filter(grepl ("drug", original_crime_type_name)) %>% 
  mutate(cleaned = "drug")

xhomeless <- SF %>% 
  filter(grepl ("homeless_complaint", crime2)) %>% 
  mutate(cleaned = "homeless_complaint")

new_total <- rbind(xhomeless, x915, x919, xaggr, xdrug, xsleep)

Total_Calls_Master <- new_total %>% 
  count(cleaned) %>% 
  arrange(desc(n))

colnames(Total_Calls_Master)[1:2] <- c("Complaints", "Number")

write_csv(Total_Calls_Master, "Total_Calls_Master.csv")
#
#Make into html table
install.packages("kableExtra")
library(kableExtra)
# This makes kables
Total_Calls_Master %>% 
  kable() %>%
  kable_styling("striped")
#Export from Viewer as .png


#----------------------------------------------
#Task #1: Expand the Option 3 to build a comprehensive table violations
#  - Review Calls_Table, Total_Calls for others we haven't addressed
#
#Build a professional chart using the formatting tricks (labels, etc)
#
#----------------------------------------------

#Task #2: Tabulate complaints by day of the week
#https://github.com/profrobwells/Data-Analysis-Class-Jour-405v-5003/blob/master/Readings/dealing-with-dates.pdf
#
#Key code from tutorial:
#data$weekday <- wday(data$DOB, label=TRUE, abbr=FALSE)

SF <- SF %>% 
  mutate(weekday = wday(call_date, label=TRUE, abbr=FALSE))

#Build a summary table with the days of the week with the greatest
#number of calls

#Create a graphic

#Then build a table to see if the complaints vary by day

#----------------------------------------------
#Task #3: Calls vs Dispositions
#
#What calls resulted in arrests? What calls resulted in citations?

Action2 <- SF %>%
  select(crime_id, original_crime_type_name, disposition) 

#We need to pair the crime type and disposition and then count them

