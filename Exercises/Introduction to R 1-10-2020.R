# "Introduction to R - NICAR, 2020"
# Rob Wells, PhD
# @rwells1961
# 1/9/2020

# ------- Get Organized --------- #  

remember some data scientists hate the setwd command - look at alternatives

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
Do I want to do this?
Create an R Project in that folder

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
dont know if I need this
#install.packages("here")


#Check out media salaries data
https://docs.google.com/spreadsheets/d/1SP3Sqqdv6R8chFamjtgdNlOrUar-hJXvkMSeha2mHQ8/edit#gid=0
#
#IRE Old School: Four Corners Test!
#8 Columns
#1661 Rows
#Numberic data in Salary, Years Experience
#Mixed string data in Gender Identity / Ethnicity, Job duties


#Import Data - Cleaned Version!
MediaBucks <- rio::import("https://docs.google.com/spreadsheets/d/1HY3JzI4NEMIX7Oi46j-ML_QnVE7dbAK1qnu7mjE6JCE/edit#gid=476066139", which = "1")
#Look at the table
View(MediaBucks)
#What happened?
#R grabbed the spreadsheet from the URL
#We told R to grab the first sheet, RealMediaSalaries2
#basics of R
#<- is known as an “assignment operator.” It means: “Make the object named to the left equal to the output of the code to the right.”

# How many rows?  
nrow(MediaBucks)

# How many columns?
ncol(MediaBucks)

# Let's look at the first five rows
head(MediaBucks)

#Check data types
glimpse(MediaBucks)

#Names of your columns
colnames(MediaBucks)

#OR
names(MediaBucks)


#Here is a quick way to view the range of your data  
summary(MediaBucks$Salary)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0   42000   60000   64174   78000  770000       4 

#Distribution
quantile(MediaBucks$Salary, c(0.1, 0.2, 0.3, 0.4,0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
quantile(MediaBucks$Salary, c(0.25, 0.50, 0.75, 0.9, 0.99), na.rm=TRUE)


MediaBucks$Salary2 <- as.numeric(MediaBucks$Salary)
sum(MediaBucks$Salary2, na.rm=TRUE)
sum(MediaBucks$Salary, na.rm=TRUE)
mean(MediaBucks$Salary, na.rm=TRUE)

#Shortcut Commands
#Tab - Autocomplete
#In Console Window (lower left) 
#--Control (or Command) + UP arrow - last lines run
#Control (or Command) + Enter - Runs current or selected lines of code in the top left box of RStudio
#Shift + Control (or Command) +P - Reruns previous region code

#dplyr

#Build a simple summary table by Gender
Summary <- MediaBucks %>% 
  select(Gender, Salary) %>% 
  group_by(Gender) %>% 
  summarize(Total = sum(Salary, na.rm=TRUE))

#
#select Choose which columns to include.
#filter Filter the data.
#arrange Sort the data, by size for continuous variables, by date, or alphabetically.
#group_by Group the data by a categorical variable.
#
#What is the sample size?
Gender <- MediaBucks %>% 
  count(Gender) %>% 
  arrange(desc(n))

#Better idea: Check Averages!
#Build a simple summary table by Gender
Summary <- MediaBucks %>% 
  select(Gender, Salary) %>% 
  group_by(Gender) %>% 
  summarize(Avg_Salary = mean(Salary, na.rm=TRUE))




#Quick filter out hourly workers
MediaSalary <- MediaBucks %>% 
  filter(Salary >= 1000)

#Questions
# 1: View the range of your data  
#2: Number of rows
#3: Number of rows cut with filter

#Answer: #1
summary(MediaSalary$Salary)

#Answer: #2
nrow(MediaSalary)

#Answer: #3
#How many rows cut out with hourly filter?
nrow(MediaBucks)-nrow(MediaSalary)

#Find Your News Organization

#Filter
WSJ <- subset(MediaBucks, COMPANY=="WallStreetJournal")  

summary(WSJ$Salary)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    38   41000   51100   64275   75750  236000 
#
#Using Wildcards 
Journal <- subset(MediaBucks, grepl("?Journal", COMPANY))
Bloom <- subset(MediaBucks, grepl("?Bloomberg", COMPANY))
#
#Questions
#1: Build a table for NewYorkTimes employees
#2: Determine median salary of NewYorkTimes employees
#3: Identify title, gender and race of the highest paid position at NYT'
#4: Search for "Business" in Company, check salaries, compare to "Bloomberg"

#Answers:
#1: Build a table for NewYorkTimes employees
NYT <- subset(MediaBucks, COMPANY=="NewYorkTimes") 
#2: Determine median salary of NewYorkTimes employees
summary(NYT$Salary)
#3: Identify title, gender and race of the highest paid position at NYT
NYT %>% 
  filter(Salary == 350000)
#4: Search for "Business" in Company, check median salaries, compare to "Bloomberg"
Biz <- subset(MediaBucks, grepl("?Business", COMPANY))
summary(Biz$Salary)
#$46,000
Bloom <- subset(MediaBucks, grepl("?Bloomberg", COMPANY))
summary(Bloom$Salary)
#$50,350
#
#Back at it!
#Build a table with several companies of your choice
BigBoys <- filter(MediaSalary, COMPANY %in% c("NewYorkTimes", "WallStreetJournal", "Bloomberg"))    
#
#Table with just reporter salaries
Reporters <- subset(MediaBucks, grepl("?reporter", TITLE))
summary(Reporters$Salary)
#
#Questions:
#1: Who is making $230,504 as a reporter???
#2: Make a table for editors, figure out medians.
#3: Find highest paid editor. Resent them.
#4: Make a table for any position involving data

#Answer:
#1: Who is making $230,504 as a reporter???
Reporters %>% 
  filter(Salary == 230504)
#2: Make a table for editors, figure out medians.
Editors <- subset(MediaBucks, grepl("?editor", TITLE))
summary(Editors$Salary)
#3: Find highest paid editor. Resent them.
Editors %>% 
  filter(Salary == 245000)
#4: Make a table for any position involving data

Data <- subset(MediaBucks, grepl(("?Data"), TITLE))
summary(Editors$Salary)
#case sensitive search! 
str_detect(colnames(MediaBucks$TITLE), fixed("data", ignore_case=TRUE))



#-------------------------------------------------------------------#
#      What You Have Learned So Far
#-------------------------------------------------------------------#  

# How to navigate in R studio
# How to install libraries and packages 
# How to import a .csv file into R: read.csv
# How to obtain summary statistics (summary)
How to create a new calculated field



#---------------------------------------------#
#   Tutorials                                 #
#---------------------------------------------#
All Cheat Sheets
https://www.rstudio.com/resources/cheatsheets/
  
  #MaryJo Webster tutorials
  
  http://mjwebster.github.io/DataJ/R_tutorials/opiate_deaths.nb.html
https://github.com/mjwebster/R_tutorials/blob/master/Using_R.Rmd

#Aldhous' R tutorial
http://paldhous.github.io/NICAR/2018/r-analysis.html

Ron Campbell Lecture
https://github.com/roncampbell/NICAR2018/blob/master/Intro%20to%20R.md

R Part 2
http://paldhous.github.io/NICAR/2018/r-analysis.html

R3
https://github.com/Caelainn/R3_NICAR18

#Excellent Tutorial Spelling out Excel and Comparable Commands in R
First Data analysis in R
https://trendct.org/2015/06/12/r-for-beginners-how-to-transition-from-excel-to-r/
  
  https://docs.google.com/presentation/d/1O0eFLypJLP-PAC63Ghq2QURAnhFo6Dxc7nGt4y_l90s/edit#slide=id.g1bc441664e_0_59


Andrew Ba Tran first Data Analysis Steps Using R
https://docs.google.com/presentation/d/1O0eFLypJLP-PAC63Ghq2QURAnhFo6Dxc7nGt4y_l90s/edit#slide=id.p


Tirelli handout
http://r4ds.had.co.nz/data-visualisation.html

http://www.r-project.org
http://www.johndcook.com
http://bostondatacommunity.org/onlinecourses/
http://www.computerworld.com/article/2497143/business-intelligence/business-intelligence-beginner-s-guide-to-r-introduction.html
@abtran


Simply Statistics Blog Explains R
https://simplystatistics.org/post/
  
  #Charting
  https://www.rdocumentation.org/packages/ggplot2/versions/1.0.1/topics/geom_bar

http://www.cookbook-r.com/Graphs/Bar_and_line_graphs_(ggplot2)/
  
  Base R Cheat Sheet
https://www.povertyactionlab.org/sites/default/files/r-cheat-sheet.pdf


#-----------------------------------------------------------------#
#  Stopped Jan 9
#-----------------------------------------------------------------#

Add a table where we count companies, men and women

Case insensitive searching
https://stackoverflow.com/questions/5671719/case-insensitive-search-of-a-list-in-r

Data <- subset(MediaBucks, grepl(("?Data"), TITLE))
summary(Editors$Salary)
#case sensitive search! 
str_detect(colnames(MediaBucks$TITLE), fixed("data", ignore_case=TRUE))





#Table with Gender and Race Counted

MediaSalary %>%
  select(Gender, Race, Salary) %>% 
  count(Gender)

%>%
  county(Race)

df3 <- filter(murders, Relationship_label %in% c("Husband", "Boyfriend")

# Create a subset chart to show the top 10 by Pct Change
TopTags <- filter(AOC_table, (Freq > 2) & (Freq < 36))
              
              
df3 <- filter(MediaSalary, Gender %in% c("male", "female"))
                        
#Good R Tutorial with Basic Statistics
https://www.princeton.edu/~otorres/sessions/s2r.pdf

#-------------------------------------------------------------------#
#Build a chart - Total loan disbursement
#-------------------------------------------------------------------#

hist(MediaBucks$Salary)
barplot(MediaBucks$Salary)
plot(MediaBucks$Salary)

#library(ggplot2)

MediaBucks %>% ggplot(aes(y = Salary, x=Gender)) +
  geom_bar(stat = "identity") +
  #coord_flip() +     #this makes it a horizontal bar chart instead of vertical
  labs(title = "Your title here", 
       subtitle = "Subtitle and source ",
       caption = "Graphic by Rob Wells",
       x="County",
       y="Population growth 2016-2017")

#stopped Jan 9

#Build a summary table by Gender, Race
Summary <- MediaSalary %>% 
  select(Gender, Race, Salary) %>% 
  group_by(Gender) %>% 
  summarize(Total = sum(Salary, na.rm=TRUE)) %>% 
  group_by(Race) %>% 
  summarize(Total = sum(Salary, na.rm=TRUE))

#stopped Jan 7 
MediaSalary %>%
  select(Gender, Race, Salary) %>% 
  count(Gender) %>% 
  ungroup() %>% 


#company and gender
Summary <- MediaBucks %>% 
  select(COMPANY, Gender, Salary) %>% 
  group_by(Gender, COMPANY) %>% 
  summarize(Avg_Salary = mean(Salary, na.rm=TRUE)) %>% 
  arrange(desc(n))


#Build a simple summary table
Summary <- MediaBucks %>% 
  select(COMPANY, LOCATION, Salary2) %>% 
  filter(Salary >= 100000) %>%  
  group_by(COMPANY) %>% 
  count(COMPANY) %>% 
  filter(n>2) %>% 
  arrange(desc(n))

#Misc Cleaning  
# Clean up column names to they are R friendly
ArkCensus <- janitor::clean_names(ArkCensus)
View(ArkCensus)



# another method Rename a specific column
colnames(ArkCensus)[5] <- "BaseEstimate"
View(ArkCensus)


#Create a New Column and a Formula: Percents of Washington County as Whole of the state of Arkansas

ArkCensus$Pct2017 <- ((ArkCensus$x2017-ArkCensus$x2016)/(ArkCensus$x2016))

#To quickly format into percents, load
install.packages("formattable")
library(formattable)

ArkCensus$Pct2017 <- percent(ArkCensus$Pct2017)

View(ArkCensus)



#-----------------------------------------------------------------#
#   PART 2: Data Cleaning Exercise!
#-----------------------------------------------------------------#

#Data Cleaning Exercise!


#Load table - Media Salaries - 
MediaBucks <- rio::import("https://docs.google.com/spreadsheets/d/1SP3Sqqdv6R8chFamjtgdNlOrUar-hJXvkMSeha2mHQ8/edit#gid=0", which = "1", skip=3)
#
#Look at the table
View(MediaBucks)
#What happened?
#R grabbed the spreadsheet from the URL
#We told R to grab the first sheet, Salaries
#We told R to skip the first three rows - we didn't need that and it would cause problems
#
#For example, we don't skip the first three rows:
Bad_MediaBucks <- rio::import("https://docs.google.com/spreadsheets/d/1SP3Sqqdv6R8chFamjtgdNlOrUar-hJXvkMSeha2mHQ8/edit#gid=0", which = "1")
#
#Look at the table
View(Bad_MediaBucks)
#And we have some problems. Name them!

#Note that this data table has sortable headers. Pretty goddamned nice.  

# How many rows?  
nrow(MediaBucks)

# How many columns?
ncol(MediaBucks)

# Let's look at the first six rows
head(MediaBucks)

#Check data types
glimpse(MediaBucks)
#
#Problem? What is it?
#
#Ya can't do math on <chr> - Need to convert to <num>
#
#Data Cleaning Time!
#
#What do we need to clean?
str(MediaBucks)
colnames(MediaBucks)
#
#Answer: SALARY AND YEARS OF EXPERIENCE
#
#Before doing major surgery to a column, make a copy
MediaBucks$SALARY1 <- MediaBucks$SALARY

#Use as.numeric to convert to salary
MediaBucks$SALARY2 <- as.numeric(MediaBucks$SALARY1)
View(MediaBucks)
#Why was that a bad idea?
#
#One quick and brutal way to extract numbers from this messy dataset
MediaBucks$SALARY1 <- readr::parse_number(MediaBucks$SALARY1)
#
#Problem with this? 
#We lose the detail 
#Parse it out!
MediaBucks2 <- MediaBucks
#
#split Salary column into two
#https://stackoverflow.com/questions/7069076/split-column-at-delimiter-in-data-frame
MediaBucks2 <- separate(data = MediaBucks2, col = SALARY1, into = c("Salary3", "Details"), sep = " ", extra = "merge", fill = "right")
#
#Parse the main salary data
MediaBucks2$Salary4 <- readr::parse_number(MediaBucks2$Salary3)
MediaBucks2$Salary4 <- as.numeric(MediaBucks2$Salary4)
str(MediaBucks2)
#
#Create a subset with no salary qualifications
MediaBucks3 <- MediaBucks2 %>% 
  filter(is.na(Details)) %>% 
  select(TITLE, COMPANY, `Gender Identity / Ethnicity`, `YEARS OF EXPERIENCE`, LOCATION, `JOB DUTIES`, Salary4) 

nrow(MediaBucks3)
#We now have 1,445 records from the original 1,662 records. We can deal with the others later
#
#Here is a quick way to view the range of your data  
summary(MediaBucks3$Salary4)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0   43000   60000   63942   77119  770000       5 
#Median salary for these people is $60,000

  
  #Build a simple summary table
  Summary <- MediaBucks3 %>% 
  select(COMPANY, LOCATION, Salary4) %>% 
  filter(Salary4 >= 100000) %>%  
  group_by(COMPANY) %>% 
  count(COMPANY) %>% 
  filter(n>2) %>% 
  arrange(desc(n))
  
  #-----------------------------------------------------------------#
  #  notes
  #-----------------------------------------------------------------#

  #Extract Words from the Salary column
  MediaBucks2$detail <-str_extract_all(MediaBucks2$SALARY, "[a-z]+")
  #Clean detail
  
  MediaBucks$SALARY1 <- readr::parse_number(MediaBucks$SALARY1)
  
  MediaBucks2$detail <-str_split(MediaBucks2$SALARY1, " ")
  
  X<- MediaBucks2 %>% separate(SALARY, c("Detail1", "Detail2"), extra = "merge", fill = "left")
  
  MediaBucks2$detail2 <- readr::guess_parser(MediaBucks2$SALARY)
  
  liremove all special characters
  gsub("[[:punct:]]", "", c)
  
  #Do we need CAD - if we have the city in Canada?
  
  #Remove strings: $ and # and k  and / 
  
  MediaBucks$SALARY1 <- gsub("\\$", "", MediaBucks$SALARY1) 
  MediaBucks$SALARY1 <- gsub("\\#", "", MediaBucks$SALARY1) 
  #-- earnings$TOTAL.EARNINGS <- gsub("\\$", "", MediaBucks$SALARY) 
  



#Write.csv
write.csv(Washington1, "Washington1.csv")


#-------------------------------------------------------------------#
#Build a chart - Total loan disbursement
#-------------------------------------------------------------------#

library(ggplot2)

TopGrowthChart <- ggplot(TopGrowth, aes(x = county, y=Pct2017)) +
  geom_bar(stat = "identity") +
  coord_flip() +     #this makes it a horizontal bar chart instead of vertical
  labs(title = "Your title here", 
       subtitle = "Subtitle and source ",
       caption = "Graphic by Rob Wells",
       x="County",
       y="Population growth 2016-2017")
plot(TopGrowthChart)




