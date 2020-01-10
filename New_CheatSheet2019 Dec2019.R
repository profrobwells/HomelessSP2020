# "Introduction to R - Spring 2018"
# Rob Wells, PhD
# 1/15/2019

#source functions
source("Functions.R")


#fix value in df data.frame[row_number, column_number] = new_value
manual[1,1] = 876

#check if two dfs match
anti_join(DOG, DOGtype, by=c("article_nmbr", "article_nmbr"))
#http://zevross.com/blog/2014/08/05/using-the-r-function-anti_join-to-find-unmatched-records/

#Look at specific articles
#Examine Most Negative Article
Neg_Stories[which(Neg_Stories$n == max(Neg_Stories$n)),]
#Examine specific article
NYT2 <- NYT[which(NYT$newarticle == "90"),]
#To examine a specific article by article number - assumes the text
#has been tokenized by line and indexed
article <- WSJ_Broadfilter %>%
  filter(article_nmbr == 641, text != "")
#
cat(article$text, sep = "\n")


#---------------------------------------------------
Commands
Clean corpus, part 1

#translate all letters to lower case
clean_corpus <- tm_map(sms_corpus, tolower)
# remove numbers
clean_corpus <- tm_map(clean_corpus, removeNumbers)
# remove punctuation
clean_corpus <- tm_map(clean_corpus, removePunctuation)
Finally, remove the excess white space.
clean_corpus <- tm_map(clean_corpus, stripWhitespace)

Regular Expressions Cheat sheet
https://www.regular-expressions.info/rlanguage.html

Text Cleaning
https://cran.r-project.org/web/packages/textclean/textclean.pdf


`grep` practice

Extract the entries with . Note this is a metacharacter that must be escaped.
bb <- c("Once", "the", "3rd", "**alarm**", "sounds", "...")
grep("\\.", bb, value=T)

need to search by wildcards
exchange2 <- Bigrams_BF_all %>% 
  filter(word1=="exchange" | word1=="currency" | word2=="currency" | word1==“manipulat?!XXX")
         
         
         
         Print out a story
         https://www.tidytextmining.com/usenet.html
         Let’s check this by looking at the most positive message in the whole dataset. To assist in this we could write a short function for printing a specified message.
         print_message <- function(group, message_id) {
         result <- cleaned_text %>%
         filter(newsgroup == group, id == message_id, text != "")
         
         cat(result$text, sep = "\n")
         }
         
         print_message("rec.sport.hockey", 53560)



#---------------------------------------------------#
#Tables
#---------------------------------------------------#

#Build a simple summary table
count <- cos_sum_sentiment %>% 
  select(pattern, article_nmbr) %>% 
         group_by(pattern) %>% 
         count(pattern) %>% 
         ungroup()


#Build Table of Common Wells Words 
CommonWellsWords <- tweet_words %>%
  count(word) %>%
  filter(sum(n) >= 5) %>%
  ungroup() %>% 
  arrange(desc(n))

#Summary table by year
WSJ_table_sentiment <- BF_WSJ_sentiment %>% 
  select(year, score) %>% 
  group_by(year) %>% 
  summarise(sentiment = sum(score)) 

# Here's an elegant way to create a Tweets by Month, Year Table
Coulter_y_Month <- Coulter %>%
  select(created_at, yearmon, text, screen_name) %>%   
  group_by(yearmon) %>%
  count(yearmon)

#rbind
#Create Name Label
Trump_bigram_counts1$Name <- "Trump"
Pelosi_bigram_counts1$Name <- "Pelosi"

#Using RBind, connect the 2 charts
Trump_Pelosi_Bigrams <- rbind(Trump_bigram_counts1, Pelosi_bigram_counts1)


#

#Data Cleaning




#Filter out some garbage
junk <- c("http", "mt", "rt")

CommonWellsWords <- CommonWellsWords %>%
  filter(!word %in% junk)



Top_People <- select(Coulter2, created_at, text, is_retweet, mentions_screen_name) %>% 
  mutate(PeopleCount = mentions_screen_name) %>% 
  count(PeopleCount) %>%
  drop_na() %>% 
  top_n(15, n) %>%
  arrange(desc(n))


# ------- Get Organized --------- #  

###Set Working Directory. My directory "~/Dropbox/Classes/Data-Analysis-Class-Jour-405v-5003" is an example

getwd()
setwd("~/Dropbox/Classes/Data-Analysis-Class-Jour-405v-5003")

#  Here is some basic information about R  
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
# Create an R Project in that folder


#Install software to grab data
##rio handles more than two dozen formats including tab-separated data (with the extension .tsv), 
#JSON, Stata, and fixed-width format data (.fwf).
install.packages("rio") 
install.packages("tidyverse")
install.packages("readxl")
install.packages("dplyr")
install.packages("janitor")
install.packages("here")

### Load Libraries  
#Libraries are bits of software you will have to load each time into R to make things run. Here, we will **load library(tidyverse) and library (readxl)**:  

library (tidyverse)
library (readxl)
library (dplyr)
library(rio)
library(janitor)
library(here)


#Get census data
#Check the website: 
#https://factfinder.census.gov/bkmk/table/1.0/en/PEP/2017/PEPANNRES/0400000US05.05000


#Load table - ArkCensus.csv - 
download.file("https://bit.ly/2FxLJHj", "ArkCensus.csv")

#It downloaded it to your current working directory. Use this command to find where it is
getwd() 

#Import using Rio. Make sure ArkCensus is in your working directory!
ArkCensus <- rio::import("ArkCensus.csv")

# ------- Basic Data Exploration--------- # 

data(MCAS)
View(MCAS)
head(MCAS)
tail(MCAS)
names(MCAS)
summary(MCAS) 
str(MCAS)

dim(): shows the dimensions of the data frame by row and column
str(): shows the structure of the data frame
summary(): provides summary statistics on the columns of the data frame
colnames(): shows the name of each column in the data frame
head(): shows the first 6 rows of the data frame
tail(): shows the last 6 rows of the data frame
View(): shows a spreadsheet-like display of the entire data frame
https://rveryday.wordpress.com/2016/11/29/examine-a-data-frame-in-r-with-7-basic-functions/

#Look at the table
View(ArkCensus)

#View specific columns
# colnames(bigrams)
#[1]"V1"#"bigram"# "n"#"year"#"pub"   
#        > View(bigrams[1:25, pub])
#         > View(bigrams[1:25, bigram])


#Note that this data table has sortable headers. Pretty goddamned nice.  



# How many rows?  
nrow(ArkCensus)

# How many columns?
ncol(ArkCensus)

# Let's look at the first six rows
head(ArkCensus)

#Check data types
glimpse(ArkCensus)

#Here is a quick way to view the range of your data  
summary(ArkCensus)

# Clean up column names to they are R friendly
ArkCensus <- janitor::clean_names(ArkCensus)
View(ArkCensus)



# another method Rename a specific column
colnames(ArkCensus)[5] <- "BaseEstimate"
View(ArkCensus)

#rename a specific value in a df
# Replace the data in a field based on equal to some value
#SchoolData$Grade[SchoolData$Grade==5] <- "Grade Five"
x$type3[x$type3==TRUE] <- "News"
x$type3[x$type3==FALSE] <- "Opinion"


#Create a New Column and a Formula: Percents of Washington County as Whole of the state of Arkansas

ArkCensus$Pct2017 <- ((ArkCensus$x2017-ArkCensus$x2016)/(ArkCensus$x2016))

#To quickly format into percents, load
install.packages("formattable")
library(formattable)

ArkCensus$Pct2017 <- percent(ArkCensus$Pct2017)

View(ArkCensus)


#regular expressions
https://www.regular-expressions.info/

# Sort to see biggest-smallest descending in population from 2016-2017 

ArkCensus <- ArkCensus[order(-ArkCensus$Pct2017),]
View(ArkCensus)

# What is the average population change?

mean(ArkCensus$Pct2017)
median(ArkCensus$Pct2017)

#Find all counties with population growth above 1%

Above1pct <- ArkCensus%>%select(county, Pct2017)%>%filter(Pct2017 > .01)

#Build a table with places with upper quantile of crime
quantile(ArkCensus$Pct2017)

# the Upper Quantile is 0.74% to 2.94%
TopGrowth <- ArkCensus%>%select(county, Pct2017)%>%filter(Pct2017 > 0.0074)

#Find all places with below average crime

MajorLosers <- ArkCensus%>%select(county, Pct2017)%>%filter(Pct2017 < -0.0093)

#Filter
Filter
Washington1 <- subset(ArkCensus, County=="Washington County, Arkansas")  

#Using Wildcards
WashingtonCo <- subset(ArkCensus, grepl("^Washington", County))


#Write.csv
write.csv(Washington1, "Washington1.csv")


#-------------------------------------------------------------------#
#Build a chart - Total Growth
#-------------------------------------------------------------------#

library(ggplot2)
TopGrowthChart <- ggplot(TopGrowth, aes(x = reorder(county, -Pct2017), y = Pct2017))  +
  geom_bar(stat = "identity") +
  coord_flip() +
labs(title = "Top Growing Counties in Arkansas", 
subtitle = "U.S. Census Data, 2017: https://factfinder.census.gov",
caption = "Graphic by Rob Wells",
x="County",
y="Population growth 2016-2017")
plot(TopGrowthChart)


#-------------------------------------------------------------------#
#      What You Have Learned So Far
#-------------------------------------------------------------------#  

# How to navigate in R studio
# How to install libraries and packages 
# How to import a .csv file into R: read.csv
# How to obtain summary statistics (summary)
# How to create a new calculated field

#Title: Downloading data
#Jan 22 2019 with Census Exercise

#rio
install.packages("rio") 

#rio handles more than two dozen formats including tab-separated data (with the extension .tsv), 
#JSON, Stata, and fixed-width format data (.fwf).

StudentLoans <- rio::import('./Data/AR2016_SMALL.csv')
View(StudentLoans)

# Number columns
ncol(StudentLoans)

#vignettes: Learn about packages and commands
browseVignettes("rio")
??rio


#--------------------------------------------------------------------#
#Converting character strings into numeric
#--------------------------------------------------------------------#

#What is the character type?
library(dplyr)

#Tibble or Dplyr for the glimpse function
dplyr::glimpse(StudentLoans)
tibble::glimpse(StudentLoans)  

#chr stands for character vectors, or strings.
#int stands for integers.
#dbl stands for doubles, or real numbers.
#dttm stands for date-times (a date + a time).

#Convert numbers to "numeric" data
#We want to turn all columns after HMC2 into numeric
#HMC2 is Column #10

#Check you have the right names you want to convert
colnames(StudentLoans[10:102])


StudentLoans[10:102] <- lapply(StudentLoans[10:102], as.numeric)
glimpse(StudentLoans)  
#Data changed from <chr> to <dbl>


#Run stats
summary(StudentLoans)

#Do some math - average number of white students
mean(StudentLoans$UGDS_WHITE)

#Why the NA result? NA= Missing Value

# list rows of data that have missing values 
#mydata[!complete.cases(mydata),]

StudentLoans[!complete.cases(StudentLoans)]

#Doing math on columns with missing values

sum(StudentLoans$UGDS_WHITE, na.rm=TRUE)
mean(StudentLoans$TUITIONFEE_IN, na.rm=TRUE)


#--------------------------------------------------------------------#
# Loading Data from Scratch
#--------------------------------------------------------------------#

#Loading data
#RSQlite - read data from a database
#xlsx - read in Excel spreadsheets

#Import Income data from US Census
#INCOME IN THE PAST 12 MONTHS (IN 2017 INFLATION-ADJUSTED DOLLARS) 
#2013-2017 American Community Survey 5-Year Estimates. S1901. All Arkansas Counties

#https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_17_5YR_S1901&prodType=table

#Load Data
ArkCo_Income_2017 <- rio::import("Data/ArkCo_Income_2017.csv")

Load a specific sheet: sheet = "Chinacleaned"
FDI <- rio::import("./Narrative Analysis/Chinese Companies Coverage/China FDImarkets FDI US state.xlsx", sheet = "Chinacleaned")


#Look at the table
View(ArkCo_Income_2017)

# How many rows?  
nrow(ArkCo_Income_2017)

# How many columns?
ncol(ArkCo_Income_2017)

#Install dplyr or tibble for the glimpse function if you haven't already
#library (tibble)

#Check data types
glimpse(ArkCo_Income_2017)

#What is the issue? (Don't read ahead and spoil the fun)

#You are reading ahead
#Caught you.



#What is the issue? 




#Delete First Row Headers
#Reimport the data and skip the first row
#read.csv(.... , skip=1)

ArkCo_Income_2017 <- rio::import("Data/ArkCo_Income_2017.csv", skip=1)
View(ArkCo_Income_2017)

#Clean Headers - Janitor package
#library(janitor)

# Clean up column names to they are R friendly
ArkCo_Income_2017 <- janitor::clean_names(ArkCo_Income_2017)
View(ArkCo_Income_2017)


# Still need to fix column names
colnames(ArkCo_Income_2017)

#You can do it one at a time
#Column 4 households_estimate_total renamed to household_income
colnames(ArkCo_Income_2017)[4] <- "household_income"
colnames(ArkCo_Income_2017)

#change it back
colnames(ArkCo_Income_2017)[4] <- "households_estimate_total"
colnames(ArkCo_Income_2017)

Delete Columns
df <- FDI[ -c(6,7,11:13) ]


#------------------------------------------#
#Rename a whole slug of columns at once!
#So the following is a *little intense*
#------------------------------------------#

#Use setnames from the data.tablepackage will work on data.frames or data.tables
#Example
#library(data.table)
#setnames(d, old = c('a','d'), new = c('anew','dnew'))
#d


#We are changing all of the old column names to new ones
#That's 19 column names we are changing.

#New Names
#library(data.table)
data.table::setnames(ArkCo_Income_2017, old = c('id', 'id2', 'geography', 'households_estimate_total', 
                                                'households_estimate_less_than_10_000', 'households_estimate_10_000_to_14_999', 
                                                'households_estimate_15_000_to_24_999', 'households_estimate_25_000_to_34_999', 
                                                'households_estimate_35_000_to_49_999', 'households_estimate_50_000_to_74_999', 
                                                'households_estimate_75_000_to_99_999', 'households_estimate_100_000_to_149_999', 
                                                'households_estimate_150_000_to_199_999', 'households_estimate_200_000_or_more',
                                                'households_estimate_median_income_dollars', 'households_estimate_mean_income_dollars',
                                                'households_estimate_percent_allocated_household_income_in_the_past_12_months',
                                                'households_estimate_percent_allocated_family_income_in_the_past_12_months',
                                                'households_estimate_percent_allocated_nonfamily_income_in_the_past_12_months'),
                     new = c('id','id2','geography','households_estimate_total','less10_000','10k_to_14_999','15k_to_24_999',
                             '25k_to_34_999', '35k_to_49_999','50k_to_74_999','75k_to_99_999','100k_to_149_999',
                             '150k_to_199_999','200k_plus','median_income','mean_income',
                             'pct_allocated_household_income','pct_allocated_family_income','pct_allocated_nonfamily_income'))

View(ArkCo_Income_2017)  

#Manipulating data
#dplyr - fast data work
#stringr - work with strings

#Data Management
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

# window functions are useful for grouped mutates
mtcars %>%
  group_by(cyl) %>%
  mutate(rank = min_rank(desc(mpg)))

#Use mutate to add together the percentages of low-wage households
ArkCo_Income_2017 <- ArkCo_Income_2017 %>%
  replace(is.na(.), 0) %>%
  mutate(Low_Wage_Households = rowSums(.[5:7]))

#--Export data 
#Write Export output this file to a CSV or Excel  write.csv or write.excel
write.csv(ArkCo_Income_2017,"ArkCo_Income_2017.csv") 



#Exercises
# 1) Create a column for working class households: $25,000 to $50,000
# 2) Create a column for middle class households: $50,000 to $150,000
# 3) Create a column for upper income households: More than $150,000
# 4) Using these percentages, create new columns for low-wage, working class, middle class, and upper income 
# and calculate the actual number of people in each income group
# This will require looking at the table data structure, so go to the census.gov link provided above


#Answers
# 1) Create a column for working class households: $25,000 to $50,000
ArkCo_Income_2017 <- ArkCo_Income_2017 %>%
  replace(is.na(.), 0) %>%
  mutate(WorkingClass = rowSums(.[8:9]))


# 2) Create a column for middle class households: $50,000 to $150,000
ArkCo_Income_2017 <- ArkCo_Income_2017 %>%
  replace(is.na(.), 0) %>%
  mutate(MiddleClass = rowSums(.[10:12]))


# 3) Create a column for upper income households: More than $150,000
ArkCo_Income_2017 <- ArkCo_Income_2017 %>%
  replace(is.na(.), 0) %>%
  mutate(UpperIncome = rowSums(.[13:14]))


# 4) Using these percentages, create new columns for low-wage, working class, middle class, and upper income 
# and calculate the actual number of people in each income group
# This will require looking at the table data structure, so go to the census.gov link provided above

#Copied this as a test
#ArkCensus$Pct2017 <- ((ArkCensus$x2017-ArkCensus$x2016)/(ArkCensus$x2016))


ArkCo_Income_2017$LowWagePop <- ((ArkCo_Income_2017$households_estimate_total*ArkCo_Income_2017$Low_Wage_Households)/100)

ArkCo_Income_2017$WorkingClassPop <- ((ArkCo_Income_2017$households_estimate_total*ArkCo_Income_2017$WorkingClass)/100)

ArkCo_Income_2017$MiddleClassPop <- ((ArkCo_Income_2017$households_estimate_total*ArkCo_Income_2017$MiddleClass)/100)

ArkCo_Income_2017$UpperIncomePop <- ((ArkCo_Income_2017$households_estimate_total*ArkCo_Income_2017$UpperIncome)/100)

#For amusement, see if they all add up
ArkCo_Income_2017 <- ArkCo_Income_2017 %>%
  replace(is.na(.), 0) %>%
  mutate(SumPop = rowSums(.[24:27]))


#Eyeball the two columns, household_estimate_total and our SumPop
#df1 <- select(AR2016ALL, V4:V8, V10:20)
PopCheck <- select(ArkCo_Income_2017, households_estimate_total, SumPop) 

#which ones varied the most?

PopCheck$variance <- (ArkCo_Income_2017$households_estimate_total- ArkCo_Income_2017$SumPop) 

#nerdy checking individual
ArkCo_Income_2017 <- ArkCo_Income_2017 %>%
  +   replace(is.na(.), 0) %>%
  +   mutate(SumIndivdPct = rowSums(.[5:14]))

#more sum groups
ArkCo_Income_2017 <- ArkCo_Income_2017 %>%
  replace(is.na(.), 0) %>%
  mutate(SumGroupPct = rowSums(.[20:23]))


PopCheck <- select(ArkCo_Income_2017, households_estimate_total, SumPop, SumIndivdPct, SumGroupPct) 


#Other tools

#rename - Rename column(s).  
#bind_rows - Merge two data frames into one, combining data from columns with the same name.


#Other data cleaning tricks
#Change column to number format (first you have to strip out the $)  
#--The $ is a special character  
#-- earnings$TOTAL.EARNINGS <- gsub("\\$", "", earnings$TOTAL.EARNINGS) 


#Quick Data Viz
#Basic graphs
plot(ArkCo_Income_2017$median_income)

hist(ArkCo_Income_2017$median_income)  
boxplot(ArkCo_Income_2017$median_income)
barplot(ArkCo_Income_2017$median_income)
barplot(sort(ArkCo_Income_2017$median_income, decreasing = TRUE))


#--------------------------------------------------------------------#
#More Advanced Section from Machlis Book, Ch. 4
#--------------------------------------------------------------------#


#get data for tutorial
download.file("http://bit.ly/BostonSnowfallCSV", "BostonWinterSnowfalls.csv")

#load into memory
snowdata <- rio::import("BostonWinterSnowfalls.csv")



#Data Cleaning install own function in my own rmiscutils package 
#turns “character strings” -- numbers with commas back into numbers
pacman::p_load_gh("smach/rmiscutils")

#more software
install.packages("remotes")
install.packages("githubinstall")
githubinstall::gh_install_packages("rmiscutils")

install.packages("htmltab")
library(htmltab)
citytable <- htmltab(
  "https://en.wikipedia.org/wiki/List_of_United_States_cities_by_population", 
  which = 5)
colnames(citytable)

library(rmiscutils)
citytable$PopEst2017 <- number_with_commas(citytable$`2017estimate`)

#parsing numbers with readr
#After installing readr, you could generate numbers from the 
#2017 estimate column with readr:

citytable$PopEst2017 <- readr::parse_number(citytable$`2017 estimate`)


#More on Parsing rows with commas into columns
 with tidyr only: With tidyr 0.5.0 (and later), you can also just use separate_rows:"

separate_rows(v, director, sep = ",")
#You can use the convert = TRUE parameter to automatically convert numbers into numeric columns.
#https://stackoverflow.com/questions/13773770/split-comma-separated-strings-in-a-column-into-separate-rows



#-----------------------------#
# Notes on Ch. 4
#-----------------------------#  

#Ch 4 Import Data into R
#* rio
#* htmltab
#* readxl
#* googlesheets
#* pacman
#* janitor
#* rmiscutils (GitHub) or readr
#* tibble

rio
install.packages("rio") 
#--rio handles more than two dozen formats including tab-separated data (with the extension .tsv), JSON, Stata, and fixed-width format data (.fwf).
#Chr to Numeric
#This is one of R’s small annoyances: R generally doesn’t understand that 8,550 is a number. I dealt with this problem myself by writing my own function in my own rmiscutils package to turn all those “character strings” that are really numbers with commas back into numbers. Anyone can download the package from GitHub and use it.
#Skip rows
#eg %>% %>% %>% %>% inning rows that aren’t part of the data. If you know that the first few rows of an Excel spreadsheeet don’t have data you want, you can tell rio to skip one or more lines. The syntax is rio::import("mySpreadsheet.xlsx", skip=3) to exclude the first three rows. skip takes an integer.
#Import and create new column names
#Or, use a syntax such as rio::import("mySpreadsheet.xlsx", col_names = c("City", "State", "Population")) to set your own column names.
#Import second tab of SS
#If there are multiple tabs in your spreadsheet, the which argument will override the default of reading in the first worksheet. rio::import("mySpreadsheet.xlsx", which = 2) reads in the second worksheet.
#Slick trick to add a column and do a conversion
#It’s easy to add a column to a data frame. Currently, the Total column shows winter snowfall in inches. To add a column showing totals in Meters, you can use this format:
  snowdata$Meters <- snowdata$Total * 0.0254
#4.5 Convert zip codes into usable data. Boston zip codes with leading zeroes.


#Title: Machlis - Basic Data Visualization
#UpdatedJan 30 2019


#load software - Select NO when asked to restart
install.packages("ggplot2")
install.packages("dplyr")
install.packages("usethis")
install.packages("forcats")

#call software into memory
library(ggplot2)
library(dplyr)
library(usethis)
library(forcats)

#Basic demo
#You will run the commands from the Console below
demo(topic="graphics")

library(dplyr)

#Tutorial
#Import Data, Create Dataframe, Rename Columns
snowdata <- rio::import("data/BostonChicagoNYCSnowfalls.csv")
bostonsnow <- select(snowdata, Winter, Boston)
names(bostonsnow)[2] <- "TotalSnow"

#Doing the same thing but with pipe function
bostonsnow2 <- select(snowdata, Winter, Boston) %>%
  rename(TotalSnow = Boston)

#Doing the same thing but more efficiently
bostonsnow3 <- select(snowdata, Winter, TotalSnow = Boston) 

#Basic graphs
plot(bostonsnow$TotalSnow)

hist(bostonsnow$TotalSnow)
boxplot(bostonsnow$TotalSnow)
barplot(bostonsnow$TotalSnow)
barplot(sort(bostonsnow$TotalSnow, decreasing = TRUE))

#qplot
qplot(data=bostonsnow, y = TotalSnow)
qplot(y = bostonsnow$TotalSnow)

#basic ggplot2 - boxplot
ggplot(data=snowdata) + 
  geom_boxplot(aes(x = "Boston", y = Boston))

#dual box plots
ggplot(data=snowdata) + 
  geom_boxplot(aes(x = "Boston", y = Boston)) +
  geom_boxplot(aes(x = "Chicago", y = Chicago))

#bring in snowdata tidy
snowdata_tidy <- rio::import("data/snowdata_tidy.csv")

#view a tidy table
View(snowdata_tidy)

#Boxplot with ggplot
ggplot(snowdata_tidy, aes(x = City, y = TotalSnow)) +
  geom_boxplot()

#Line graphs
ggplot(snowdata_tidy, aes(x = Winter, y = TotalSnow, group = City)) +
  geom_line()

#ggplot with colors and points

ggplot(snowdata_tidy, aes(x = Winter, y = TotalSnow, group = City, color = City)) +
  geom_line()


#ggplot with colors and points
ggplot(snowdata_tidy, aes(x = Winter, y = TotalSnow, group = City, color = City)) +
  geom_line() +
  geom_point() 

#Filtered for two years, 1999 and 2000
snowdata_tidy21 <- filter(snowdata_tidy, Winter >= "1999-2000")
ggplot(snowdata_tidy21, aes(x = Winter, y = TotalSnow, group = City, color = City)) +
  geom_line() +
  geom_point()

#Barplots
ggplot(data = snowdata_tidy21, aes(x = Winter, y = TotalSnow, group = City, color = City)) +
  geom_col() 

#Not so ugly bars
ggplot(data = snowdata_tidy21, aes(x = Winter, y = TotalSnow, group = City, fill = City)) +
  geom_col(position = "dodge") 

test <- ggplot(human_rights)+
  aes(x = year, y = n, fill = name)+
  scale_y_continuous(labels = scales::comma) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "'Human Rights' in Economic News, 2000-2018", 
       subtitle = "NYT, WP, WSJ, NYT, Inside US Trade 2000-2018",
       caption = "Source: ProQuest, 4,334 articles. Graphic by Rob Wells",
       x="Month",
       y="Frequency of 'Human Rights'")

#Save high resolution
ggsave("test.png", width = 6.94, height = 4.79, dpi = 1000)

#Graph with percentages on the axis, adjusted scale, bars with figures

Z_AR_PCT %>% 
  filter(Pct_Chg > .13) %>% 
  ggplot(aes(x = reorder(region_name, Pct_Chg), y = Pct_Chg, fill = Pct_Chg)) +
  geom_col(position = "dodge", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_text(aes(label = Pct_Chg), hjust = -.5, size = 2) +
  scale_y_continuous(limits=c(0, .45),labels = scales::percent) +
  coord_flip() +
  labs(title = "Arkansas Real Estate Values, 2010-2019",
       subtitle = "Zillow housing index",
       caption = "Graphic by Rob Wells, 12-22-19",
       y="Index, Pct Change",
       x="City")




#-------------------------------------------------------------------#
#Build a chart - Snow
#-------------------------------------------------------------------#

library(ggplot2)
SnowChartBoston <- ggplot(bostonsnow, aes(x = reorder(Winter, TotalSnow), y = TotalSnow))  +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Snow", 
       subtitle = "lots of it",
       caption = "Graphic by Rob Wells",
       x="Years",
       y="snow in inches")
plot(SnowChartBoston)


#Notes from R for Data Scientists - Wickham
#https://r4ds.had.co.nz/

#Feb. 2 2019
install.packages('tidyverse')
install.packages(c("nycflights13", "gapminder", "Lahman"))

#If we want to make it clear what package an object comes from, we’ll use the package name followed by two colons, like dplyr::mutate(), or
#nycflights13::flights. This is also valid R code.

library(tidyverse)


#Do cars with big engines use more fuel than cars with small engines? 
#displ, a car’s engine size, in litres.
#hwy, a car’s fuel efficiency on the highway, in miles per gallon (mpg). 
#A car with a low fuel efficiency consumes more fuel than a car with a high fuel efficiency when they travel the same distance.
#To learn more about mpg, open its help page by running ?mpg.

mpg

#Create a ggplot
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

#3.2.3 A graphing template
#Let’s turn this code into a reusable template for making graphs with ggplot2. 
#To make a graph, replace the bracketed sections in the code below with a dataset, a geom function, or a collection of mappings.
#ggplot(data = <DATA>) + 
#  <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

ggplot(data = mpg)

#using color to distinguish class
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

#MPG categorical = manufacturer model cyl trans drv fl class
#continuous = disply cty hwy
#Map a continuous variable to color, size, and shape. 
#How do these aesthetics behave differently for categorical vs. continuous variables?
#Answer - they do not come up in discrete blocks by on a spectrum range

#Color by manufacturer
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color =manufacturer))

#Color and Size
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color =manufacturer, size=manufacturer))


#adding size
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class, color = class))

#What does the stroke aesthetic do? What shapes does it work with? (Hint: use ?geom_point)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class, stroke=20))

#What happens if you map an aesthetic to something other than a variable name, 
#like aes(colour = displ < 5)? Note, you’ll also need to specify x and y.

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = displ < 5))

#It gives you a true false by color

#Exercise:
#Exercise with ArkCo_Income_2017
#1. Create a plot chart with the top 10 counties with the greatest percentage of low-income population
#Your answer should look like this
#https://bit.ly/2BgPmyo

#2. Create a plot chart with the top 10 counties with the greatest percentage of upper-income population






#Answer to Monday Feb 4 Exercise

#---------------------------------------------------------------------#
#Exercise Answer:
#---------------------------------------------------------------------#

#https://bit.ly/2BgPmyo

#Pull in ArkCo_Income_2017
#Load Data
ArkCo_Income_2017 <- rio::import("https://raw.githubusercontent.com/profrobwells/Data-Analysis-Class-Jour-405v-5003/master/Data/ArkCo_Income_2017.csv", skip=1)

#Clean labels
ArkCo_Income_2017 <- janitor::clean_names(ArkCo_Income_2017)
View(ArkCo_Income_2017)

#Rename Columns
library(data.table)
data.table::setnames(ArkCo_Income_2017, old = c('id', 'id2', 'geography', 'households_estimate_total', 
                                                'households_estimate_less_than_10_000', 'households_estimate_10_000_to_14_999', 
                                                'households_estimate_15_000_to_24_999', 'households_estimate_25_000_to_34_999', 
                                                'households_estimate_35_000_to_49_999', 'households_estimate_50_000_to_74_999', 
                                                'households_estimate_75_000_to_99_999', 'households_estimate_100_000_to_149_999', 
                                                'households_estimate_150_000_to_199_999', 'households_estimate_200_000_or_more',
                                                'households_estimate_median_income_dollars', 'households_estimate_mean_income_dollars',
                                                'households_estimate_percent_allocated_household_income_in_the_past_12_months',
                                                'households_estimate_percent_allocated_family_income_in_the_past_12_months',
                                                'households_estimate_percent_allocated_nonfamily_income_in_the_past_12_months'),
                     new = c('id','id2','geography','households_estimate_total','less10_000','10k_to_14_999','15k_to_24_999',
                             '25k_to_34_999', '35k_to_49_999','50k_to_74_999','75k_to_99_999','100k_to_149_999',
                             '150k_to_199_999','200k_plus','median_income','mean_income',
                             'pct_allocated_household_income','pct_allocated_family_income','pct_allocated_nonfamily_income'))

#load dplyr to use the mutate function and create groups
library(dplyr)

#Create Groups
#Low Wage
ArkCo_Income_2017 <- ArkCo_Income_2017 %>%
  replace(is.na(.), 0) %>%
  mutate(Low_Wage_Households = rowSums(.[5:7]))

#Working Class households: $25,000 to $50,000
ArkCo_Income_2017 <- ArkCo_Income_2017 %>%
  replace(is.na(.), 0) %>%
  mutate(WorkingClass = rowSums(.[8:9]))

#Middle class households: $50,000 to $150,000
ArkCo_Income_2017 <- ArkCo_Income_2017 %>%
  replace(is.na(.), 0) %>%
  mutate(MiddleClass = rowSums(.[10:12]))

#Upper income households: More than $150,000
ArkCo_Income_2017 <- ArkCo_Income_2017 %>%
  replace(is.na(.), 0) %>%
  mutate(UpperIncome = rowSums(.[13:14]))

#Using these percentages, create new columns for low-wage, working class, middle class, and upper income 
# and calculate the actual number of people in each income group
ArkCo_Income_2017$LowWagePop <- ((ArkCo_Income_2017$households_estimate_total*ArkCo_Income_2017$Low_Wage_Households)/100)
ArkCo_Income_2017$WorkingClassPop <- ((ArkCo_Income_2017$households_estimate_total*ArkCo_Income_2017$WorkingClass)/100)
ArkCo_Income_2017$MiddleClassPop <- ((ArkCo_Income_2017$households_estimate_total*ArkCo_Income_2017$MiddleClass)/100)
ArkCo_Income_2017$UpperIncomePop <- ((ArkCo_Income_2017$households_estimate_total*ArkCo_Income_2017$UpperIncome)/100)

#Save this table
#Write Export output this file to a CSV or Excel  write.csv or write.excel
write.csv(ArkCo_Income_2017,"ArkCo_Income_2017_Modified.csv") 

#1. Create a bar chart with the top 10 counties with the greatest percentage of low-income population
#Find Top10Counties
#Use Dplyr - arrange
#https://dplyr.tidyverse.org/reference/arrange.html

Top10_LowWage <- ArkCo_Income_2017%>%select(geography, Low_Wage_Households)%>%arrange(desc(Low_Wage_Households))

#The cutoff for the top 10 is 39%
#Filter table to just that
Top10_LowWage <- Top10_LowWage%>%filter(Low_Wage_Households > 38.9)

#or try this
CommonCoulterWords <- tweet_words %>%
  count(word) %>%
  filter(sum(n) >= 5) %>%
  ungroup() %>% 
  arrange(desc(n))

#sort columns in ggplot
  ggplot(aes(x = reorder(pattern, -n), y=n, fill=pattern))+


#Chart It
library(ggplot2)

Top10_LowWage_Chart <- ggplot(data = Top10_LowWage) + geom_point(mapping
                                                                 =aes(x = reorder(geography, Low_Wage_Households), y = Low_Wage_Households,
                                                                      color = Low_Wage_Households, size = Low_Wage_Households)) + 
  coord_flip() +
  labs(title = "Arkansas Counties With Most Low Wage Households", 
       subtitle = "Source: U.S. Census Data, 2017",
       caption = "Graphic by Rob Wells",
       x="Counties",
       y="Percentage Households Less $25,000")
plot(Top10_LowWage_Chart)

#A bad version with the X axis labels bunched together
Top10_LowWage_Chart2 <- ggplot(data = Top10_LowWage) + geom_point(mapping
                                                                  =aes(x = geography, y = Low_Wage_Households,
                                                                       color = Low_Wage_Households, size = Low_Wage_Households)) + 
  
  labs(title = "Arkansas Counties With Most Low Wage Households", 
       subtitle = "Source: U.S. Census Data, 2017",
       caption = "Graphic by Rob Wells",
       x="Counties",
       y="Percentage Households Less $25,000")
plot(Top10_LowWage_Chart2)

#Version with X Axis labels adjusted
#myplot + theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
Top10_LowWage_Chart2 + theme(axis.text.x = element_text(angle = 45, vjust = 0.5))


#Version with chart colored with more than 42% poverty
Top10_LowWage_Chart3 <- ggplot(data = Top10_LowWage) + geom_point(mapping
                                                                  =aes(x = reorder(geography, Low_Wage_Households), y = Low_Wage_Households,
                                                                       color = Low_Wage_Households < 42, size = Low_Wage_Households)) + 
  coord_flip() +
  labs(title = "Arkansas Counties With Most Low Wage Households", 
       subtitle = "Source: U.S. Census Data, 2017",
       caption = "Graphic by Rob Wells",
       x="Counties",
       y="Percentage Households Less $25,000")
plot(Top10_LowWage_Chart3)



#Done with bar graph
Top10_LowWage_Chart4 <- ggplot(Top10_LowWage, aes(x = reorder(geography, Low_Wage_Households), y = Low_Wage_Households))  +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Arkansas Counties With Most Low Wage Households", 
       subtitle = "Source: U.S. Census Data, 2017",
       caption = "Graphic by Rob Wells",
       x="Counties",
       y="Percentage Households Less $25,000")
plot(Top10_LowWage_Chart4)


#2. Create a bar chart with the top 10 counties with the greatest percentage of upper-income population


#-------------------------------------------------------------------------#
#Stop Here Monday#
#-------------------------------------------------------------------------#



#Code snippets
#snippet myg_barplot_grouped
#ggplot(${1:mydataframe}, aes(${2:xcolname}, ${3:ycolname}, group = ${4:groupbycolname},
#                             fill = ${4:groupbycolname})) +
  geom_col(position = "dodge")

ggplot(mydataframe, aes(xcolname, ycolname, group = groupbycolname, 
                        fill = groupbycolname)) +
  geom_col(position = "dodge")

boston10 <- bostonsnow %>%
  top_n(10, TotalSnow) %>%
  arrange(desc(TotalSnow))

ggplot(data = boston10, aes(x = Winter, y = TotalSnow)) + 
  geom_col(fill = "rainbow") +
  theme_minimal() 

ggplot(data = boston10, aes(x = Winter, y = TotalSnow)) + 
  geom_col(fill = "dodgerblue4") +
  theme_minimal() +
  labs(title = " XXXX Any Title Here",
       subtitle = "XXXX Any subtitle", 
       caption = "Source: XXXXX")

#With ordered bars
ggplot(boston10, aes(x=fct_reorder(Winter, TotalSnow), y=TotalSnow)) + 
  geom_col()

#month factor
month_factor <- factor(month.name, levels = month.name, ordered = TRUE) 
month_factor


#Adjust the axes
myplot + theme(axis.text.x = element_text(angle = 45, vjust = 0.5))


# "First Chart in R - January 2019"
# Rob Wells, PhD
# 12/22/2018

# ------- Get Organized --------- #  

###Set Working Directory. My directory "Student-Loan-Data-R" is an example

getwd()
setwd("~/Dropbox/Student-Loan-Data-R")


library (tidyverse)
library (readxl)
library (dplyr)

#-------------------------------------------------------------------#
#Load the data from the Introduction to R exercise
#-------------------------------------------------------------------#
#Pull in Alexandra Ocasio-Cortez Twitter feed
#Load Data
AOC <- rio::import("https://raw.githubusercontent.com/profrobwells/Data-Analysis-Class-Jour-405v-5003/master/Data/AOC.csv")

#Clean labels
AOC <- janitor::clean_names(AOC)

#Look at table
View(AOC)
str(AOC)

#What do these columns mean?
#https://developer.twitter.com/en/docs/tweets/data-dictionary/overview/user-object

#Question #1: What is the most popular tweet, what did it say and what day did it happen?
#Build a Smaller Table with four elements: 
#the date of the tweet, the text, count of favorites, count of retweets
#and sort by number of favorites

#Answer
library(dplyr)
#AOC2 <- AOC %>%select(created_at, text, favorite_count, retweet_count) %>%arrange(desc(favorite_count)) 
#I hear the GOP thinks women dancing are scandalous. Wait till they find out Congresswomen dance too! 💃🏽 Have a great weekend everyone :) https://t.co/9y6ALOw4F6
#Retweeted 785,382 times
#Jan 4, 2019

#Question #2: What is the third most popular retweet, what did it say and what day did it happen?

#Answer
#AOC3 <- AOC %>%select(created_at, text, favorite_count, retweet_count) %>%arrange(desc(retweet_count)) 
#Oct 18, 2018
#President @BarackObama doesn't have time for these 7 excuses not to vote. https://t.co/2Etpm6taTq 
#--111,234 times

#Question #3: Count the tweets by day

Count <- AOC %>% select(created_at) %>% group_by(created_at) %>% summarize()


#create date objects with lubridate
#NYT <- na.omit(NYT)
NYT$date <- NYT$date %>% mdy
#a create a new year column
NYT$year <- year(NYT$date)


library(lubridate)
AOC2 <- -c(AOC2$dates)

AOC2$date < AOC2$created_at

AOC2$date <- ymd(AOC2$created_at)

CommonTweets <- AOC %>%
  count(created_at) %>%
  filter(sum(n) >= 5) %>%
  spread(source, n, fill = 0) %>%
  ungroup()

#title: "Assignment#1_KEY_Static_Graphic"
# 2/9/2019"

#THIS IS THE KEY#

#Assignment #1 - Data Analysis for Journalists - Jour 405v
###Static Graphic - Managing Data in R.   Due Feb 6

#Students will use R Studio to gather, analyze and visualize FBI Uniform Crime data for Arkansas.    

#1. Load the appropriate R software packages to analyze and calculate data  
###Set Working Directory. 
getwd()
setwd("~/YOUR FOLDER PATH NAME HERE")

#load packages needed for this; needs to be done every time
#these are all part of tidyverse
#for importing csv file
library(readr) 
#for analysis
library(dplyr) 
#for creating charts
library(ggplot2)  
#themes for data viz
library(ggthemes) 

#2. Import FBI Uniform Crime data for Arkansas for 2017: arkansas_crime.xls 
ArkCrime2017 <- rio::import("Data/arkansas_crime.xls", skip=4)
#Cut the crud at the end of the table
ArkCrime2017 <- ArkCrime2017[-c(187),]
View(ArkCrime2017)

#3. Clean labels, convert appropriate columns to numeric data    
ArkCrime2017 <- janitor::clean_names(ArkCrime2017)

#Rename state to city
colnames(ArkCrime2017)[1] <- "city"

#Check the labels and data types
str(ArkCrime2017)
colnames(ArkCrime2017)

#If you needed to convert to numeric, use this
#ArkCrime2017$violent_crime <- as.numeric(ArkCrime2017$violent_crime)

#4: Provide the R code for a summary of the statistics, the number of rows, number of columns   
View(ArkCrime2017)
str(ArkCrime2017)  
nrow(ArkCrime2017)
summary(ArkCrime2017)

#5: Create a new crime rate column with violent crime per 10,000 residents   

ArkCrime2017$CrimePer10000 <- (ArkCrime2017$violent_crime / ArkCrime2017$population) *10000 
View(ArkCrime2017)

#6: Sort the table descending with the highest crime rate on top   

# Sort by largest percentage change
ArkCrime2017 <- ArkCrime2017[order(-ArkCrime2017$CrimePer10000),]

OR
ArkCrime2017a <- ArkCrime2017%>%select(city, CrimePer10000)%>%arrange(desc(CrimePer10000))


#7: Create separate table with just the top five counties' crime rate
Top5 <- ArkCrime2017 %>% select(city, CrimePer10000) %>% filter(CrimePer10000 >= 163)
View(Top5)  

#8: Export these two tables as .csv files    
write.csv(ArkCrime2017, "ArkCrime2017.csv")
write.csv(Top5, "Top5.csv")

#July 2: Formatting ggplot
http://www.sthda.com/english/wiki/ggplot2-axis-ticks-a-guide-to-customize-tick-marks-and-labels

#News only line
EF_type_sent %>%
  filter(type=="News") %>% 
  filter(year!=2019) %>%
  group_by(year, type) %>% 
  summarize(mean = mean(score))  %>% 
  ggplot(aes(x = year, y = mean)) +
  geom_smooth(se=FALSE)+
  scale_x_continuous(breaks=c(2000:2018)) +
  theme(axis.text.x = element_text(angle=90)) +
  #scale_x_date() +
  #scale_y_reverse() +
  #coord_flip()  +
  labs(title = "Sentiment News Articles- China Economic News", 
       subtitle = "Sentiment Score: NYT, WSJ, LAT, WP, IUT: Economic Filter, 2000-2018.",
       caption = "Bing sentiment dictionary. Source: ProQuest. Graphic by Rob Wells",
       x="Year",
       y="Sentiment score")


# Mean score per year
EF_type_sent %>% 
  filter(year!=2019) %>%
  filter(type=="Opinion") %>% 
  group_by(year, Pub) %>% 
  summarize(mean = mean(score))  %>% 
  ggplot(aes(x= year, y= mean, fill= Pub)) +
  geom_bar(stat="identity", width=.5, position = "dodge") +
  scale_y_reverse() +
  coord_flip()  +
  labs(title = "Sentiment Of Opinion Articles: China Economic News", 
       subtitle = "Sentiment Score: NYT, WSJ, LAT, WP, IUT: Economic Filter, 2000-2018.",
       caption = "Bing sentiment dictionary. Source: ProQuest. Graphic by Rob Wells",
       x="Year",
       y="Average Sentiment Score / Year")  

#9: Visualize findings, create a simple chart using ggplot that shows the top 5 counties' crime rate.   
# If you haven't installed the ggplot2 package yet, uncomment and run the line below
# install.packages("ggplot2")
library(ggplot2)
library(forcats)


#SAMPLE CODE: Loanchart<- ggplot(NationalLoans, 
#               aes(x=TotalDisbursements_Mil., y=fct_reorder(State, TotalDisbursements_Mil., desc=TRUE))) +

#Build a basic percentage change chart  
Top5Chart <- ggplot(Top5, aes(x = CrimePer10000, y=fct_reorder(city, CrimePer10000, desc=TRUE))) +
  geom_col () +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "Arkansas Cities With Top Crime Rates Per 10,000, 2017", 
       subtitle = "FBI Uniform Crime Report Data: https://ucr.fbi.gov/crime-in-the-u.s/2017/crime-in-the-u.s.-2017/tables/table-8/table-8-state-cuts/arkansas.xls",
       caption = "Graphic by Rob Wells",
       x="Crime Rate Per 10,000 People",
       y="City")

plot(Top5Chart)


library(ggplot2)
Top5Chart <- ggplot(Top5, aes(x = reorder(city, -CrimePer10000), y = CrimePer10000))  +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Arkansas Cities With Top Crime Rates Per 10,000", 
       subtitle = "FBI Uniform Crime Report Data, 2017",
       caption = "Graphic by Rob Wells",
       x="City",
       y="Crime Rate Per 10,000 People")
plot(Top5Chart)

#10: Upload the R script, .csv files and .jpg file to Blackboard.




#-----------------------------------#
# Extra touches from your colleagues
#-----------------------------------#

#Do the Katie Serrano special and add some killer colors
ColorTop5Chart <- ggplot(Top5, aes(x = reorder(city, -CrimePer10000), y = CrimePer10000, color = city, fill=city))  +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Arkansas Cities With Top Crime Rates Per 10,000", 
       subtitle = "FBI Uniform Crime Report Data, 2017",
       caption = "Graphic by Rob Wells",
       x="City",
       y="Crime Rate Per 10,000 People")
plot(ColorTop5Chart)


#Alex Nichol decimal rounding:
CrimeData$ViolentCrimePer10000<-round(CrimeData$ViolentCrimePer10000, 2)


#Mohamed and the top n function
#Create separate table with just the top five counties' crime rate: dplyr has a "top_n" function that i find handy
Top_5_county_rates <- Arkansas_Crime %>%
  select(County, violent_crime_rate_per10k) %>%
  top_n(5, violent_crime_rate_per10k) %>%
  arrange(desc(violent_crime_rate_per10k))

#Mohamed and rio for export
rio::export(Top_5_county_rates, "Top_5_violent_crimes.csv")

#Mohamed and the graphic
#Visualize findings, create a simple bar chart using ggplot that shows the top 5 counties' crime rate. 

library(ggplot2)
ggplot(Top_5_county_rates) +
  aes(reorder(County, violent_crime_rate_per10k), y = violent_crime_rate_per10k, fill = County) +
  geom_col() +
  labs(title = "Arkansas Crime Rate Per 10k",  
       subtitle = "Top 5 Counties",
       caption = "Graphic by Mohamed",
       x="County",
       y="Crime Rate")

#---------------------------------------------------------
Resources on R coding

Formatting ggplot
http://www.sthda.com/english/wiki/ggplot2-axis-ticks-a-guide-to-customize-tick-marks-and-labels

https://tutorials.quanteda.io/basic-operations/
  
  https://www.oreilly.com/library/view/practical-text-mining/9780123869791/xhtml/CHP008.html

For later
https://www3.nd.edu/~steve/computing_with_data/19_strings_and_text/strings_and_text.html#/23

More text mining
https://www.hackerearth.com/practice/machine-learning/advanced-techniques/text-mining-feature-engineering-r/tutorial/
  
  For later
https://programminghistorian.org/en/lessons/basic-text-processing-in-r

https://stackoverflow.com/questions/49173770/find-occurrences-of-huge-list-of-phrases-in-text


http://www.endmemo.com/program/R/grepl.php

Regular expressions
https://www.hackerearth.com/practice/machine-learning/advanced-techniques/regular-expressions-string-manipulation-r/tutorial/
  
  —reviewed R for Data Science
https://r4ds.had.co.nz/transform.html
to ch 5

—Sentiment analysis book
https://towardsdatascience.com/sentiment-analysis-in-r-good-vs-not-good-handling-negations-2404ec9ff2ae

—Intro to tidy text mining
https://medium.com/inside-socialcops/an-introduction-to-tidy-text-mining-49133697f61f

Tidy Data techniques
https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html