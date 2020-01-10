#Title: Zillow Data
#12-22-19 

#install.packages("rio") 
library(tidyverse)
library(rio)
#rio handles more than two dozen formats including tab-separated data (with the extension .tsv), 
#JSON, Stata, and fixed-width format data (.fwf).
#
#Import all US cities
Zillow <- rio::import('/Users/robwells/Dropbox/Classes/Homeless Adv Reporting Spring 2020/Zillow City_ZRI_AllhomesplusMultifamily.csv')
#
#Filter to just Arkansas
ZillowAR <- Zillow %>% filter(State == "AR")
#
View(ZillowAR)

# Number columns
ncol(ZillowAR)
# Number rows
nrow(ZillowAR)

#check data types
str(ZillowAR)
#
#Clean Headers - Janitor package
#install.packages("janitor")
library(janitor)

# Clean up column names to they are R friendly
ZillowAR <- janitor::clean_names(ZillowAR)
View(ZillowAR)
#
#Run stats
summary(ZillowAR)
#
Z_AR_PCT <- ZillowAR %>% 
  select(region_name, metro, x2010_09, x2019_11) %>% 
  mutate(Pct_Chg=((x2019_11-x2010_09)/(x2010_09))) 

#To quickly format into percents, load
#install.packages("formattable")
library(formattable)
#
Z_AR_PCT$Pct_Chg <- percent(Z_AR_PCT$Pct_Chg)
#
#Do some math - average number of white students
mean(Z_AR_PCT$x2010_09, na.rm=TRUE)
#Average value for Arkansas, Sept 2010= 770.444
mean(Z_AR_PCT$x2019_11, na.rm=TRUE)
#Average value for Arkansas, Nov. 2019= 867.8966
#
#Pct Change
(867.8966-770.444)/770.444
#Statewide increase Sept. 2010 to Nov. 2019 = 12.6%

#Graph

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = displ < 5))

Z_AR_PCT %>% 
  filter(Pct_Chg > .20) %>% 
  ggplot(aes(region_name, Pct_Chg), y=region_name, x=Pct_Chg) +
  geom_point(mapping = aes(x =Pct_Chg, y = region_name, color = Pct_Chg <.25)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(labels = scales::percent) +
  #coord_flip() +
  labs(title = "Arkansas Real Estate Values, 2010-2019",
       subtitle = "Zillow housing index",
       caption = "Graphic by Rob Wells, 12-22-19",
       y="Index, Pct Change",
       x="Year")
#
#See this webpage for great formatting tips:
#http://www.sthda.com/english/wiki/ggplot2-axis-scales-and-transformations

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

#------------------------------------
#Trend by month
#Tidy
#To create a “narrow”" tidy data frame with tidyr, use the gather() function. I think of it as gathering up all the column information that’s spread out over multiple column names but shouldn’t be, and putting them into a single new column. 
#Here’s the compact version: gather(mydf, newVariableColumnName, newValueColumnName, ColumnsYouWantToGather).
library(tidyr)
ZillowAR3 <- gather(ZillowAR2, year_mo, value, x2010_09:x2019_11)
#Clean the date
ZillowAR3$date <- gsub("x", "", ZillowAR3$year_mo)
ZillowAR3$year <- substr(ZillowAR3$date,1,nchar(ZillowAR3$date)-3)
#Extract the month - first five character
#df$text_col <- gsub("^.{0,3}", "", df$text_col)
#https://stackoverflow.com/questions/54836518/how-to-remove-the-first-three-characters-from-every-row-in-a-column-in-r
ZillowAR3$month <- gsub("^.{0,5}", "", ZillowAR3$date) 
#
#Convert to year
library(lubridate)

ZillowAR3$year1 <- as.numeric(ZillowAR3$year)


#Stacked Bar Chart
ZillowAR3 %>% 
  filter(region_name=="Fayetteville") %>% 
  ggplot(aes(x = year, y= value, fill = value)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Fayetteville Real Estate Values, 2010-2019",
       subtitle = "Zillow housing index",
       caption = "Graphic by Rob Wells, 12-22-19",
       y="Index",
       x="Fayetteville")
  
#By year, summarized
ZillowAR3 %>% 
  filter(region_name=="Fayetteville") %>% 
   group_by(year) %>% 
  summarize(year_total = mean(value, na.rm=TRUE))  %>% 
  ggplot(aes(x = year, y= year_total, fill = year_total)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Fayetteville Real Estate Values, 2010-2019",
       subtitle = "Zillow housing index",
       caption = "Graphic by Rob Wells, 12-29-19",
       y="Index",
       x="Fayetteville")
#
#Referred to https://r4ds.had.co.nz/data-visualisation.html
#
#Smoothed Line Chart
i <- ZillowAR3 %>% 
  filter(region_name=="Fayetteville") %>% 
  group_by(year) %>% 
  summarize(year_total = mean(value, na.rm=TRUE)) 
#make sure you are mapping numeric values
i$year <- as.numeric(i$year)
#
  ggplot(data = i) +
  geom_smooth(mapping = aes(x = year, y= year_total)) +
  labs(title = "Fayetteville Real Estate Values, 2010-2019",
       subtitle = "Zillow housing index",
       caption = "Graphic by Rob Wells, 12-29-19",
       y="Index",
       x="Fayetteville")
#
#Other charts
str(ZillowAR3)
ZillowAR3$year <- as.numeric(ZillowAR3$year)
#
#crude line drawing
  p <- ZillowAR3 %>% 
    group_by(region_name, year) %>% 
    summarize(year_total = mean(value, na.rm=TRUE)) 
  #
  ggplot(data = p) +
    geom_line(mapping = aes(x = year, y= year_total, fill=region_name)) +
    labs(title = "Ark Real Estate Values, 2010-2019",
         subtitle = "Zillow housing index",
         caption = "Graphic by Rob Wells, 12-29-19",
         y="Index",
         x="year")
  
#----------------------------------------------------------------------------------------#
  
  
#Import from a PDF
#The  tabulizer package is an excellent choice for extracting reasonably well structured tables from PDFs. It’s definitely worth adding to your R toolkit, with the format mydata <- extract_tables(myfile, output = "data.frame")  
#See lesson on using Tabula to extract tables from PDFs and exporting them into CSV

UofA <- rio::import('/Users/robwells/Dropbox/Classes/Homeless Adv Reporting Spring 2020/UofA Data/tabula-UA Muni Disclosure 2019.xlsx', which = "Enrollment_to_2010")
    
Fay_Zillow <- ZillowAR3 %>% 
  filter(region_name=="Fayetteville") %>% 
  group_by(year) %>% 
  summarize(year_total = mean(value, na.rm=TRUE))


#Percentage Change by Year
#
#pct change by prior year in a stack
#https://stackoverflow.com/questions/48196552/calculate-percentage-change-in-r-using-dplyr
# z %>%
#  group_by(VERTICAL) %>% 
#  arrange(YEAR, .by_group = TRUE) %>%
#  mutate(pct_change = (Profit/lag(Profit) - 1) * 100)

UofA <- UofA %>% 
  #select(Year, Total) %>% 
  arrange(Year, .by_group = TRUE) %>%
  mutate(total_enroll_pct_change = (Total/lag(Total)-1))
#
#To quickly format into percents, load
#install.packages("formattable")
library(formattable)
#
UofA$total_enroll_pct_change <- percent(UofA$total_enroll_pct_change)
#
#Rename columns
#You can do it one at a time
#Column 4 households_estimate_total renamed to household_income
#colnames(ArkCo_Income_2017)[4] <- "household_income"
colnames(UofA)[2] <- "total_enrollment"

#Repeat for Zillow Fayetteville Index
colnames(Fay_Zillow)[2] <- "Fay_Zillow_Index"
colnames(Fay_Zillow)[1] <- "Year"
Fay_Zillow <- Fay_Zillow %>% 
  arrange(year, .by_group = TRUE) %>%
  mutate(zillow_pct_change = (Fay_Zillow_Index/lag(Fay_Zillow_Index)-1))
#
Fay_Zillow$zillow_pct_change <- percent(Fay_Zillow$zillow_pct_change)
#
str(Fay_Zillow)
str(UofA)
#join dbs
#
Fay_Zillow <- Fay_Zillow %>% 
  inner_join(UofA)
write.csv(Fay_Zillow,"Fay_Zillow.csv") 


#solution: https://stackoverflow.com/questions/3099219/ggplot-with-2-y-axes-on-each-side-and-different-scales
ggplot() + 
  geom_bar(mapping = aes(x = Fay_Zillow$Year, y = Fay_Zillow$total_enroll_pct_change), stat = "identity", fill = "pink") +
  geom_line(mapping = aes(x = Fay_Zillow$Year, y = Fay_Zillow$zillow_pct_change), size = 2, color = "blue") + 
  #scale_x_date(name = "Day", labels = NULL) +
  scale_y_continuous(name = "Enroll Pct Change", 
                     sec.axis = sec_axis(~./1, name = "Zillow Pct Change")) + 
  theme(
    axis.title.y = element_text(color = "pink"),
    axis.title.y.right = element_text(color = "blue"))

#----------------------------------------------------------------------------------------#
#   NOTES BELOW THIS LINE
#----------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------#
#   NOTES BELOW THIS LINE
#----------------------------------------------------------------------------------------#
Fay_Zillow %>% 
  ggplot(aes(x = Year, y= zillow_pct_change)) +
  geom_line(show.legend = FALSE) +
  scale_y_continuous("total_enroll_pct_change"),
                     sec.axis = sec_axis("total_enroll_pct_change")) +
  
  labs(title = "Fayetteville Real Estate Values, 2010-2019",
       subtitle = "Zillow housing index",
       caption = "Graphic by Rob Wells, 12-22-19",
       y="Index",
       x="Fayetteville")

ZillowAR3 %>% 
filter(region_name=="Fayetteville") %>% 
ggplot() + 
  geom_area(mapping = aes(x = year, y = value))


  
ggplot(BroadFilter_year, aes(x = year, y = n, fill=pubtitle))  +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle=90)) +
  labs(fill = "Publication Name") +
  labs(title = "U.S.-China Broad Filter News - Articles by Year", 
       caption = "Source: ProQuest - BroadFilter Search. 2,424 articles
       Graphic by Rob Wells. 7-27-19",
       x="Year",
       y="Number of Articles") 




ggplot(x, aes(x = reorder(region_name, Pct_Chg), y = Pct_Chg, fill = Pct_Chg)) +
  geom_col(position = "dodge", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_text(aes(label = Pct_Chg), hjust = -.5, size = 2) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs(title = "Arkansas Real Estate Values, 2010-2019",
       subtitle = "Zillow housing index",
       caption = "Graphic by Rob Wells, 12-22-19",
       y="Index, Pct Change",
       x="City")

## set the levels in order we want
theTable <- within(theTable, 
                   Position <- factor(Position, 
                                      levels=names(sort(table(Position), 
                                                        decreasing=TRUE))))
## plot
ggplot(theTable,aes(x=Position))+geom_bar(binwidth=1)


x <- Z_AR_PCT %>% 
  filter(Pct_Chg > .13) %>% 
  arrange(desc(Pct_Chg))
  
  
  x %>% 
    ggplot(aes(region_name, Pct_Chg), y=region_name, x=-Pct_Chg, fill = Pct_Chg) +
    geom_col(position = "dodge") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_y_continuous(labels = scales::percent) +
    coord_flip() +
    labs(title = "Arkansas Real Estate Values, 2010-2019",
         subtitle = "Zillow housing index",
         caption = "Graphic by Rob Wells, 12-22-19",
         y="Index, Pct Change",
         x="Year")
#
  df %>%
    count(Position) %>%
    ggplot(aes(x = reorder(Position, n), y = n)) +
    geom_bar(stat = 'identity') +
    xlab("Position")
  
  ggplot(tips2, aes(x = reorder(day, -perc), y = perc)) + geom_bar(stat = "identity")
  
  ggplot(x, aes(x = reorder(region_name, -Pct_Chg), y = Pct_Chg)) + geom_bar(stat = "identity")
  
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

#Look at the table
View(ArkCo_Income_2017)

# How many rows?  
nrow(ArkCo_Income_2017)

# How many columns?
ncol(ArkCo_Income_2017)

#Install dplyr or tibble for the glimpse function if you haven't already
#library (tibble)

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

--Export data 
Write Export output this file to a CSV or Excel  write.csv or write.excel
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
--The $ is a special character  
-- earnings$TOTAL.EARNINGS <- gsub("\\$", "", earnings$TOTAL.EARNINGS) 


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





#-----------------------------#
# Notes on Ch. 4
#-----------------------------#  

Ch 4 Import Data into R
* rio
* htmltab
* readxl
* googlesheets
* pacman
* janitor
* rmiscutils (GitHub) or readr
* tibble

rio
install.packages("rio") 
--rio handles more than two dozen formats including tab-separated data (with the extension .tsv), JSON, Stata, and fixed-width format data (.fwf).
Chr to Numeric
This is one of R’s small annoyances: R generally doesn’t understand that 8,550 is a number. I dealt with this problem myself by writing my own function in my own rmiscutils package to turn all those “character strings” that are really numbers with commas back into numbers. Anyone can download the package from GitHub and use it.
Skip rows
Beginning rows that aren’t part of the data. If you know that the first few rows of an Excel spreadsheeet don’t have data you want, you can tell rio to skip one or more lines. The syntax is rio::import("mySpreadsheet.xlsx", skip=3) to exclude the first three rows. skip takes an integer.
Import and create new column names
Or, use a syntax such as rio::import("mySpreadsheet.xlsx", col_names = c("City", "State", "Population")) to set your own column names.
Import second tab of SS
If there are multiple tabs in your spreadsheet, the which argument will override the default of reading in the first worksheet. rio::import("mySpreadsheet.xlsx", which = 2) reads in the second worksheet.
Slick trick to add a column and do a conversion
It’s easy to add a column to a data frame. Currently, the Total column shows winter snowfall in inches. To add a column showing totals in Meters, you can use this format:
  snowdata$Meters <- snowdata$Total * 0.0254
4.5 Convert zip codes into usable data. Boston zip codes with leading zeroes.


