#San Francisco 311 Call for Service Data


library(tidyverse)
library(janitor)

#WARNING: This line below loads 3 million records and will take about 3 minutes to run
SF1 <- rio::import("https://data.sfgov.org/api/views/hz9m-tj6z/rows.csv?accessType=DOWNLOAD")
SF2 <- janitor::clean_names(SF1)

library(lubridate)
SF2$call_date2 <- mdy(SF2$call_date)
SF2$year <- year(SF2$call_date2)

Days <- SF2 %>% 
  count(call_date) %>% 
  group_by(call_date) %>% 
  arrange(desc(n))
  
Years <- SF2 %>% 
  count(year) %>% 
  group_by(year) %>% 
  arrange(desc(n))

# A tibble: 4 x 2
# # Groups:   year [4]
# year      n
# <dbl>  <int>
#   1  2017 841947
# 2  2018 803985
# 3  2019 771401
# 4  2016 631464

SF2019 <- SF2 %>% 
  filter(year==2019)

write.csv(SF2019,"SF2019.csv")


SF2019_NOV30 <- SF2 %>% 
  filter(call_date2=="2019-11-30")

write.csv(SF2019_NOV30,"SF2019_NOV30.csv")

  
  #Total Tweets by Day
  AOCDaytotal <- AOCdates2 %>% 
  count(date) %>% 
  group_by(date) %>% 
  arrange(desc(n))

#Calculate single busiest day
AOCdailytweets <- AOCdates2 %>% 
  count(date2) %>% 
  group_by(date2) %>% 
  arrange(desc(n))


#Total Tweets by Month
AOCmonth <- AOCdates2 %>% 
  count(month) %>% 
  group_by(month) %>% 
  arrange(desc(n))




#This worked - returned a huge list
#install.packages("OData")
library(OData)
SF <- retrieveData("https://data.sfgov.org/resource/hz9m-tj6z.json") 

SF <- downloadResourceCsv("https://data.sfgov.org/resource/hz9m-tj6z.json") 

SF1 <- downloadResourceCsv("https://data.sfgov.org/api/odata/v4/hz9m-tj6z", ",")



#df <- data.frame(matrix(unlist(l), nrow=14, byrow=T),stringsAsFactors=FALSE)

SF1 <- data.frame(matrix(unlist(SF), nrow=1000, byrow=F),stringsAsFactors=FALSE)
SF1 <- as.data.frame(SF)


SF <- rio::import("./Data/Fayettevillle Police 2020.xlsx", which = "Jan 2020", skip=11)

Cops <- janitor::clean_names(Cops)
colnames(Cops)

Busted <- Cops %>% 
  select(date_time_of_call, description) %>% 
  group_by(description) %>% 
  count(description) %>% 
  arrange(desc(n)) %>% 
  ungroup()
