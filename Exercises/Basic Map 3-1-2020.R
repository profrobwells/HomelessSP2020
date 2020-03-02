#Basic Map
#March 1, 2019
#Advanced Reporting, School of Journalism
#Rob Wells, PhD
#
#We will create a map of homeless children by schools in Arkansas
#
#This lesson references the Machlis Ch 11 map exercise as a template. 
#It also references Andrew Ba Tran's map tutorial
https://github.com/profrobwells/HomelessSP2020/blob/master/Readings/Tran%20-%20Geolocating.pdf
#
#-------------------------------------------------------------#
#               HERE WE GO!                                  #
#-------------------------------------------------------------#
#
library(tidyverse)

#Load ChildrenAR
ChildrenAR <- rio::import("2018-2019-demographic-data-for-districts.xlsx", which = "Data", skip=8)
#
#Wells version
ChildrenAR <- rio::import("./Data/2018-2019-demographic-data-for-districts.xlsx", which = "Data", skip=8)
#
# Clean up column names to they are R friendly
ChildrenAR <- janitor::clean_names(ChildrenAR)
#
#Join ChildrenAR with addresses
#Addresses
School_Address <- rio::import("./Data/SDI Districts Printed-02-26-2020-11_38_05.xlsx", skip =1)

#convert School address LEA to numeric and match
School_Address$LEA <- as.numeric(School_Address$LEA)
ChildrenAR$LEA <- ChildrenAR$district_lea

#QUESTION: Join School_Address to ChildrenAR
#
#
#QUESTION: How many records in new combined dataframe? What is the difference?
#
#
#NEXT, we geocode the addresses using mutate_geocode
#This is a little involved... 

# ?register_google
#Google API Key https://www.youtube.com/watch?v=1JNwpp5L4vM
#https://console.cloud.google.com/apis/credentials?folder=&organizationId=&project=phonic-botany-269820
#register_google(key = "MY TOP SECRET KEY")
#schools_geocoded <- mutate_geocode(abc, Address)
#write.csv(schools_geocoded, "schools_geocoded.csv")
#
#Shortcut!!!
schools_geocoded <- rio::import("./Data/schools_geocoded.csv")
View(schools_geocoded)
#
#QUESTION: How can we simplify the school district names?
#
#Let’s pull town shapes for Arkansas with tigris.
# If you don't have tigris or ggplot2 or sf installed yet, uncomment and run the line belo
#install.packages("tigris", "sf", "ggplot2")
library(tigris) 
library(sf) 
library(ggplot2)
# set sf option
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
ar_schools <- school_districts("AR", cb=T)
#
#Basic Map of School Districts
ggplot(ar_schools) +
  geom_sf() +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) + labs(title="AR School Districts")
#
#Adding layer with homeless data
set.seed(25)
#
#QUESTION: Create a temporary table "bf" that has the district school names, percent homeless
#lon, lat and filtered for districts with more that 10% homeless

#

#Plot map with subset data
ggplot(ar_schools) +
  geom_sf() +
  geom_point(data=bf, aes(x=lon, y=lat, size=district_percent_homeless, color=district_percent_homeless), show.legend = FALSE) + 
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) + 
  labs(title="Homeless Children in Arkansas",
       subtitle = " School Districts With More Than 10% Homeless Students",
       caption = "Source: Ar Dept of Education. Graphic by Wells. 3-1-2020")
#
bf <- janitor::clean_names(bf)

#Plot with labels
library(ggrepel)
#
ggplot(ar_schools) +
  geom_sf() +
  geom_point(data=bf, aes(x=lon, y=lat, 
                          size=district_percent_homeless, color=district_percent_homeless), 
             show.legend = FALSE) + 
  geom_label_repel(data=bf, aes(x=lon, y=lat, label = Name_Cleaned), size=2.5, color="blue") +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) + 
  labs(title="Homeless Children in Arkansas",
       subtitle = " School Districts With More Than 10% Homeless Students",
       caption = "Source: Ar Dept of Education. Graphic by Wells. 3-1-2020")

#Question: Create a dataframe with the top 10 districts with the largest
#percentage increase in homelessness since 2013 and map it.


#------------------------
#Machlis - Maps in R Ch 11
library(devtools)
#Install mapping material
#install devtools if you haven't already done so
#install("devtools")

devtools::install_github('walkerke/tigris')
devtools::install_github('bhaskarvk/leaflet.extras', force = TRUE)

#install.packages("tmap")
#install.packages("tidycensus")
#install.packages("ggmap")
library(tidyverse) #loads dplyr, ggplot2, readr, tidyr,, purrr, tibble, stringr, forcats


library(leaflet)
library(glue)
library(sf)
library(tmap)
library(tmaptools)
library(tidycensus)
library(ggmap)
library(htmltools)
library(htmlwidgets)
library(ggmap)
library(tigris)

#load the map software
library(tigris)
options(tigris_use_cache = TRUE)

#create a cache directory on your machine
#Go to environment, New Folder, name is cache
#Go to Finder, select cache directory
#cntl+click on the cache folder. A menu appears
#cntl+option = copy cache directory path
#paste that path below

tigris_cache_dir("/Users/robwells/Dropbox/HomelessSP2020/cache")
readRenviron('~/.Renviron')
#Restart R
#Reload libraries
library(tidyverse) #loads dplyr, ggplot2, readr, tidyr,, purrr, tibble, stringr, forcats
library(leaflet)
library(glue)
library(sf)
library(tmap)
library(tmaptools)
library(tidycensus)
library(ggmap)
library(htmltools)
library(htmlwidgets)
library(ggmap)
library(tigris)

#For many mapping analyses, you need 
#1) a file defining a geographic area such as towns, counties, or states; 
#2) a file with data about those units, such as which towns voted for what candidates
#3) a way to merge the two, and then display the results.
