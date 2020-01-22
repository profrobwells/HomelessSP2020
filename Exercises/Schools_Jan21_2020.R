#School homeless data, Exercise 1 
#Jan 21, 2020
#Rob Wells, University of Arkansas
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
#Question #2: Run a summary of the homeless data. 
#What is the median homeless percentage? 
#What is the average (mean)?
#What is the maximum?
#
#Answer:

#
#Question #3: Create a table with all school districts with more than 10% homeless
#HINT: SEE THE R_Intro_Jan15, the code for "Build a table with all counties above that median income"



