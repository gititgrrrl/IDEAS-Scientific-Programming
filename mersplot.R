#############################
## Author:  Joy Vaz       ###
## Date:    15 May 2018   ###
## Purpose: ProjMgmt      ###
#############################


library(ggplot2)
library(lubridate)

mers <- read.csv('C:/Users/workshop/Documents/project managment/IDEASprojectmanagment/cases.csv')

mers$hospitalized[890] <- c('2015-02-20') # change the date on row 890
mers <- mers[-471,] # omit row 471 from the dataset (bc it has conflicting dates?)

#use lubridate to change the columns containing dates from "Factor" to "Dates"

mers$onset2 <- ymd(mers$onset) # create new sister columns for 'onset' and 'hospitalised' columns
mers$hospitalized2 <- ymd(mers$hospitalized) # and use 'ymd' function to turn them into dates
class(mers$onset2) #check the class of these new colums to see if they have changed from "Factor" to "Date"
class(mers$hospitalized2)

day0 <- min(na.omit(mers$onset2)) 
#use na.omit because if there are NAs R will be confused about what the minimum is

mers$epi.day <- as.numeric(mers$onset2 - day0) #creating a new numeric value for the epidemic day for each case


#making plots

library(ggplot2)
ggplot(data = mers) +
  geom_bar(mapping=aes(x=epi.day, fill=country), na.rm = TRUE) +
  labs(x='Epidemic day', y='Case count', title= 'Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
