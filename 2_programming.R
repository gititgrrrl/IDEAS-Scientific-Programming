#############################
## Author:  Joy Vaz       ###
## Date:    14 May 2018   ###
## Purpose: workshop      ###
#############################

library(ggplot2)
library(magrittr)
library(plyr)
library(Rmisc)

mean.ndr.bystate <- function(df, states, years){
  dfsmall <- df[df$State%in%states&df$Year%in%years,]
    m <- mean(dfsmall$ndr)
  return(m)
}

se <- function(x){
  return(sd(x)/sqrt(length(x)))
}


setwd('C:/Users/workshop/Documents/wnv')
wnv <- read.csv('wnv.csv')

# SCRIPTS

ggplot(data = wnv) +
  geom_histogram(mapping=aes(x=Total), binwidth = 25, fill=rainbow(119)) +
  labs(x='Total number of cases for a given state in a given year', y='Frequency', 
       title='Distribution of WNV Ourbreak Sizes in Continental US States from 1999-2007',
       caption="Data from: https://diseasemaps.usgs.gov/")

ggplot(data = wnv) +
  geom_histogram(mapping=aes(x= log(Total))) +
  labs(x='Log of total number of cases for a given state in a given year', y='Frequency', 
       title='Distribution of WNV Ourbreak Sizes in Continental US States from 1999-2007',
       caption="Data from: https://diseasemaps.usgs.gov/")

#second way to log transform, add layer - scale_x_log10
ggplot(data = wnv) +
  geom_histogram(mapping = aes(x = Total)) +
  scale_x_log10() +
  labs(x='Log of total number of cases for a given state in a given year', y='Frequency', 
       title='Distribution of WNV Ourbreak Sizes in Continental US States from 1999-2007',
       caption="Data from: https://diseasemaps.usgs.gov/")

# Use arithmetic operators to calculate the raw case fatality rate (CFR) in each state in each year.

wnv$cfr <- wnv$Fatal/wnv$Total

ggplot(data = wnv) +
  geom_histogram(mapping=aes(x=wnv$cfr)) +
  labs(x='CFR for a given state in a given year', y='Frequency', 
       title='Distribution of calculated WNV Case Fatality Rate (CFR) for Continental US States from 1999-2007',
       caption="Data from: https://diseasemaps.usgs.gov/")

# Use arithmetic operators, logical operators, and the function sum to verify that the variable Total is simply the sum of the number of febrile cases, neuroinvasive cases, and other cases.

wnv$Total == wnv$EncephMen + wnv$Fever + wnv$Other

sum(wnv$Total) == sum(wnv$EncephMen, wnv$Fever, wnv$Other)

# FUNCTIONS
wnv$ndr <- wnv$EncephMen/wnv$Total


# Use your function to calculate the average severe disease rate in California, Colorado, and New York.

allstates <- unique(wnv$State)

allyears <- c(1999:2007)

mean.ndr.bystate(wnv, allstates, allyears)

mean.ndr.bystate(wnv, 'Colorado', allyears)

mean.ndr.bystate(wnv, 'New York', allyears)

mean.ndr.bystate(wnv, 'California', allyears)

mean.ndr.bystate(wnv, 'California', 2004)

# Use ggplot to show the neurovinvasive disease rate for these states as a bar graph with error bars to show the standard deviation.

wnv2 <- summarySE(wnv, measurevar="ndr", groupvars=c("State"), na.rm = TRUE)
wnv2 

mystates <- c('California', 'Colorado', 'New York')

ggplot(wnv2%>%  
         subset(State%in%mystates), mapping = aes(x=State, y=ndr)) +
  geom_bar(stat = 'identity') + 
  geom_errorbar(aes(ymin=ndr-se, ymax=ndr+se))
  
# Use your function and ggplot to show the neurovinvasive disease rate for all states.

ggplot(wnv2, mapping = aes(x=State, y=ndr)) +
  geom_bar(stat = 'identity') + 
  geom_errorbar(aes(ymin=ndr-se, ymax=ndr+se)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x='State', y='Average NDR', 
       title='Neuroinvasive Disease Rate (NDR) for Continental US States from 1999-2007',
       caption="Data from: https://diseasemaps.usgs.gov/")


# Control of flow

#conditional exclusion

wnv$EW <- ifelse(wnv$Longitude< -98.35, "WEST", "EAST")

#latitudinal gradient

ggplot(wnv, mapping = aes(x=Latitude, y=cfr)) +
  geom_point() +
  geom_smooth()

Year <- c(1999:2007)
state.number <- rep(NA, length(year))
case.total <- rep(NA, length(year))
fatal.total <- rep(NA, length(year))
cfr <- rep(NA, length(year))

for(i in c(1:length(Year))) {
  state.number[i] <- length(unique(wnv$State[wnv$Year==Year[i]]))
  case.total[i] <- sum(wnv$Total[wnv$Year==Year[i]])
  fatal.total[i] <- sum(wnv$Fatal[wnv$Year==Year[i]])
  cfr[i] <- mean(wnv$cfr[wnv$Year==Year[i]])
}

