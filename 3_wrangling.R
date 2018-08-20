#############################
## Author:  Joy Vaz       ###
## Date:    15 May 2018   ###
## Purpose: Wrangling     ###
#############################

library(tidyverse)
library(magrittr)
library(dplyr)
library(stringr)
library(GGally)
library(maptools)
library(ggmap)
library(maps)
library(ggplot2)

makeitthree <- function(y, x){
  a <- ifelse(str_length(x)==3, x, paste("", x, sep = "0"))
  b <- ifelse(str_length(a)==3, a, paste("", a, sep = "0"))
  z <- paste(y, b, sep = "")
  return(as.integer(z))
}

ld <- read.csv('C:/Users/workshop/Documents/wrangling/lyme.csv')
pop <- read.csv('C:/Users/workshop/Documents/wrangling/pop.csv')
prism <- read.csv('C:/Users/workshop/Documents/wrangling/climate.csv')

# select all pop data from the years 2000-2009 and corresponding fips
pop %<>% select(fips,starts_with("pop2"))
# take all pop colums and collapse them into key-value pairs of unique popYear-popsize pairs
pop %<>% gather(starts_with("pop2"),key="str_year",value="size") %>% na.omit
# add column named 'year' that takes the 4 digit suffix following 'pop' and makes it year
pop %<>% mutate(year=str_replace_all(str_year,"pop",""))
# change class of the values in the year column from character to integer
pop %<>% mutate(year=as.integer(year))
#not sure what this does:
pop %<>% mutate(fips=str_replace_all(fips,"^0",""))
# change class of the values in the fips column from character back to integer
pop %<>% mutate(fips=as.integer(fips))

ld %<>% gather(starts_with("Cases"),key="str_year",value="cases")
ld %<>% mutate(year=str_replace_all(str_year,"Cases",""))
ld %<>% mutate(year=as.integer(year))
ld %<>% rename(state=STNAME,county=CTYNAME)

ld$fips <- makeitthree(ld$STCODE, ld$CTYCODE)
ld$fips
ld %<>% select(-c(STCODE,CTYCODE,str_year))

# check known location to verify that fips column worked
AthensFIPS <- unique(ld$fips[ld$state=="Georgia"&ld$county=="Clarke County"])
length(AthensFIPS) == 1
AthensFIPS == 13059

ld.prism<- inner_join(ld,prism)
ld.prism.pop <- inner_join(ld.prism,pop)

save(ld.prism, file = 'C:/Users/workshop/Documents/wrangling/joyldprism.Rdata')
write_csv(ld.prism, path = 'C:/Users/workshop/Documents/wrangling/joyldprism.csv')

save(ld.prism.pop, file = 'C:/Users/workshop/Documents/wrangling/joyldprismpop.Rdata')
write_csv(ld.prism.pop, path = 'C:/Users/workshop/Documents/wrangling/joyldprismpop.csv')


cases_by_year <- ld %>% ungroup %>% group_by(year) %>% 
  summarise(total=sum(cases)) %>% arrange(desc(total))

cases_by_state <- ld %>% ungroup %>% group_by(state) %>% 
  summarise(total=mean(cases)) %>% arrange(desc(total))

#get map data for US counties and states
county_map <- map_data("county")
state_map <- map_data("state")
## ANNOTATE FROM HERE
# 
ag.fips <- group_by(ld.prism.pop,fips)
# 
ld.16y<-summarize(ag.fips,all.cases=sum(cases))
# 
ld.16y<-left_join(select(ld.prism.pop,c(state,county,fips)),ld.16y)
ld.16y<-distinct(ld.16y)
ld.16y %<>% rename(region=state,subregion=county)
ld.16y$subregion<-str_replace_all(ld.16y$subregion," County","")
ld.16y$region<-tolower(ld.16y$region)
ld.16y$subregion<-tolower(ld.16y$subregion)
ld.16y %<>% mutate(log10cases=log10(1+all.cases))
map.ld.16y<-left_join(county_map,ld.16y)
p <- ggplot2::ggplot(map.ld.16y)+geom_point(aes(long,lat,color=log10cases),size=0.2) +
  scale_colour_gradientn(colours=rev(rainbow(4)))

p

ggplotly(p)

