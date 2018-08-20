#############################
## Author:  Joy Vaz       ###
## Date:    15 May 2018   ###
## Purpose: Modeling      ###
#############################

library(tidyverse)
library(magrittr)
library(GGally)
library(ggplot2)
library(modelr)
library(plotly)

linGrowth_model <- function(df){
  lm(size ~ year, data = df)
}

sum_resids <- function(x){
  sum(abs(x$resid))
}

get_slope <- function(model){
  model$coefficients[2]
}

runCor <- function(df){
  suppressWarnings(cor.test(df$cases,df$prcp,method="spearman")$estimate)
}

# Task 1

ld.prism.pop <- read_csv('C:/Users/workshop/Documents/wrangling/joyldprismpop.csv')

# Task 2
ggpairs(ld.prism.pop,columns=c("prcp","avtemp","size","cases"))

# Task 3
ld.prism.pop %<>% mutate(log10size=log10(size))
ld.prism.pop %<>% mutate(log10cases=log10(cases+1))
ld.prism.pop %>% ggpairs(columns=c("prcp","avtemp","log10size","log10cases"))

# Task 4
set.seed(222); ld.prism.pop.small <- ld.prism.pop %>% sample_n(100)

myplot <- ggplot(ld.prism.pop.small, mapping=aes(prcp, avtemp)) +
  geom_point() 

myplot

# Task 5
myplot + 
  geom_smooth(method = "lm")

# Task 6
my.lm <- lm(avtemp ~ prcp, data = ld.prism.pop.small)

summary(my.lm)

# Task 7
summary(my.lm)$coefficients[2,1]
summary(my.lm)$coefficients[2,4]

# Task 8
ld.prism.pop %>% group_by(year) %>% 
  summarise(total=sum(size)) %>%
  ggplot(mapping = aes(year,total))+
  geom_point()

# Task 9
by_state <- ld.prism.pop %>% group_by(state)
by_state

# Task 10
by_state %<>% nest
by_state

# Task 11
by_state$data[[10]]

# Task 12
linGrowth_model <- function(df){
  lm(size ~ year, data = df)
}

models <- purrr::map(by_state$data, linGrowth_model)

# Task 13
by_state %<>% mutate(model = map(data, linGrowth_model))

# Task 14
by_state %<>% mutate(resids = map2(data, model, add_residuals))

by_state$resids[[1]]
# resids is a column of lists

# Task 15
sum_resids <- function(x){
  sum(abs(x$resid))
}

by_state %<>% mutate(totalResid = map(resids,sum_resids))

# Task 16
get_slope <- function(model){
  model$coefficients[2]
}

by_state %<>% mutate(slope = purrr::map(model, get_slope))

slopes <- unnest(by_state, slope)
totalResids <- unnest(by_state, totalResid)

# Task 17
slopes %>% ggplot(aes(state,slope))+geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Task 18
totalResids %>% ggplot(aes(state,totalResid))+geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Task 19
by_state2 <- ld.prism.pop %>% group_by(state)
by_state2

by_state2 %<>% nest
by_state2

#Task 20
runCor <- function(df){
  suppressWarnings(cor.test(df$cases,df$prcp,method="spearman")$estimate)
}

by_state2 %<>% mutate(spCor = purrr::map(data, runCor))
spCors <- unnest(by_state2,spCor)
spCors %<>% arrange(desc(spCor))
spCors$state <- factor(spCors$state, levels=unique(spCors$state))
ggplot(spCors,aes(state,spCor))+geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

____________________

cases_by_year <- ld %>% ungroup %>% group_by(year) %>% 
  summarise(total=sum(cases)) %>% arrange(desc(total))

cases_by_state <- ld %>% ungroup %>% group_by(state) %>% 
  summarise(total=mean(cases)) %>% arrange(desc(total))

#get map data for US counties and states
county_map <- map_data("county")
state_map <- map_data("state")
## ANNOTATE FROM HERE
# group same fips code/county together

set.seed(222); ld.prism.pop.med <- ld.prism.pop %>% sample_n(5000)

ag.fips2 <- group_by(ld.prism.pop.med,fips)
# 
ld.16y2<-summarize(ag.fips2,all.cases=sum(cases),avtemp.16y=mean(avtemp),avprcp.16y=mean(prcp),avsize.16y=mean(size))

# 
ld.16y2<-left_join(select(ld.prism.pop.med,c(state,county,fips)),ld.16y2)
ld.16y2<-distinct(ld.16y2)
ld.16y2 %<>% rename(region=state,subregion=county)
ld.16y2$subregion<-str_replace_all(ld.16y2$subregion," County","")
ld.16y2$region<-tolower(ld.16y2$region)
ld.16y2$subregion<-tolower(ld.16y2$subregion)
ld.16y2 %<>% mutate(log10cases=log10(1+all.cases))
ld.16y2 %<>% mutate(log10popsize=log10(avsize.16y))
map.ld.16y2<-left_join(county_map,ld.16y2)

map.ld.16y2 %>% plot_ly(x = ~avtemp.16y, y = ~log10popsize, type = 'scatter', size = ~all.cases, color = ~long, 
                        mode = 'markers', text = ~subregion, textposition = 'middle right',
                        marker = list(line = list(color = "grey", width = 0.5))) 
# %>%  layout(title = 'Lyme Disease 16 year data', yaxis = list(zeroline = FALSE), xaxis = list(zeroline = FALSE)
 