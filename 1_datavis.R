### Author: Joy Vaz             ###
### Date: May 14, 2018          ###
### Class: IDEAS workshop       ###
### Topic: Data Visualization   ###
### Data: MERS                  ###



# Set working directory as the folder where the MERS case data is saved.
setwd('C:/Users/workshop/Documents/mers') 

# Load data from the .csv file into R as a dataframe and assign it the name 'mers'
mers <- read.csv('cases.csv')

# Inspect the data and see that some variables, such as onset and hospitalized are dates, but are formatted as "factor"
head(mers)

# Fix some mistakes in the data
mers$hospitalized[890] <- c('2015-02-20') # change the date on row 890
mers <- mers[-471] # omit row 471 from the dataset
### WAS TRYING TO REMOVE COUNTRY-LESS LINES FROM DATASET- NEED NEW METHOD. 


# Show that the onset column containing dates is formatted as "factor"
class(mers$onset)  

# Use the 'lubridate' package to change the formatting of columns containing dates from "factor" to "Date"

library(lubridate)

# Create new sister columns for 'onset' and 'hospitalised' columns and use 'ymd' function to format them as "Date"
mers$onset2 <- ymd(mers$onset) 
mers$hospitalized2 <- ymd(mers$hospitalized)

# Show that the new columns are formatted as "Date"
class(mers$onset2) 
class(mers$hospitalized2)

# Assign the earliest onset date to 'day0' by finding the minimun vale in the onset2 column
day0 <- min(na.omit(mers$onset2)) 
# Question: Why do we use the function na.omit? What happens if we neglect this command?
# Answer: We use na.omit because if there are NAs R will unable to identify the minimum value (ie. earliest onset date).

# Create a new, numeric value for the epidemic day for each case by calculating its difference from 'day0' (2012-03-21)
mers$epi.day <- as.numeric(mers$onset2 - day0) 
# Question: What purpose does the command as.numeric serve?
# Answer: The as.numeric command treats the values in the onset2 and day0 as "numeric" instead of "Date" so that it can subtract them to give a numeric answer

# Making a plot

# Load package
library(ggplot2)

# Produce an epidemic curve by adding a bar plot using the function geom_bar
ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day)) + 
  labs(x='Epidemic day', y='Case count', title= 'Global count of MERS cases by date of symptom onset', # add some axis labels and a title
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
# Question: We end each line with the addition syhmbol "+". What happens if we don't use this convention?
# Answer: Neglecting to add the "+" at the end on each line will result in subquent layers being ommitted from the plot.

# Add the aesthetic 'fill' to color case count by country
ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day, fill=country)) +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

# Modify the plot using the argument position="fill" so that each bar stack is of a standardized height, and the y-axis changes to proportions instead of counts
ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day, fill=country), position="fill") + 
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

# Modify the plot to change the coordinates by adding coord_flip() which flips the x and y axes.
ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day, fill=country)) + 
  coord_flip() +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

# Modify the plot to change the coordinates by adding coord_flip() which uses the polar coordinate system to plot the epidemic data.
ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day, fill=country)) + 
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv") + coord_polar()


# Univariate plots

# Calculate the "raw" infectious period as the difference between the date of hospitalization and the date of onset
mers$infectious.period <- mers$hospitalized2-mers$onset2

# Show that the data are formatted as "difftime"
class(mers$infectious.period)

# Format the infectious period column as "numeric" in units of days instead of "difftime"
mers$infectious.period <- as.numeric(mers$infectious.period, units = "days")

# Plot a histogram of to show the distribution of infectious periods
ggplot(data=mers) +
  geom_histogram(aes(x=infectious.period), na.rm = TRUE, binwidth = 5) +
  labs(x='Infectious period', y='Frequency', 
        title='Distribution of calculated MERS infectious period',
        caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

# Problem: there are negative infectious periods because of nosocomical infections where the date of hospitalisation is earlier than the date of onset

# Calculate a new value which is the calculated infectious period in the case where it is positive and zero otherwise
mers$infectious.period2 <- ifelse(mers$infectious.period<0, 0, mers$infectious.period)

ggplot(data=mers) +
  geom_histogram(aes(x=infectious.period2), na.rm = TRUE, binwidth = 5) +
  labs(x='Infectious period', y='Frequency', 
       title='Distribution of calculated MERS infectious period (positive values only)',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

# Investigate and plot the frequency of hospital-acquired infections of MERS as a density plot
ggplot(data=mers) +
  geom_density(mapping=aes(x=infectious.period2), na.rm = TRUE) +
  labs(x='Infectious period', y='Frequency',
       title='Probability density for MERS infectious period (positive values only)', 
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

# Investigate and plot the frequency of hospital-acquired infections of MERS as an area plot
ggplot(data=mers) +
  geom_area(stat = 'bin', mapping=aes(x=infectious.period2), na.rm = TRUE, binwidth =5) +
  labs(x='Infectious period', y='Frequency',
       title='Area plot for MERS infectious period (positive values only)',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

# Experiment with other univariate plot types like geom_dotplot and geom_bar
ggplot(data=mers) + 
  geom_dotplot(mapping=aes(x=infectious.period2)) + 
  labs(x='Infectious period', y='Frequency', 
       title='Dot plot for MERS infectious period (positive values only)',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

ggplot(data=mers) + 
  geom_bar(mapping=aes(x=infectious.period2)) + 
  labs(x='Infectious period', y='Frequency', 
       title='Bar plot for MERS infectious period (positive values only)',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

# Bivariate plots

# Use our corrected infectious period variable (infectious.period2) to study the change in the infectious period over the course of the MERS epidemic.
ggplot(data=mers) +
  geom_point(mapping = aes(x=epi.day, y=infectious.period2, color=country)) +
  scale_y_continuous(limits = c(0, 40)) +
  labs(x='Epidemic day', y='Infectious period',
       title='MERS infectious period over time (positive values only)', 
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

# Add a curve fit using geom_smooth
ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2)) + 
  geom_point(mapping = aes(color=country)) +
  scale_y_continuous(limits = c(0, 40)) +
  geom_smooth(method="loess") +
  labs(x='Time', y='Infectious period', 
       title='MERS infectious period over time (positive values only)',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
# There does not appear to be evidence for societal learning in this data when cases from all countries are lumped toghether.

# Add a separate smooth fit for each country
ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2)) + 
  geom_point() +
  scale_y_continuous(limits = c(0, 40)) +
  geom_smooth(method="loess", mapping = aes(color=country)) +
  labs(x='Time', y='Infectious period', 
       title='MERS infectious period over time (positive values only)',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
# There does appear to be evidence for societal learning in this data for several individual countries, most notably Oman, Qatar, and UAE.

# Faceting

# Facet by country
ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2)) +
  geom_point(mapping = aes(color=country)) +
  facet_wrap(~ country) +
  scale_y_continuous(limits = c(0, 40)) +
  labs(x='Epidemic day', y='Infectious period',
       title='MERS infectious period over time (positive values only)', 
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

# Facet by country and gender
ggplot(data=subset(mers, gender %in% c('M', 'F') & country %in% c('KSA', 'Oman', 'Iran', 'Jordan', 'Qatar', 'South Korea','UAE')), mapping=aes(x=epi.day, y=infectious.period2)) +
  geom_point(mapping = aes(color=country)) + 
  facet_grid(gender ~ country) +
  scale_y_continuous(limits = c(0, 40)) +
  labs(x='Epidemic day', y='Infectious period',
       title='MERS infectious period by gender and country', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

# Create an interactive epidemic curve plot
library(plotly)
epi.curve <- ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day)) +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
ggplotly(epi.curve)
