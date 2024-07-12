# Script for analyzing Airbrushing data
# Created 7/8/24 by GAL
# Updated 7/11/2024 by GAL

# Installing packages
library(tidyverse)
library(data.table)

####

# Correcting the data and adding a treatment variable
air.data <- read.csv('Chl/Airbrush_data.csv')
air.data$treatment <- substr(air.data$coral, 5, 5)
air.data$aliquot <- as.factor(air.data$aliquot)
air.data$treatment <- as.factor(air.data$treatment)

# Selecting the variables of interest
cell.counts <- air.data%>%
  select(coral,slurry.vol,aliquot,count1,count2,treatment)

#Arranging the cell counts into one column
??pivot_longer
cell.counts <- cell.counts%>%
  pivot_longer(
    cols = -c(coral,slurry.vol,aliquot,treatment),
    names_to = "count#",
    values_to = "cell.total"
  )

# Calculating cell density (cells per cm squared)
cell.counts <- cell.counts%>%
  mutate(cell.density = cell.total/slurry.vol)

# Determining the mean, st.dev, and st.dev/mean of the count data
cell.counts <- cell.counts%>%
  group_by(coral)%>%
  mutate(mean = mean(cell.total), sd = sd(cell.total), sd.mean = sd/mean)

#Plotting the data in a histogram
??ggplot
ggplot(cell.counts, aes(cell.total))+
  geom_histogram()+
  facet_wrap(~treatment,2,1,axes="all")

ggplot(cell.counts, aes(sd.mean))+
  geom_histogram(binwidth = 0.01)+
  facet_wrap(~treatment,2,1,axes="all")