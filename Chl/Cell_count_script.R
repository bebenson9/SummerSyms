# Script for analyzing Airbrushing data
# Created 7/8/24 by GAL

# Installing packages
library(tidyverse)
library(data.table)

####

# Correcting the data
air.data <- read.csv('Chl/Airbrush_data.csv')
str(Air.data)
air.data$aliquot <- as.factor(air.data$aliquot)
air.data$treatment <- substr(air.data$coral, 5, 5)
??rename
air.data %>%
  vars(B)
  rename_with(control)
  


# Selecting the variables of interest
cell.counts <- air.data%>%
  select(coral,slurry.vol,aliquot,count1,count2)

#Arranging the cell counts into one column
??pivot_longer
cell.counts <- cell.counts%>%
  pivot_longer(
    cols = -c(coral,slurry.vol,aliquot),
    names_to = "count#",
    values_to = "cell.total"
  )

# Determining the mean, st.dev, and st.dev/mean of the count data
cell.counts%>%
  group_by(coral)%>%
  mutate(mean = mean(cell.total), sd = sd(cell.total), sd.mean = sd/mean)

#Plotting the data in a histogram
??hist
hist(cell.counts$cell.total)
