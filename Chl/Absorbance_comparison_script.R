# Chlorophyll Plate Comparisons 2
# Created 7/15/24 by GAL 

# Loading packages
library(tidyverse)
library(data.table)

# Importing and correcting the data
chl.abs <- read.csv("Chl/Data/chl_abs_data.csv")
str(chl.abs)
chl.abs$replicate <- as.factor(chl.abs$replicate)
chl.abs$column <- as.factor(chl.abs$column)
chl.abs$wavelength <- as.factor(chl.abs$wavelength)

# Creating separate data frames for first plate reading and the second, selecting for only relevant columns
# Also averaging the replicates for both plates
plate1 <- chl.abs%>%
  filter(plate == "RETUSA1_1")%>%
  select(sample, replicate, wavelength, absorbance)
plate1.abs <- setDT(plate1)[, list(plate1.abs=mean(absorbance)), by=list(sample,wavelength)]
plate2 <- chl.abs%>%
  filter(plate == "RETUSA1_2")%>%
  select(sample,replicate,wavelength,absorbance)
plate2.abs <- setDT(plate2)[, list(plate2.abs=mean(absorbance)), by=list(sample,wavelength)]

# Merging the two plates into one data frame
plate.both <- merge(plate1.abs, plate2.abs)

# Graphing to make the comparison
plate.both%>%
  ggplot(aes(x=plate1.abs, y=plate2.abs))+
  geom_point()+
  facet_wrap(~wavelength,4,1,axes="all")
