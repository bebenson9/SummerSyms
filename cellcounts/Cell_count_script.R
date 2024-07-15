# Script for analyzing Airbrushing data
# Created 7/8/24 by GAL
# Updated 7/11/2024 by GAL

# Installing packages
library(tidyverse)
library(data.table)
library(ggsci)
library(ggridges)
####

# Correcting the data and adding a treatment variable
air.data <- read.csv("Airbrush_data.csv")
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


cells.slurry <- cell.counts[,c("coral","treatment","slurry.vol","mean")] 
cells.slurry <- unique(cells.slurry)
# estimated number of cells in the slurry 
# average number of cells in 10uL * 100 * (slurry.vol/0.01)
cells.slurry$cells.in.slurry = cells.slurry$mean*100*(cells.slurry$slurry.vol/.01)
cells.slurry$cells.per.ml = cells.slurry$cells.in.slurry/cells.slurry$slurry.vol

# surface area, import retusa wax dipping data
retusa.wax <- read.csv("retusa-wax-dipping.csv")
retusa.wax <- retusa.wax[,c("coral","surface.area")]
retusa.sa <- merge(cells.slurry,retusa.wax)

# cells per cm^2
retusa.sa$cells.per.cm2 <- retusa.sa$cells.in.slurry/retusa.sa$surface.area
retusa.sa$treatment <- gsub("D", "36", retusa.sa$treatment)
retusa.sa$treatment <- gsub("E", "36", retusa.sa$treatment)
retusa.sa$treatment <- gsub("B", "29", retusa.sa$treatment)

retusa.sa %>%
  ggplot(aes(x = treatment, y = cells.per.cm2, color = treatment,fill = treatment))+
  geom_point(size=0.8)+
  geom_boxplot()+
  scale_color_aaas()+
  scale_fill_aaas(alpha = 0.2)+
  theme_bw()

setDT(retusa.sa)[,list(mean.symbionts.cm2=mean(cells.per.cm2)), by = .(treatment)]

retusa.pam <- read.csv("pam-and-habitat.csv")
retusa.pam <- retusa.pam %>%
  filter(species=="Retusa")
colnames(retusa.sa)[colnames(retusa.sa) == "treatment"] <- "temp"
retusa.sa$colony.id <- str_sub(retusa.sa$coral,1,4)
retusa.pam.sa.cc <- merge(retusa.pam,retusa.sa,all=TRUE)

retusa.pam.sa.cc %>%
  ggplot(aes(x = habitat, y = cells.per.cm2, color = temp,fill = temp))+
  geom_point(size=0.8)+
  geom_boxplot()+
  scale_color_aaas()+
  scale_fill_aaas(alpha = 0.2)+
  theme_bw()

