# load packages
library(tidyverse)
library(data.table)
library(ggsci)
library(ggridges)

library(dplyr)

# read in the pam (fvfm) data
pam <- read.csv('data/pam.csv')
# change from temp numeric to factor
pam$temp <- as.factor(pam$temp)
# change frag id from numeric to factor and reorder the frag.id values so that they group by treatment temperature in plots 
pam$frag.id <- factor(pam$frag.id, levels=c('A','B','F'('G','C','D','E','H'))

# obtain average values of fvfm for each nubbin (from the 3 technical replicates/measurements per nub) as well as the range of those values and the standard error 
fvfm<-setDT(pam)[, list(fvfm=mean(fv.fm),range=(max(fv.fm)-min(fv.fm)), se=(sd(fv.fm)/sqrt(3))), by=list(colony.id, frag.id, temp)]
fvfm$temp <- as.factor(fvfm$temp)



# plot all the data to look at the spread of fvfm values across ambient and heated treatments 
fvfm %>%
  ggplot(aes(x = temp, y = fvfm, color = temp, fill = temp))+
  geom_boxplot(size=0.8)+
  geom_jitter(position=position_jitterdodge(jitter.width = 0.5),alpha=0.5)+
  scale_color_aaas()+
  scale_fill_aaas(alpha = 0.2)+
  theme_bw()


# plot individual colonies (all 8 nubs) to see what the variation looks like (ie, are our 3 replicate measurements per nubbin giving us consistent results for that branch?)
pam %>%
  filter(colony.id == 'JH01') %>%
  ggplot(aes(x = frag.id, y = fv.fm, color = temp,fill = temp))+
  geom_point(size=0.8)
  
pam %>%
  filter(colony.id == 'JH02') %>%
  ggplot(aes(x = frag.id, y = fv.fm, color = temp,fill = temp))+
  geom_point(size=0.8)+
  geom_boxplot()+
  scale_color_aaas()+
  scale_fill_aaas(alpha = 0.2)+
  theme_bw()
  

# PAM by colony stuff <<--- this first part might not work with all the colony IDs, im not sure how to make all the levels without entering all possible colony IDs manually (or is that the solution)
pam$colony.id <- factor(pam$colony.id, levels=c('JH01','JH02','JH03','JH04','JH05','JH06','JH07','JH08','JH09','JH10','JH11','JH12','JH13','JH14','JH15','JH16','JH17','JH18','JH19','JH20','JH21','JH22','JH23','JH24','JH25','JH26','JH27','JH28','JH29','JH30','JH31','JH32','JH33','JH34','JH35','JH36'))
fvfm.avg <- setDT(pam)[, list(fvfm.avg=mean(fv.fm),range=(max(fv.fm)-min(fv.fm)),se=(sd(fv.fm)/sqrt(4))), by=list(colony.id,temp)]

pam %>%
  filter(colony.id == 'JH01') %>%
  ggplot(aes(x = temp, y = fv.fm, color = temp,fill = temp))+
  geom_point(size=0.8)+
  geom_boxplot()+
  scale_color_aaas()+
  scale_fill_aaas(alpha = 0.2)+
  theme_bw()
