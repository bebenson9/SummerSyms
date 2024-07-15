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
pam$frag.id <- factor(pam$frag.id, levels=c('A','B','F','G','C','D','E','H'))
# updated to remove the "+R" in the frag.id of some corals near the bottom of the sheet
pam$updated.frag.id <- str_sub(pam$frag.id,1,1)
pam$updated.frag.id <- factor(pam$updated.frag.id, levels=c('A','B','F','G','C','D','E','H'))

# separating colony id into site and species
pam$site <- str_sub(pam$colony.id,1,1)
pam$site <- as.factor(pam$site)
pam$site <- gsub("H","Hilton",pam$site)
pam$site <- gsub("J","Jet Ski",pam$site)
pam$species <- str_sub(pam$colony.id,2,2)
pam$species <- as.factor(pam$species)
pam$species <- gsub("H","Hyacinthus",pam$species)
pam$species <- gsub("R","Retusa",pam$species)

#fv/fm by site in hyacinthus (all values, not averages)
pam %>%
  filter(species=="Hyacinthus")%>%
  ggplot(aes(x = site, y = fv.fm, color = temp, fill = temp))+
  geom_boxplot(size=0.8)+
  geom_jitter(position=position_jitterdodge(jitter.width = 0.5),alpha=0.5)+
  ggtitle("Fv/Fm by site in Hyacinthus")+
  ylab("Fv/Fm")+
  scale_color_aaas()+
  scale_fill_aaas(alpha = 0.2)+
  theme_bw()

# obtain average values of fvfm for each nubbin (from the 3 technical replicates/measurements per nub) as well as the range of those values and the standard error 
fvfm<-setDT(pam)[, list(fvfm=mean(fv.fm),range=(max(fv.fm)-min(fv.fm)), se=(sd(fv.fm)/sqrt(3))), by=list(colony.id, updated.frag.id, temp,species,site)]
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
  ggplot(aes(x = updated.frag.id, y = fv.fm, color = temp,fill = temp))+
  geom_point(size=0.8)
  
pam %>%
  filter(colony.id == 'JH01') %>%
  ggplot(aes(x = updated.frag.id, y = fv.fm, color = temp, fill = temp))+
  geom_point(size=0.8)+
  geom_boxplot()+
  scale_color_aaas()+
  scale_fill_aaas(alpha = 0.2)+
  theme_bw()

#average branch fvfm by site (average of technical replicates of each branch)  
fvfm %>%
  ggplot(aes(x = site, y = fvfm, color = temp, fill = temp))+
  geom_boxplot(size=0.8)+
  geom_jitter(position=position_jitterdodge(jitter.width = 0.5),alpha=0.5)+
  ggtitle("Average branch fvfm by site")+
  ylab("Average branch Fv/Fm")+
  scale_color_aaas()+
  scale_fill_aaas(alpha = 0.2)+
  theme_bw()+
  facet_wrap(~species,2,1,axes="all")

# PAM by colony stuff 
pam$colony.id <- factor(pam$colony.id[1:3864])
fvfm.avg <- setDT(fvfm)[, list(fvfm.avg=mean(fvfm),range=(max(fvfm)-min(fvfm)),se=(sd(fvfm)/sqrt(4))), by=list(colony.id,temp,species,site)]

fvfm %>%
  filter(colony.id == 'JH01') %>%
  ggplot(aes(x = temp, y = fvfm, color = temp,fill = temp))+
  geom_point(size=0.8)

fvfm %>%
  filter(colony.id == 'JH01') %>%
  ggplot(aes(x = temp, y = fvfm, color = temp,fill = temp))+
  geom_point(size=0.8)+
  geom_boxplot()+
  scale_color_aaas()+
  scale_fill_aaas(alpha = 0.2)+
  theme_bw()

#average fvfm of each colony
fvfm.avg %>%
  ggplot(aes(x = site, y = fvfm.avg, color = temp, fill = temp))+
  geom_boxplot(size=0.8)+
  geom_jitter(position=position_jitterdodge(jitter.width = 0.5),alpha=0.5)+
  ggtitle("Colony fvfm average by site")+
  ylab("Average Fv/Fm")+
  scale_color_aaas()+
  scale_fill_aaas(alpha = 0.2)+
  theme_bw()+
  facet_wrap(~species,2,1,axes="all")
  
#creating control and treatment data frames
control.fvfm <- fvfm.avg%>%
  filter(temp=="29")
names(control.fvfm)[names(control.fvfm)=="fvfm.avg"] <- "control.fvfm"
control.fvfm[,temp:=NULL]
control.fvfm[,range:=NULL]
control.fvfm[,se:=NULL]
treatment.fvfm <- fvfm.avg%>%
  filter(temp=="36")
names(treatment.fvfm)[names(treatment.fvfm)=="fvfm.avg"] <- "treatment.fvfm"
treatment.fvfm[,temp:=NULL]
treatment.fvfm[,range:=NULL]
treatment.fvfm[,se:=NULL]

#merging control and treatment dataframes, including difference and percent change
both.fvfm <- merge(control.fvfm, treatment.fvfm, all=TRUE)
both.fvfm$difference <- both.fvfm$treatment.fvfm-both.fvfm$control.fvfm
both.fvfm$percent.change <- (both.fvfm$difference/both.fvfm$control.fvfm)*100

#boxplot of difference in fvfm between the same colony at 29 and 36 degrees
both.fvfm %>%
  ggplot(aes(x = site, y = difference, color = site, fill = site))+
  geom_boxplot(size=0.8)+
  geom_jitter(position=position_jitterdodge(jitter.width = 0.5),alpha=0.5)+
  scale_color_aaas()+
  scale_fill_aaas(alpha = 0.2)+
  theme_bw()+
  facet_wrap(~species,2,1,axes="all")

#percent change in fvfm between the same colony at 29 and 36 degrees
both.fvfm %>%
  ggplot(aes(x = site, y = percent.change, color = site, fill = site))+
  geom_boxplot(size=0.8)+
  geom_jitter(position=position_jitterdodge(jitter.width = 0.5),alpha=0.5)+
  scale_color_aaas()+
  scale_fill_aaas(alpha = 0.2)+
  theme_bw()+
  facet_wrap(~species,2,1,axes="all")

#adding in colony sampling data
colony.sampling.data <- read.csv('data/colony-sampling-data.csv')
colony.sampling.data[ , c('nth','trial','coll.date','baggie','gps.device','gps.wp','lat','long','camera','TH.imaging','img.start','img.stop','file.start','file.stop','all.images','filt.images','orientation','notes')] <- list(NULL)
colony.sampling.data$site <- str_sub(colony.sampling.data$colony.id,1,1)
colony.sampling.data$site <- as.factor(colony.sampling.data$site)
colony.sampling.data$site <- gsub("H","Hilton",colony.sampling.data$site)
colony.sampling.data$site <- gsub("J","Jet Ski",colony.sampling.data$site)

# creating dataframe with average colony fvfm and colony sampling data
sample.data.fvfm <- merge(colony.sampling.data,fvfm.avg,all=TRUE)
# creating a column of water column depth (sum of colony depth below surface and height above sea-floor)
sample.data.fvfm$water.column <- sample.data.fvfm$depth.below+sample.data.fvfm$height.above

sample.data.fvfm %>%
  filter(species=="Hyacinthus",temp=="36")%>%
  ggplot(aes(x = water.column, y = fvfm.avg,color=temp))+
  geom_jitter(position=position_jitterdodge(jitter.width = 0.5),alpha=0.5)+
  scale_color_aaas()+
  scale_fill_aaas(alpha = 0.2)+
  theme_bw()+
  ggtitle("Water column by average colony fv/fm in Hyacinthus at 36C")+
  xlab("Water column height")+
  ylab("Colony average fv/fm")+
  facet_wrap(~site,2,1)

# select for Hyacinthus at 36 degrees --> linear model of average colony fvfm ~ water column depth
hyacinthus.36 <- sample.data.fvfm %>%
  filter(species=="Hyacinthus",temp=="36")
summary(lm(hyacinthus.36$fvfm.avg~hyacinthus.36$water.column))

sample.data.fvfm %>%
  filter(species=="Hyacinthus",temp=="36")%>%
  ggplot(aes(x = depth.below, y = fvfm.avg,color=temp))+
  geom_jitter(position=position_jitterdodge(jitter.width = 0.5),alpha=0.5)+
  scale_color_aaas()+
  scale_fill_aaas(alpha = 0.2)+
  theme_bw()+
  ggtitle("Depth below by average colony fv/fm in Hyacinthus at 36C")+
  xlab("Colony depth below surface")+
  ylab("Colony average fv/fm")

sample.data.fvfm %>%
  filter(species=="Hyacinthus",temp=="36")%>%
  ggplot(aes(x = height.above, y = fvfm.avg,color=temp))+
  geom_jitter(position=position_jitterdodge(jitter.width = 0.5),alpha=0.5)+
  scale_color_aaas()+
  scale_fill_aaas(alpha = 0.2)+
  theme_bw()+
  ggtitle("Height by average colony fv/fm in Hyacinthus at 36C")+
  xlab("Colony height above ground")+
  ylab("Colony average fv/fm")

# cleaned up the "habitat" column (removed rows with colonies in non-discernable habitats; ex "open/crevice)
updated.habitat <- sample.data.fvfm%>% filter (habitat %in% c("open", "exposed","sheltered","overhang","crevice"))
updated.habitat %>%
  ggplot(aes(x = habitat, y = fvfm.avg, color = temp,fill = temp))+
  geom_point(size=0.8)+
  geom_boxplot()+
  scale_color_aaas()+
  scale_fill_aaas(alpha = 0.2)+
  theme_bw()

updated.habitat$species <- str_sub(updated.habitat$colony.id,2,2)
updated.habitat$species <- as.factor(updated.habitat$species)
updated.habitat$species <- gsub("H","Hyacinthus",updated.habitat$species)
updated.habitat$species <- gsub("R","Retusa",updated.habitat$species)

# only retusa fvfm by habitat
updated.habitat %>%
  filter(species=="Retusa") %>%
  ggplot(aes(x = habitat, y = fvfm.avg, color = temp,fill = temp))+
  geom_point(size=0.8)+
  geom_boxplot()+
  scale_color_aaas()+
  scale_fill_aaas(alpha = 0.2)+
  theme_bw()

write.csv(updated.habitat,"~/Downloads/pam-and-habitat.csv", row.names=FALSE)
