## Symbiont Analysis for Acropora Hyacinthus at Jetski Rental
# Created 7/16/2024 by GAL
# Updated 8/2/2024 by GAL

# Loading packages
library(tidyverse)
library(data.table)
library(ggplot2)
library(ggsci)
library(ggridges)
library(googlesheets4)

## Data Importing #####
# Importing the sheets
gs4_deauth()
sheet='https://docs.google.com/spreadsheets/d/1tWq6_fLJQfaoTOhGlfNUh5Slx0dwekDhXS96KD-gy5w/edit?usp=sharing'

range.airbrush='airbrush'
col_types.airbrush='cDcnnic'

range.airwax='air-wax'
col_types.airwax='cDccnnc'

range.chl='chl'
col_types.chl='cDccicnnnnc'

range.chlwax='chl-wax'
col_types.chlwax='cDccnnc'

range.stand='stand-wax'
col_types.stand='cDcccnnnnnnnc'

airbrush=read_sheet(sheet,range=range.airbrush,col_types=col_types.airbrush)
air.wax=read_sheet(sheet,range=range.airwax,col_types=col_types.airwax)
chl=read_sheet(sheet,range=range.chl,col_types=col_types.chl)
chl.wax=read_sheet(sheet,range=range.chlwax,col_types=col_types.chlwax)
stand=read_sheet(sheet,range=range.stand,col_types=col_types.stand)

## Algal Cell Density #####
# Determining the mean and st.dev of the counts and adding it the count data frame (cell.counts)
cell.counts <- airbrush%>%
  group_by(sample, slurry.vol)%>%
  summarise(count.mean = mean(count), count.sd = sd(count), sd.mean = count.sd/count.mean)

cells.slurry <- cell.counts[,c("sample","slurry.vol","count.mean")]
cells.slurry <- unique(cells.slurry)
#Averaging the number of cells in 10uL * 100 * (slurry.vol/0.01)
cells.slurry$cells.in.slurry = cells.slurry$count.mean*100*(cells.slurry$slurry.vol/.01)
cells.slurry$cells.per.ml = cells.slurry$cells.in.slurry/cells.slurry$slurry.vol

## Calculating surface area for coral nubbins for airbrushing
stand.air <- stand%>%
  select(curve,slope,intercept)%>%
  filter(curve=="1" | curve=="3" | curve=="5")
stand.air <- unique(stand.air)

air.sa <- merge(air.wax,stand.air, by="curve")
air.sa <- air.sa%>%
  mutate(wax.weight = post.mass-pre.mass)%>%
  mutate(surf.area = slope*wax.weight+intercept)

# Merging the airbrushed and air.wax coral data frames
hyacinthus.air <- merge(cells.slurry,air.sa, by="sample")
hyacinthus.air <- hyacinthus.air%>%
  select(sample,slurry.vol,cells.in.slurry,cells.per.ml,count.mean)
hyacinthus.air$cells.per.cm2 <- hyacinthus.air$cells.in.slurry/air.sa$surf.area


## Chlorophyll Concentration #####
# Removing background absorbency (manually calculated blank averages) for each plate run
chl.a1 <- chl%>%
  filter(plate=="HYACINTHUS_2_1")%>%
  mutate(e632 = e632-0.0395)%>%
  mutate(e649 = e649-0.039)%>%
  mutate(e665 = e665-0.0385)%>%
  mutate(e696 = e696-0.038)
chl.a2 <- chl%>%
  filter(plate=="HYACINTHUS_2_2")%>%
  mutate(e632 = e632-0.0505)%>%
  mutate(e649 = e649-0.0505)%>%
  mutate(e665 = e665-0.0515)%>%
  mutate(e696 = e696-0.0485)
chl.a3 <- chl%>%
  filter(plate=="HYACINTHUS_3_1")%>%
  mutate(e632 = e632-0.0375)%>%
  mutate(e649 = e649-0.037)%>%
  mutate(e665 = e665-0.037)%>%
  mutate(e696 = e696-0.037)
chl.a4 <- chl%>%
  filter(plate=="HYACINTHUS_3_2")%>%
  mutate(e632 = e632-0.0415)%>%
  mutate(e649 = e649-0.0415)%>%
  mutate(e665 = e665-0.0415)%>%
  mutate(e696 = e696-0.0395)

# Re-merging all the plate runs into one data frame
chl.all <- rbind(chl.a1, chl.a2, chl.a3, chl.a4)

# Taking the mean absorbency of the replicates
chl.absorb <- chl.all%>%
  group_by(sample)%>%
  summarise(e632=mean(e632), e649=mean(e649), e665=mean(e665), e696=mean(e696))

# Obtaining the Chlorophyll content for chl(a,b,c,d)
chl.absorb$chl.a = ((0.0604*chl.absorb$e632)+(-4.5224*chl.absorb$e649)+(13.2969*chl.absorb$e665)+(-1.7453*chl.absorb$e696))
chl.absorb$chl.b = ((-4.1982*chl.absorb$e632)+(25.7205*chl.absorb$e649)+(-7.4096*chl.absorb$e665)+(-2.7418*chl.absorb$e696))
chl.absorb$chl.c = ((28.4593*chl.absorb$e632)+(-9.9944*chl.absorb$e649)+(-1.9344*chl.absorb$e665)+(-1.8093*chl.absorb$e696))
chl.absorb$chl.d = ((-0.2007*chl.absorb$e632)+(0.0848*chl.absorb$e649)+(-0.1909*chl.absorb$e665)+(12.1302*chl.absorb$e696))
chl.absorb$chl.total = ((24.11209*chl.absorb$e632)+(11.2884*chl.absorb$e649)+(3.762*chl.absorb$e665)+(5.8338*chl.absorb$e696))

# Calculating surface area of coral nubbins for chlorophyll concentration
stand.chl <- stand%>%
  select(curve,slope,intercept)%>%
  filter(curve == "7" | curve=="10" | curve=="11")
stand.chl <- unique(stand.chl)

chl.sa <- merge(chl.wax,stand.chl, by="curve")
chl.sa <- chl.sa%>%
  mutate(wax.weight = post.mass-pre.mass)%>%
  mutate(surf.area = slope*wax.weight+intercept)

# Merging the chlorophyll(a,b,c,d) content and surface area data into one frame
hyacinthus.chl <- merge(chl.sa, chl.absorb, by="sample")
hyacinthus.chl <- hyacinthus.chl%>%
  mutate(chl.a.cm2 = chl.a/surf.area,
            chl.b.cm2 = chl.b/surf.area,
            chl.c.cm2 = chl.c/surf.area,
            chl.d.cm2 = chl.d/surf.area,
            chl.total.cm2 = chl.total/surf.area,)%>%
  select(sample, chl.a.cm2, chl.b.cm2, chl.c.cm2, chl.d.cm2, chl.total.cm2)

## Pam Data #####
pam <- read.csv("pam.csv")

# change from temp numeric to factor
pam$temp <- as.factor(pam$temp)
# change frag id from character to factor and reorder the frag.id values so that they group by treatment temperature in plots 
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

# fv/fm by site in retusa (all values, not averages)
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

# average branch fvfm by site (average of technical replicates of each branch)  
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

# average fvfm of each colony
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

# creating control and treatment data frames
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

# merging control and treatment dataframes, including difference and percent change
both.fvfm <- merge(control.fvfm, treatment.fvfm, all=TRUE)
both.fvfm$difference <- both.fvfm$treatment.fvfm-both.fvfm$control.fvfm
both.fvfm$percent.change <- (both.fvfm$difference/both.fvfm$control.fvfm)*100

# boxplot of difference in fvfm between the same colony at 29 and 36 degrees
both.fvfm %>%
  ggplot(aes(x = site, y = difference, color = site, fill = site))+
  geom_boxplot(size=0.8)+
  geom_jitter(position=position_jitterdodge(jitter.width = 0.5),alpha=0.5)+
  scale_color_aaas()+
  scale_fill_aaas(alpha = 0.2)+
  theme_bw()+
  facet_wrap(~species,2,1,axes="all")

# percent change in fvfm between the same colony at 29 and 36 degrees
both.fvfm %>%
  ggplot(aes(x = site, y = percent.change, color = site, fill = site))+
  geom_boxplot(size=0.8)+
  geom_jitter(position=position_jitterdodge(jitter.width = 0.5),alpha=0.5)+
  scale_color_aaas()+
  scale_fill_aaas(alpha = 0.2)+
  theme_bw()+
  facet_wrap(~species,2,1,axes="all")

# adding in colony sampling data
colony.sampling.data <- read.csv('pam/data/colony-sampling-data.csv')
colony.sampling.data[ , c('nth','trial','coll.date','baggie','gps.device','gps.wp','lat','long','camera','TH.imaging','img.start','img.stop','file.start','file.stop','all.images','filt.images','orientation','notes')] <- list(NULL)
colony.sampling.data$site <- str_sub(colony.sampling.data$colony.id,1,1)
colony.sampling.data$site <- as.factor(colony.sampling.data$site)
colony.sampling.data$site <- gsub("H","Hilton",colony.sampling.data$site)
colony.sampling.data$site <- gsub("J","Jet Ski",colony.sampling.data$site)

# creating dataframe with average colony fvfm and colony sampling data
sample.data.fvfm <- merge(colony.sampling.data,fvfm.avg,all=TRUE)
# creating a column of water column depth (sum of colony depth below surface and height above sea-floor)
sample.data.fvfm$water.column <- sample.data.fvfm$depth.below+sample.data.fvfm$height.above

## Data Finalization #####
# Prep for eventually merge (only need control data)
hyacinthus.air$colony.id <- substr(hyacinthus.air$sample,1,4)
hyacinthus.air$treat <- substr(hyacinthus.air$sample,5,5)
hyacinthus.air <- hyacinthus.air%>%
  filter(treat=="B")

hyacinthus.chl$colony.id <- substr(hyacinthus.chl$sample,1,4)
hyacinthus.chl$treat <- substr(hyacinthus.chl$sample,5,5)
hyacinthus.chl <- hyacinthus.chl%>%
  filter(treat=="A")

# Filtering to only get the fvfm average following treatment (36 Celsius)
post.treat.fvfm <- sample.data.fvfm%>%
  filter(temp=="36")

# Merging the key data into one overall data frame
hyacinthus.both <- merge(hyacinthus.air, hyacinthus.chl, by="colony.id")
hyacinthus.all <- merge(hyacinthus.both, post.treat.fvfm, by="colony.id")
hyacinthus.all <- hyacinthus.all%>%
  select(colony.id,fvfm.avg,cells.per.cm2,chl.a.cm2,chl.b.cm2,chl.c.cm2,chl.d.cm2,chl.total.cm2,
         species,temp,site,habitat,depth.below,height.above,water.column)%>%
  mutate(chl.per.cell = chl.total.cm2/cells.per.cm2)

# Plotting the algal symbiont efficiency graph
hyacinthus.all%>%
  ggplot(aes(x=chl.per.cell, y = fvfm.avg, color = temp, fill=temp))+
  geom_point(size=1.8)+
  ggtitle("Symbiont Efficiency")+
  ylab("Fv/Fm post bleaching")

#Plotting the algal cell density graph
hyacinthus.all%>%
  ggplot(aes(x=cells.per.cm2, y = fvfm.avg, color = temp, fill=temp))+
  geom_point(size=1.8)+
  ggtitle("Cell Density")+
  ylab("Fv/Fm post bleaching")

write.csv(hyacinthus.all,"C:/Users/gl6li/OneDrive/Documents/School & Lab Docs/EERREC Project/Hyacinthus_analysis.csv", row.names = FALSE)
