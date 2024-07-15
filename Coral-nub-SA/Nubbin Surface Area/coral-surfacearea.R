coral_nubs <- read.csv("data/Wax-Dipping-Data.csv")
coral_nubs <- coral_nubs[-c(289),]

str(coral_nubs)
coral_nubs[,c("surface.area")]

surface_area <- coral_nubs$surface.area
mean(surface_area) #mean is 6.867303 (cm^2)
range(surface_area) #range is 2.5098 to 15.8084 (cm^2)
sd(surface_area,na.rm=FALSE) #standard deviation is 2.396137

library(tidyverse)
library(data.table)
library(ggsci)
library(ggridges)
library("dplyr")

coral_nubs$species <- str_sub(coral_nubs$coral,2,2)
coral_nubs$species <- gsub("H","Hyacinthus",coral_nubs$species)
coral_nubs$species <- gsub("R","Retusa",coral_nubs$species)


hist(surface_area,main="Coral surface area") 

plot(surface.area~pre.mass,coral_nubs,pch=5,main="Coral Suface Area vs. Pre-Mass")

coral_nubs$temp <- as.factor(coral_nubs$temp)
coral_nubs$species <- as.factor(coral_nubs$species)

library(ggplot2)
ggplot(coral_nubs)
ggplot(coral_nubs)+
  geom_point(aes(x=pre.mass,y=surface.area,color=temp,shape=person))+
  ggtitle("Coral surface area by pre mass")+
  xlab("Coral pre-mass (g)")+
  ylab("Coral surface area (cm^2)")


coral_nubs%>%
  filter(species=="Hyacinthus")%>%
  ggplot(aes(x=temp,y=surface.area,fill=temp))+
  geom_boxplot(size=0.8)+
  ggtitle("Boxplot of surface area by temperature in Hyacinthus")+
  xlab("Temperature")+
  ylab("Nubbin surface area (cm^2)")+
  theme(panel.grid.major=element_line(linewidth=.5,color="grey"),
        axis.text.x=element_text(angle=45,hjust=1),
        axis.title=element_text(size=rel(1.5)),
        axis.text=element_text(size=rel(1.25)))

bp <- ggplot(coral_nubs,aes(x=species,y=surface.area,fill=temp))+
  geom_boxplot()+
  ggtitle("Boxplot of surface area by species")+
  xlab("Species")+
  ylab("Nubbin surface area (cm^2)")+
  theme(panel.grid.major=element_line(linewidth=.5,color="grey"),
        axis.text.x=element_text(angle=0,hjust=1),
        axis.title=element_text(size=rel(1.5)),
        axis.text=element_text(size=rel(1.25)))
  
bp + scale_fill_manual(breaks=c("29","36"),
                       values=c("turquoise",""))
  
  