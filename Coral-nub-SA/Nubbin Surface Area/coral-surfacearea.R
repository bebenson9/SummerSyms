coral_nubs <- read.csv("data/Wax.Dipping.Data.csv")

str(coral_nubs)
coral_nubs[,c("surface.area")]

surface_area <- coral_nubs$surface.area
mean(surface_area) #mean is 6.51138 (cm^2)
range(surface_area) #range is 2.559 to 11.679 (cm^2)
sd(surface_area,na.rm=FALSE) #standard deviation is 2.007561

library("dplyr")

hist(surface_area,main="Coral surface area") 

plot(surface.area~pre.mass,coral_nubs,pch=5,main="Coral Suface Area vs. Pre-Mass")

library(ggplot2)
ggplot(coral_nubs)
ggplot(coral_nubs)+
  geom_point(aes(x=pre.mass,y=surface.area,color=treatment,shape=person))+
  ggtitle("Coral surface area by pre mass")+
  xlab("Coral pre-mass (g)")+
  ylab("Coral surface area (cm^2)")

ggplot(coral_nubs)+
  geom_boxplot(aes(x=treatment,y=surface.area,fill=treatment))+
  ggtitle("Boxplot of surface area by treatment type")+
  xlab("Treatment type")+
  ylab("Nubbin surface area (cm^2)")+
  theme(panel.grid.major=element_line(linewidth=.5,color="grey"),
        axis.text.x=element_text(angle=45,hjust=1),
        axis.title=element_text(size=rel(1.5)),
        axis.text=element_text(size=rel(1.25)))


  