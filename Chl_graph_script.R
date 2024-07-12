library(tidyverse)
library(data.table)
install.packages("dplyr")
# Openning my and"dplyr"# Openning my and lainie's data tables in R
read.csv("Chl_tables.csv")
G.Chl <- read.csv("Chl_tables.csv")
str(G.Chl)
read.csv('Chl/Data/chl-absorbance.csv')
L.Chl <- read.csv('Chl/Data/chl-absorbance.csv') # Remember to include 'read.csv' to avoid just making an object
str(L.Chl)

# Merging the two tables after correcting column typo in excel
?merge
merge(G.Chl, L.Chl)
Chl.data <-rbind(G.Chl, L.Chl)
?rbind # Tried to use 'merge' but used 'rbind' instead
Chl.data
str(Chl.data)

# Changing some of the data types in the master table to be factors, specifcally categorical columns
?as.factor
?matrix
?data.frame
class(Chl.data$wavelength) # Figured out how to access columns in a matrix/data frame with ("matrix"$"column")
as.factor(Chl.data$wavelength)
as.factor(Chl.data$plate)
as.factor(Chl.data$replicate)

#Separating the individual wavelengths to see difference in plots
?filter
sixfournine <- filter(Chl.data,wavelength==649)
sixsixfive <- filter(Chl.data,wavelength==665)
sixthreetwo <- filter(Chl.data,wavelength==632)
sixninesix <- filter(Chl.data,wavelength==696)

# Plotting the wavelengths into a graph
ggplot(sixfournine)+
  geom_point(aes(x=well,y=absorbance,color=plate))+
  theme(panel.grid.major=element_line(linewidth=.5,color="grey"),
        axis.text.x=element_text(angle=90,hjust=1),
        axis.title=element_text(size=rel(1.5)),
        axis.text=element_text(size=rel(1.25)))
?ggplot
?aes
?geom_point
?element_text
?setDT # Figuring out how to average the replicates
Chl.absorbance <- setDT(Chl.data)[, list(Chl.absorbance=mean(absorbance)), by=list(coral,plate,wavelength)]
# Replotting the graph with the averaged absorbance values
sixfournine <- filter(Chl.absorbance,wavelength==649) #Refiltering and relabeling using new averaged data table
ggplot(sixfournine)+
  geom_point(aes(x=plate ,y=plate,color=plate))+
  theme(panel.grid.major=element_line(linewidth=.5,color="grey"),
        axis.text.x=element_text(angle=90,hjust=1),
        axis.title=element_text(size=rel(1.5)),
        axis.text=element_text(size=rel(1.25)))
?theme
Chl.absorbance$plate <- as.factor(Chl.absorbance$plate)

####

#Filtering to only have tables with plate and absorbance
P1.632 <- Chl.absorbance %>% filter(plate==1) %>% filter(wavelength==632)
P1.649 <- Chl.absorbance %>% filter(plate==1) %>% filter(wavelength==649)
P1.665 <- Chl.absorbance %>% filter(plate==1) %>% filter(wavelength==665)
P1.696 <- Chl.absorbance %>% filter(plate==1) %>% filter(wavelength==696)

P2.632 <- Chl.absorbance %>% filter(plate==2) %>% filter(wavelength==632)
P2.649 <- Chl.absorbance %>% filter(plate==2) %>% filter(wavelength==649)
P2.665 <- Chl.absorbance %>% filter(plate==2) %>% filter(wavelength==665)
P2.696 <- Chl.absorbance %>% filter(plate==2) %>% filter(wavelength==696)

#Making two tables for each plate
Plate1 <- rbind(P1.632,P1.649,P1.665,P1.696)
Plate1 <- rename(Plate1, P1A=Chl.absorbance)
Plate1[,plate:=NULL]

Plate2 <- rbind(P2.632,P2.649,P2.665,P2.696)
Plate2 <- rename(Plate2, P2A=Chl.absorbance)
Plate2[,plate:=NULL]

#Making the graphs by using facet wrapping
Chl.graph <- merge(Plate1,Plate2, all=TRUE)
ggplot(Chl.graph, aes(P1A,P2A))+
  geom_point()+
  facet_wrap(~wavelength,4,1,axes="all")
