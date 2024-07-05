## Chlorophyll plate comparisons
## Updated 7/2/24 by BEB

# add comments to your code using pound/hashtag/number symbol!

# load packages
library(tidyverse) # tidyverse is a more expansive package that includes both dplyr and ggplot2
# note that you only need quote marks when installing packages, i.e., install.packages("")
# package installation should be done in your console, but packages required for your script should be attached at the beginning in the script file with library()

library(data.table)

# read in the data 
L.chl <- read.csv("Data/chl-absorbance.csv") # I think I told yall I use . to avoid spaces and make things R friendly, and that's true, but for file names I actually tend to use - between words because it's easier to read and you can more readily identify the extension (e.g., chl-absorbance.csv). This gets more important as files become more numerous and complex, but much of this is still just personal preference
# I renamed the above object to be a shorter name and to start with L so we know it's Lainie's two wavelengths 

# R has several types of objects and str() is telling us how the data in this spreadsheet is being interpreted by R
str(L.chl)
  #'data.frame':	148 obs. of  9 variables:
  #  $ person    : chr  "Lainie" "Lainie" "Lainie" "Lainie" ...
  #$ date      : chr  "6/26/24" "6/26/24" "6/26/24" "6/26/24" ...
  #$ plate     : int  1 1 1 1 1 1 1 1 1 1 ...
  #$ wavelength: int  649 649 649 649 649 649 649 649 649 649 ...
  #$ coral     : chr  "blank" "JH01A" "JH01C" "JH02A" ...
  #$ well      : chr  "A1" "B1" "C1" "D1" ...
  #$ replicate : int  1 1 1 1 1 1 1 1 1 1 ...
  #$ aborbance : num  0.119 0.238 0.15 0.226 0.142 0.175 0.152 0.248 0.175 0.203 ...
  #$ notes     : logi  NA NA NA NA NA NA ...

# Look's like we have data of types character (chr), integer (int), numeric (num), and logical (logi). Do all of these seem correct? Hint: no. We want to reclassify at least a couple of them. Hint 2: there are at least a couple variables that should be factors. Can you figure out which ones and how to change the data type for those? 


# read in Gil's data before proceeding!! 
## READ IN HERE 
# str(gil's data) to check how it compares to Lainie's above; update any data types as needed

# then merge the data sets 
# try the merge() function. type ?merge in the console for information about the function and what goes into it (i.e., the 'arguments')
# Note - I noticed a typo in the absorbance column header in Lainie's data sheet; you will need to correct that before attempting to merge the data sets
G.chl <- read.csv("Data/Chl_tables.csv")

Chl.data <- rbind(L.chl,G.chl)
Chl.data$plate <- as.factor(Chl.data$plate)
Chl.data$wavelength <- as.factor(Chl.data$wavelength)
Chl.data$replicate <- as.factor(Chl.data$replicate)
str(Chl.data)

# separate wavelengths 
sixfournine <- filter(L.chl,wavelength==649)
sixsixfive <- filter(L.chl,wavelength==665)
sixthreetwo <- filter(Chl.data,wavelength==632)
sixninesix <- filter(Chl.data,wavelength==696)

# Lainie, nice job figuring out how to plot this! However, it's a little hard to interpret at a glance
# part of the reason it looks a little odd is that plate is being interpreted as a continuous numeric variable, when in reality, it's a factor. There is nothing intrinsically numeric about these plates. We could have called them plate A and plate B. What does it mean for something to be a factor? If you haven't already made plate a factor after looking at str(), go ahead and do so 
ggplot(sixfournine)+
  geom_point(aes(x=well,y=aborbance,color=plate))+
  theme(panel.grid.major=element_line(linewidth=.5,color="grey"),
        axis.text.x=element_text(angle=90,hjust=1),
        axis.title=element_text(size=rel(1.5)),
        axis.text=element_text(size=rel(1.25)))

ggplot(sixsixfive)+
  geom_point(aes(x=well,y=aborbance,color=plate))+
  theme(panel.grid.major=element_line(linewidth=.5,color="grey"),
        axis.text.x=element_text(angle=90,hjust=1),
        axis.title=element_text(size=rel(1.5)),
        axis.text=element_text(size=rel(1.25)))

# let's take a step back before we plot any further and think about exactly what we want to see
# the question we want to answer right now = how do the values from plate 1 compare to the values we got for the same exact corals/subsamples when we re-ran the same plate a little later? 
# we want plate 1 on the x-axis and plate 2 (re-do) on the y axis, for each wavelength
  # if the plate reads were consistent, we would get a 1 to 1 line 
# how can we get the data in the format we want to test for this?
# well, first we need to average our two replicates for each fragment to obtain an average absorbance value for each coral fragment in the plate
# try setDT (and make sure you add library(data.table) at the top of this script and run that line of code to use the setDT function!)
# what's next? 
Chl.absorbance <- setDT(Chl.data)[, list(Chl.absorbance=mean(absorbance)), by=list(coral,plate,wavelength)]

Chl.absorbance%>%
  ggplot(aes(x=Chl.absorbance(filter(plate=="1")),y=Chl.absorbance(filter(plate=="2'"))))+
  geom_point()

original <- Chl.absorbance%>%
  filter(plate=="1")
redo <- Chl.absorbance%>%
  filter(plate=="2")

plot(original$Chl.absorbance~redo$Chl.absorbance)


# facet_wrap(~wavelength,4,1)


# plate1.649$w649 <- plate1.649$Chl.absorbance
plate1.649 <- Chl.absorbance %>%
  filter(plate == 1) %>%
  filter(wavelength == 649)
ch1.649 <- rename(plate1.649, P1A=Chl.absorbance)

plate1.665 <- Chl.absorbance %>%
  filter(plate == 1) %>%
  filter(wavelength == 665)
ch1.665 <- rename(plate1.665, P1A=Chl.absorbance)

plate1.632 <- Chl.absorbance %>%
  filter(plate == 1) %>%
  filter(wavelength == 632)
ch1.632 <- rename(plate1.632, P1A=Chl.absorbance)

plate1.696 <- Chl.absorbance %>%
  filter(plate == 1) %>%
  filter(wavelength == 696)
ch1.696 <- rename(plate1.696, P1A=Chl.absorbance)

plate1 <- rbind(ch1.649,ch1.665,ch1.632,ch1.696)
plate1[,plate:=NULL]

# plate 2
plate2.649 <- Chl.absorbance %>%
  filter(plate == 2) %>%
  filter(wavelength == 649)
ch2.649 <- rename(plate2.649, P2A=Chl.absorbance)

plate2.665 <- Chl.absorbance %>%
  filter(plate == 2) %>%
  filter(wavelength == 665)
ch2.665 <- rename(plate2.665, P2A=Chl.absorbance)

plate2.632 <- Chl.absorbance %>%
  filter(plate == 2) %>%
  filter(wavelength == 632)
ch2.632 <- rename(plate2.632, P2A=Chl.absorbance)

plate2.696 <- Chl.absorbance %>%
  filter(plate == 2) %>%
  filter(wavelength == 696)
ch2.696 <- rename(plate2.696, P2A=Chl.absorbance)

plate2 <- rbind(ch2.649,ch2.665,ch2.632,ch2.696)
plate2[,plate:=NULL]


 # script to make 4 plots by wavelength
chl <- merge(plate1,plate2,all=TRUE)
# facet_wrap(~wavelength,4,1)
ggplot(chl,aes(P1A,P2A))+
  geom_point()+
  facet_wrap(~wavelength,4,1,axes="all")

