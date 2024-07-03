## Chlorophyll plate comparisons
## Updated 7/2/24 by BEB

# add comments to your code using pound/hashtag/number symbol!

# load packages
library(tidyverse) # tidyverse is a more expansive package that includes both dplyr and ggplot2
# note that you only need quote marks when installing packages, i.e., install.packages("")
# package installation should be done in your console, but packages required for your script should be attached at the beginning in the script file with library()

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
G.chl <- read.csv("Chl_tables.csv")

# separate wavelengths 
sixfournine <- filter(L.chl,wavelength==649)
sixsixfive <- filter(L.chl,wavelength==665)

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
