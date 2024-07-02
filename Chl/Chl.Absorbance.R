chl_abs <- read.csv("Data/chl.absorbance.csv")
str(chl_abs)
library("dplyr")
library(ggplot2)

sixfournine <- filter(chl_abs,wavelength==649)
sixsixfive <- filter(chl_abs,wavelength==665)

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
