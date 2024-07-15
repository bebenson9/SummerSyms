library(tidyverse)
library(data.table)
library(ggsci)
library(ggridges)

retusa.chl <- read.csv("retusa-chl.csv")
retusa.chl <- retusa.chl[-c(61:240),]

retusa.chl$chl.a = ((0.0604*retusa.chl$e632)+(-4.5224*retusa.chl$e649)+(13.2969*retusa.chl$e665)+(-1.7453*retusa.chl$e696))
retusa.chl$chl.b = ((-4.1982*retusa.chl$e632)+(25.7205*retusa.chl$e649)+(-7.4096*retusa.chl$e665)+(-2.7418*retusa.chl$e696))
retusa.chl$chl.c = ((28.4593*retusa.chl$e632)+(-9.9944*retusa.chl$e649)+(-1.9344*retusa.chl$e665)+(-1.8093*retusa.chl$e696))
retusa.chl$chl.d = ((-0.2007*retusa.chl$e632)+(0.0848*retusa.chl$e649)+(-0.1909*retusa.chl$e665)+(12.1302*retusa.chl$e696))
retusa.chl$chl.total = ((24.11209*retusa.chl$e632)+(11.2884*retusa.chl$e649)+(3.762*retusa.chl$e665)+(5.8338*retusa.chl$e696))



