
# This script reads AmeriFlux L2 files (*.csv), gets the time step in the L2 data (either half-hourly or hourly) 
#  and it creates an input file for REddyProc package (*.txt) with the appropriate 
# time step (either half-hourly or hourly) and format.  
# For additional information about the format of a REddyProc input file see example available in: 
# https://www.bgc-jena.mpg.de/bgi/uploads/Services/REddyProcWebDataFormat/Example_DETha98.txt
# Author: Francesc Montane
# Contact: fmontane@email.arizona.edu

######################################################################################################################################
#####   IMPORTANT NOTES BEFORE RUNNING THE SCRIPT                                                                                 ####
#####                                                                                                                             ####
#####   1- Make sure that only the required *.csv L2 files are placed in the working directory (wd)                               ####
#####   2- Make sure that the L2 files in the wd are only for one site                                                            ####
#####   3- Make sure that the L2 files in the wd are only of one type (either gap filled or with gaps, but not both)              ####
#####   4- Make sure that each L2 file in the wd contains data for the whole year (starting at DOY=1)                             ####
#####   5- Delete L2 files in the wd that do not start at DOY=1, if any (for instance, L2 files for NR1 and year 1998)            ####
#####   6- To create a REddyProc input file with one year only, place only one L2 *csv file for that particular year in the wd    ####
#####   7 -To create a REddyProc input file with several years, place the L2 *csv files for all the years in the wd               ####          
#####                                                                                                                             ####
######################################################################################################################################

# modify working directory and "filepath"


# setwd("D:/Sites_DOE/AmeriFlux/Niwot Ridge/L2_gap_filled/V008")
# filepath="D:/Sites_DOE/AmeriFlux/Niwot Ridge/L2_gap_filled/V008/"

setwd("D:/GlobalDatasets/ameriflux.allsites.L2_data.05Mar2016/")


# 
# Howland
# 
filepath="Howland_Forest_Main/gap_filled/"
# read all the *.csv files in the working directory
#change to reflect file path
tempFilelist = list.files(filepath, pattern="*.csv")
# get the AmeriFlux files information (site and years)
files<-substr(tempFilelist,7,14)
# get the AmeriFlux site code
site<-substr(tempFilelist[1],7,9)
# get the data and header from the L2 files
Howland = do.call("rbind", lapply(paste0(filepath,tempFilelist), function(x) read.csv(x, skip=20,header=FALSE,stringsAsFactors = FALSE)))
AmFluxheader=read.csv(paste0(filepath,tempFilelist[1]),skip=17, strip.white=TRUE, nrows=1 ,header=FALSE,stringsAsFactors=FALSE)
colnames(Howland)<-AmFluxheader

#hack Howland2009.xls
#hack Howland20102011.xls

# 
# Harvard
# 

# read all the *.csv files in the working directory
filepath="Harvard_Forest/gap_filled/"
#change to reflect file path
tempFilelist = list.files(filepath, pattern="*.csv")
# get the AmeriFlux files information (site and years)
files<-substr(tempFilelist,7,14)
# get the AmeriFlux site code
site<-substr(tempFilelist[1],7,9)
# get the data and header from the L2 files
Harvard = do.call("rbind", lapply(paste0(filepath,tempFilelist), function(x) read.csv(x, skip=20,header=FALSE,stringsAsFactors = FALSE)))
AmFluxheader=read.csv(paste0(filepath,tempFilelist[1]),skip=17, strip.white=TRUE, nrows=1 ,header=FALSE,stringsAsFactors=FALSE)
colnames(Harvard)<-AmFluxheader

setwd("D:/Dropbox/rProjectsShare/Belmecheri_et_WUE/data/")
save(Howland, file = "Howland.ameriflux.allsites.L2_data.05Mar2016.RData")
save(Harvard, file = "Harvard.ameriflux.allsites.L2_data.05Mar2016.RData")

library(dplyr)
library(ggplot2)
library(tidyr)
junk2005 = filter(Howland,YEAR==2005)
junk2005a = filter(junk2005,GAP==0)
plot(junk2005a$DOY,junk2005a$GPP)

junk=Howland %>%
filter(GAP==0) %>%
  mutate(PRECIND=PREC>0, PREClag1 = lag(PREC>0), PREClag2 = lag(PREClag1==TRUE) ) %>%
  mutate(PREC2dayINDlst = 'TRUE' %in% list(PRECIND,PREClag1,PREClag2)) %>%
  mutate(WUE=GPP/LE)


list1=c(junk$PRECIND,junk$PREClag1,junk$PREClag2)

for (i in 1:length(junk$PREC))
  
{
  PREC2dayINDlst[i]='TRUE'%in% list1[i] 
  }



plot(junk$VPD[junk$VPD>-100],junk$WUE[junk$VPD>-100])


#there are gaps in the Howland Data
#there are gaps in VPD which are not the same as the NEE or GPP data
#summarize by day - 
#Count the gaps per day - new variables 





