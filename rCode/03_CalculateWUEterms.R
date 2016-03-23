# Author: Dave Moore
# Date started: 03-08-2016
# Explore L2 Data and create basic diagnostic metrics
# 1) How many missing data obs(in gapfilled data)
# 2) Describe patterns of GPP and LE with respect to VPD


# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)

Havard=load ("./data/Harvard.ameriflux.allsites.L2_data.05Mar2016.RData")
Howland=load("./data/Howland.ameriflux.allsites.L2_data.05Mar2016.RData")

hist(Harvard$GAP)
plot(Harvard$GPP[Harvard$GAP==0])

#define constants
rhoH2O = 1000 #density of water (1mm H2O * m2 = 1 kg)

#1000 kg/m^3 
LHVAP = 2502000 #latent heat of evaporation J/kg @ 0 C
SecPerhalfhour = 30*60 #seconds per half hour
SecPerDay = 86400 #seconds per day
#assume no sublimation

#define RHO 1.3 // air density (kg/m^3)
#define CP 1005. // specific heat of air (J/(kg K))
#define GAMMA 66. // psychometric constant (Pa/K)
#define E_STAR_SNOW 0.6 /* approximate saturation vapor pressure at 0 degrees C (kPa) 
# (we assume snow temperature is 0 degrees C or slightly lower) */

# 
# Evaportranspiration from latent heat of evaporation
# 
#Units LE (J s-1 m-2)
#Units E (kg m-2 s-1) 
# Note that 1 kg m-2 of evaporation is the same as a 1 mm of water over a square
# meter (assuming a water density of 1000 kg m-3).

# LE = LHVAP*EVAPOtrans

#molecular weights to convert from moles to g
MolwtC = 12.0107
MolwtO2 =16*2 
MolwtCO2 = 44.01

# VPsat = 0.611.exp[(17.3*T)/(T+237.3)]


# 
# Harvard
# 
Harvard_gapless=Harvard %>%
  filter(GAP==0) %>% #remove gaps
  mutate(LEgaps = 0) %>% #define code for LE gaps
  mutate(LEgaps = replace (LEgaps, LE==-9999, 1)) %>% #flag = 1 if there is a gap
  mutate(LE = replace(LE, LE==-9999, NA)) %>% #replace -9999 with NA in r
  mutate(LE = replace(LE, LE<0, 0)) %>% #replace neg values with zeros
  mutate(VPDgaps = 0) %>%  #define code for VPD gaps
  mutate(GPP = replace(GPP, GPP<0, 0)) %>% #replace neg GPP values with 0
  mutate(VPDgaps = replace (VPDgaps, VPD==-9999, 1)) %>%  
  mutate(VPD = replace(VPD, VPD==-9999, NA)) %>% #replace -9999 with NA in r
  mutate(PREC = replace(PREC, PREC==-9999, NA)) %>% #flag = 1 if there is a gap
  mutate(LHVAP_T=LHVAP-2308*TA) %>% #modulate latent heat of evap by ambient T
  #GPP is reported in micromoles CO2 per m^2 per sec
  #we want GPP in gC per m^2 per time step | time step is 30 minutes
  mutate(GPPgC = (GPP/1000000)*(SecPerhalfhour*MolwtC)) %>%  #GPP in gC
  mutate(EVAPOtrans = (LE/LHVAP_T)*SecPerhalfhour) #calculate ET per 30 minutes


Harvard_daily = group_by(Harvard_gapless,YEAR,DOY)
# 
# Calculate mean for ET, VPD and GPP ignoring NA  
# 
HarvardFilling = Harvard_daily %>%
  summarise(ETfill=mean(EVAPOtrans, na.rm = TRUE), VPDfill=mean(VPD, na.rm = TRUE),GPPfill=mean(GPPgC, na.rm = TRUE), LEgapsCT=sum(LEgaps), VPDgapsCT=sum(VPDgaps))  %>%
  select(YEAR,DOY,ETfill, VPDfill, GPPfill, LEgapsCT, VPDgapsCT) 

#join back by YEAR and DOY
HarvardFill01 = Harvard_daily  %>%
  left_join(HarvardFilling)  %>%
  mutate(EVAPOtrans = replace(EVAPOtrans, LEgapsCT>20, ETfill)) %>% #gapfill ET
  mutate(VPD = replace(VPD, VPDgapsCT>20, VPDfill))  #gapfill ET


# Harvard_daySum = Harvard_daily %>%
#   summarise( n=n(),ETdaily=sum(EVAPOtrans, na.rm = TRUE), VPDdaily=mean(VPD, na.rm = TRUE),GPPdaily=sum(GPPgC, na.rm = TRUE),Precip=sum(PREC, na.rm = TRUE), LEgapsCT=sum(LEgaps), VPDgapsCT=sum(VPDgaps))


Harvard_daySumFilled = HarvardFill01 %>%
  summarise( n=n(),ETdaily=sum(EVAPOtrans, na.rm = TRUE), VPDdaily=mean(VPD, na.rm = TRUE),GPPdaily=sum(GPPgC, na.rm = TRUE),Precip=sum(PREC, na.rm = TRUE), LEgapsCT=sum(LEgaps), VPDgapsCT=sum(VPDgaps))


HarvardWUE =Harvard_daySumFilled %>%
  mutate (precipFlag=0) %>%
  mutate (precipLagged=lag(Precip)) %>%
  mutate(precipFlag =replace(precipFlag,Precip>0,1)) %>%
  mutate(precipFlag =replace(precipFlag,lag(Precip)>0,1)) %>%
  mutate(precipFlag =replace(precipFlag,lag(precipLagged)>0,1)) %>%
  filter(precipFlag==0)%>%
  mutate(WUE_simple=GPPdaily/ETdaily, WUEintrinsic=WUE_simple*VPDdaily) %>%
  filter(WUE_simple<100) %>%
  
  mutate (ETobs = 0) %>%
  mutate(ETobs =replace(ETobs,ETdaily>0,1)) %>%
  mutate (GPPobs = 0) %>%
  mutate(GPPobs =replace(GPPobs,GPPdaily>0,1)) %>%
  mutate (VPDobs = 0) %>%
  mutate(VPDobs =replace(VPDobs,VPDdaily>0,1))

Harvard_AnnWUE = group_by(HarvardWUE,YEAR) %>%
  summarise( n=n(), GPP=median(GPPdaily), GPPobs=sum(GPPobs), ET=median(ETdaily), ETobs=sum(ETobs), WUEmed = median(WUE_simple), WUEintrinsicmed=median(WUEintrinsic), WUEmean = mean(WUE_simple), WUEintrinsicmean=mean(WUEintrinsic), VPDobs=sum(VPDobs) )

write.csv(Harvard_AnnWUE, file ="./data/HarvardWUEstats.csv" )

# 
# Howland
Howland_gapless=Howland %>%
  filter(GAP==0) %>% #remove gaps
  mutate(LEgaps = 0) %>% #define code for LE gaps
  mutate(LEgaps = replace (LEgaps, LE==-9999, 1)) %>% #flag = 1 if there is a gap
  mutate(LE = replace(LE, LE==-9999, NA)) %>% #replace -9999 with NA in r
  mutate(LE = replace(LE, LE<0, 0)) %>% #replace neg values with zeros
  mutate(VPDgaps = 0) %>%  #define code for VPD gaps
  mutate(GPP = replace(GPP, GPP<0, 0)) %>% #replace neg GPP values with 0
  mutate(VPDgaps = replace (VPDgaps, VPD==-9999, 1)) %>%  
  mutate(VPD = replace(VPD, VPD==-9999, NA)) %>% #replace -9999 with NA in r
  mutate(PREC = replace(PREC, PREC==-9999, NA)) %>% #flag = 1 if there is a gap
  mutate(LHVAP_T=LHVAP-2308*TA) %>% #modulate latent heat of evap by ambient T
  #GPP is reported in micromoles CO2 per m^2 per sec
  #we want GPP in gC per m^2 per time step | time step is 30 minutes
  mutate(GPPgC = (GPP/1000000)*(SecPerhalfhour*MolwtC)) %>%  #GPP in gC
  mutate(EVAPOtrans = (LE/LHVAP_T)*SecPerhalfhour) #calculate ET per 30 minutes


Howland_daily = group_by(Howland_gapless,YEAR,DOY)
# 
# Calculate mean for ET, VPD and GPP ignoring NA  
# 
HowlandFilling = Howland_daily %>%
  summarise(ETfill=mean(EVAPOtrans, na.rm = TRUE), VPDfill=mean(VPD, na.rm = TRUE),GPPfill=mean(GPPgC, na.rm = TRUE), LEgapsCT=sum(LEgaps), VPDgapsCT=sum(VPDgaps))  %>%
  select(YEAR,DOY,ETfill, VPDfill, GPPfill, LEgapsCT, VPDgapsCT) 

#join back by YEAR and DOY
HowlandFill01 = Howland_daily  %>%
  left_join(HowlandFilling)  %>%
  mutate(EVAPOtrans = replace(EVAPOtrans, LEgapsCT>20, ETfill)) %>% #gapfill ET
  mutate(VPD = replace(VPD, VPDgapsCT>20, VPDfill))  #gapfill ET


# Howland_daySum = Howland_daily %>%
#   summarise( n=n(),ETdaily=sum(EVAPOtrans, na.rm = TRUE), VPDdaily=mean(VPD, na.rm = TRUE),GPPdaily=sum(GPPgC, na.rm = TRUE),Precip=sum(PREC, na.rm = TRUE), LEgapsCT=sum(LEgaps), VPDgapsCT=sum(VPDgaps))


Howland_daySumFilled = HowlandFill01 %>%
  summarise( n=n(),ETdaily=sum(EVAPOtrans, na.rm = TRUE), VPDdaily=mean(VPD, na.rm = TRUE),GPPdaily=sum(GPPgC, na.rm = TRUE),Precip=sum(PREC, na.rm = TRUE), LEgapsCT=sum(LEgaps), VPDgapsCT=sum(VPDgaps))


HowlandWUE =Howland_daySumFilled %>%
  mutate (precipFlag=0) %>%
  mutate (precipLagged=lag(Precip)) %>%
  mutate(precipFlag =replace(precipFlag,Precip>0,1)) %>%
  mutate(precipFlag =replace(precipFlag,lag(Precip)>0,1)) %>%
  mutate(precipFlag =replace(precipFlag,lag(precipLagged)>0,1)) %>%
  filter(precipFlag==0)%>%
  mutate(WUE_simple=GPPdaily/ETdaily, WUEintrinsic=WUE_simple*VPDdaily) %>%
  filter(WUE_simple<100) %>%
  
  mutate (ETobs = 0) %>%
  mutate(ETobs =replace(ETobs,ETdaily>0,1)) %>%
  mutate (GPPobs = 0) %>%
  mutate(GPPobs =replace(GPPobs,GPPdaily>0,1)) %>%
  mutate (VPDobs = 0) %>%
  mutate(VPDobs =replace(VPDobs,VPDdaily>0,1))

Howland_AnnWUE = group_by(HowlandWUE,YEAR) %>%
  summarise( n=n(), GPP=median(GPPdaily), GPPobs=sum(GPPobs), ET=median(ETdaily), ETobs=sum(ETobs), WUEmed = median(WUE_simple), WUEintrinsicmed=median(WUEintrinsic), WUEmean = mean(WUE_simple), WUEintrinsicmean=mean(WUEintrinsic), VPDobs=sum(VPDobs) )

write.csv(Howland_AnnWUE, file ="./data/HowlandWUEstats.csv" )
