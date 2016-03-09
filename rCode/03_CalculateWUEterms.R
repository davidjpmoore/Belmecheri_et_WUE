# Author: Dave Moore
# Date started: 03-08-2016
# Explore L2 Data and create basic diagnostic metrics
# 1) How many missing data obs(in gapfilled data)
# 2) Describe patterns of GPP and LE with respect to VPD


# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)

#define constants
rhoH2O = 1 #density of water (1mm H2O * m2 = 1 kg)
LHVAP = 2501000 #latent heat of evaporation J/kg @ 0 C
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
EVAPOtrans = LE/2501000

# VPsat = 0.611.exp[(17.3*T)/(T+237.3)]


Howland_gapless=Howland %>%
  filter(GAP==0) %>% #remove gaps
  filter(LE>0) %>% #remove neg LE values
  filter(VPD>0) %>% #remove neg VPD values
  mutate(PRECIND=PREC>0, PREClag1 = lag(PREC>0), PREClag2 = lag(PREClag1==TRUE) ) %>%
  mutate(PREC2dayINDlst = 'TRUE' %in% list(PRECIND,PREClag1,PREClag2)) %>%
  mutate(EVAPOtrans = LE/2501000, WUE=GPP/EVAPOtrans, WUEintrinsic=WUE*VPD)

# 
# There are anomolously high instantaneous WUE numbers - should aggregate to daily before we calculate WUE

Harvard_gapless=Harvard %>%
  filter(GAP==0) %>% #remove gaps
  filter(LE>0) %>% #remove neg LE values
  filter(VPD>0) %>% #remove neg VPD values
  mutate(PRECIND=PREC>0, PREClag1 = lag(PREC>0), PREClag2 = lag(PREClag1==TRUE) ) %>%
  mutate(PREC2dayINDlst = 'TRUE' %in% list(PRECIND,PREClag1,PREClag2)) %>%
  mutate(EVAPOtrans = LE/2501000, WUE=GPP/EVAPOtrans, WUEintrinsic=WUE*VPD)

# 
# Diagnostic plots and checks

# plot(Howland_gapless$VPD, Howland_gapless$EVAPOtrans*SecPerDay) # fluxes are in per second - this hack gives approx daily values for EACH Obs 
# plot(Howland_gapless$DOY, Howland_gapless$WUEintrinsic)
# plot(Howland_gapless$DOY, Howland_gapless$WUE)


# 
# Create flag to remove rain days and subsequent 3 days 
# This is currently nonsense - the lags are 30 minute time periods ... it makes no sense!
How.PREC2dayINDlst=Howland_gapless$PRECIND
ThreeDayPRECIP_how=c(Howland_gapless$PRECIND,Howland_gapless$PREClag1,Howland_gapless$PREClag2)
for (i in 1:length(Howland_gapless$PREC))
{
  How.PREC2dayINDlst[i]='TRUE'%in% ThreeDayPRECIP_how[i] 
}

table(How.PREC2dayINDlst)["TRUE"]
table(Howland_gapless$PRECIND)["TRUE"]
table(Howland_gapless$PREClag1)["TRUE"]




plot(junk$VPD[junk$VPD>-100],junk$WUE[junk$VPD>-100])


#there are gaps in the Howland Data
#there are gaps in VPD which are not the same as the NEE or GPP data
#summarize by day - 
#Count the gaps per day - new variables 
