
# Author: Dave Moore
# Date started: 03-22-2016
# Explore FLUNET 2015 data for Harvard Forest
# calculate WUE mean 95% and 5%

# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)

library(doBy)

#define constants
rhoH2O = 1000 #density of water (1mm H2O * m2 = 1 kg)
#1000 kg/m^3 
LHVAP = 2502000 #latent heat of evaporation J/kg @ 0 C
SecPerhalfhour = 30*60 #seconds per half hour
SecPerDay = 86400 #seconds per day
SecPerYear =31556926
#molecular weights to convert from moles to g
MolwtC = 12.0107
MolwtO2 =16*2 
MolwtCO2 = 44.01


folderpath="H:/GlobalDatasets/FLUXNET06202016/"

# read all the *.csv files in the working directory
#change to reflect file path

filepathHa1="H:/GlobalDatasets/FLUXNET06202016/FLX_US-Ha1_FLUXNET2015_FULLSET_1991-2012_1-1/"

tempFilelist = list.files(filepathHa1, pattern="*.csv")
directorylist = list.dirs(folderpath)

#Harvard
Ha1_annual=read.csv(paste0(filepathHa1,"FLX_US-Ha1_FLUXNET2015_FULLSET_YY_1991-2012_1-1.csv"))
Ha1_Monthly=read.csv(paste0(filepathHa1,"./FLX_US-Ha1_FLUXNET2015_FULLSET_MM_1991-2012_1-1.csv"))
Ha1_daily=read.csv(paste0(filepathHa1,"./FLX_US-Ha1_FLUXNET2015_FULLSET_DD_1991-2012_1-1.csv"))

Ha1_Meteo = read.csv(paste0(filepathHa1,"./FLX_US-Ha1_FLUXNET2015_AUXMETEO_1991-2012_1-1.csv"))
FluxnetFULLheader = read.csv("H:/GlobalDatasets/FLUXNET06202016/FLUXNET2015_FULLSET_variable_list.csv")

#VPD_F_MDS Vapor Pressure Deficit, gapfilled using MDS
#VPD_ERA		Vapor Pressure Deficit, downscaled from ERA, linearly regressed using measured only site data
#VPD_F		Vapor Pressure Deficit consolidated from VPD_F_MDS and VPD_ERA
plot(Ha1_annual$VPD_F)


Ha1_GPPMethodCompare = summaryBy(GPP_DT_VUT_MEAN + GPP_DT_CUT_MEAN + GPP_NT_VUT_MEAN + GPP_NT_CUT_MEAN ~ TIMESTAMP, data =Ha1_annual)
Ha1_VPDCompare = summaryBy ( VPD_F + VPD_F_QC  + VPD_F_MDS  + VPD_F_MDS_QC  + VPD_ERA   ~ TIMESTAMP, data =Ha1_annual)

# 
# Ha1_AnnualGPP = Ha1_annual %>%
#   mutate(SiteName="US-Ha1") %>%
#   select(SiteName, TIMESTAMP, GPP_DT_VUT_MEAN, GPP_DT_VUT_05, GPP_DT_VUT_95, GPP_DT_VUT_SE, 
#                               GPP_DT_CUT_MEAN, GPP_DT_CUT_05, GPP_DT_CUT_95, GPP_DT_CUT_SE, 
#                               GPP_NT_VUT_MEAN, GPP_NT_VUT_05, GPP_NT_VUT_95, GPP_NT_VUT_SE, 
#                               GPP_NT_CUT_MEAN, GPP_NT_CUT_05, GPP_NT_CUT_95, GPP_NT_CUT_SE,
#                               LE_F_MDS, LE_F_MDS_QC, LE_CORR,  LE_RANDUNC,                   
#                               VPD_F, VPD_F_QC, VPD_F_MDS, VPD_F_MDS_QC, VPD_ERA,
#                               TA_F, TA_F_QC, TA_F_MDS, TA_F_MDS_QC, TA_ERA)

#Annual GPP is reported in gC m-2 y-1
#Annual LE is reported as W m-2 (average from daily)

plot(Ha1_AnnualGPP$VPD_F_MDS[Ha1_AnnualGPP$VPD_F_MDS>0], Ha1_AnnualGPP$GPP_DT_VUT_MEAN[Ha1_AnnualGPP$VPD_F_MDS>0])

#annual
Ha1_FN15_WUE_annual=Ha1_annual %>%
  mutate(LHVAP_T=LHVAP-2308*TA_F_MDS) %>% #modulate latent heat of evap by ambient T
  #Annual GPP is reported in gC m-2 y-1
  #Annual LE is reported as W m-2 (average from daily)

  #DAY TIME METHOD  
  mutate(EVAPOtrans_F_MDS = (LE_F_MDS/LHVAP_T)*SecPerYear) %>% #calculate ET per YEAR
  mutate(WUE_simpleDT_VUT_MEAN=GPP_DT_VUT_MEAN/EVAPOtrans_F_MDS, WUEintrinsic_VUT_MEAN=WUE_simpleDT_VUT_MEAN*VPD_F_MDS) %>%
mutate(WUE_simpleDT_VUT_05=GPP_DT_VUT_05/EVAPOtrans_F_MDS, WUEintrinsic_VUT_MEAN=WUE_simpleDT_VUT_05*VPD_F_MDS) %>%
  mutate(WUE_simpleDT_VUT_95=GPP_DT_VUT_95/EVAPOtrans_F_MDS, WUEintrinsic_VUT_MEAN=WUE_simpleDT_VUT_95*VPD_F_MDS) %>%
  
  #Night time method
  mutate(WUE_simpleNT_VUT_MEAN=GPP_NT_VUT_MEAN/EVAPOtrans_F_MDS, WUEintrinsic_VUT_MEAN=WUE_simpleNT_VUT_MEAN*VPD_F_MDS) %>%
  mutate(WUE_simpleNT_VUT_05=GPP_NT_VUT_05/EVAPOtrans_F_MDS, WUEintrinsic_VUT_MEAN=WUE_simpleNT_VUT_05*VPD_F_MDS) %>%
  mutate(WUE_simpleNT_VUT_95=GPP_NT_VUT_95/EVAPOtrans_F_MDS, WUEintrinsic_VUT_MEAN=WUE_simpleNT_VUT_95*VPD_F_MDS) 



#monthly
Ha1_FN15_WUE_monthly=Ha1_Monthly %>%
  mutate(LHVAP_T=LHVAP-2308*TA_F_MDS) %>% #modulate latent heat of evap by ambient T
  mutate(YEAR =substring(TIMESTAMP, 1,4), MONTH=substring(TIMESTAMP, 5,7)) %>%
  # mutate(YEAR =floor(TIMESTAMP/100)) %>%
  #Monthly GPP is reported in gC m-2 y-1
  #Monthly LE is reported as W m-2 (average from daily)
  
  #DAY TIME METHOD  
  mutate(EVAPOtrans_F_MDS = (LE_F_MDS/LHVAP_T)*2592000) %>% #calculate ET per 30 days
  mutate(WUE_simpleDT_VUT_MEAN=GPP_DT_VUT_MEAN/EVAPOtrans_F_MDS, WUEintrinsic_VUT_MEAN=WUE_simpleDT_VUT_MEAN*VPD_F_MDS) %>%
  mutate(WUE_simpleDT_VUT_05=GPP_DT_VUT_05/EVAPOtrans_F_MDS, WUEintrinsic_VUT_MEAN=WUE_simpleDT_VUT_05*VPD_F_MDS) %>%
  mutate(WUE_simpleDT_VUT_95=GPP_DT_VUT_95/EVAPOtrans_F_MDS, WUEintrinsic_VUT_MEAN=WUE_simpleDT_VUT_95*VPD_F_MDS) %>%
  
  #Night time method
  mutate(WUE_simpleNT_VUT_MEAN=GPP_NT_VUT_MEAN/EVAPOtrans_F_MDS, WUEintrinsic_VUT_MEAN=WUE_simpleNT_VUT_MEAN*VPD_F_MDS) %>%
  mutate(WUE_simpleNT_VUT_05=GPP_NT_VUT_05/EVAPOtrans_F_MDS, WUEintrinsic_VUT_MEAN=WUE_simpleNT_VUT_05*VPD_F_MDS) %>%
  mutate(WUE_simpleNT_VUT_95=GPP_NT_VUT_95/EVAPOtrans_F_MDS, WUEintrinsic_VUT_MEAN=WUE_simpleNT_VUT_95*VPD_F_MDS) 


plot(Ha1_FN15_WUE_annual$WUE_simpleDT_VUT_MEAN[Ha1_FN15_WUE_annual$WUE_simpleNT_VUT_MEAN<20], Ha1_FN15_WUE_annual$WUE_simpleNT_VUT_MEAN[Ha1_FN15_WUE_annual$WUE_simpleNT_VUT_MEAN<20])
plot(Ha1_FN15_WUE$TIMESTAMP[Ha1_FN15_WUE$WUEintrinsic_VUT_MEAN>0], Ha1_FN15_WUE$WUEintrinsic_VUT_MEAN[Ha1_FN15_WUE$WUEintrinsic_VUT_MEAN>0])
