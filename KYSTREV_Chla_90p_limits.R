##Title: Calculations for chlorophyll a 90 percentile
##Author: Sarah Lerch
##Date: 2024-10-24
##Description: Calculating new boundies for chlororphyll a based on 90 percentile of monitoring data within respective growth season


library(dplyr)
library(zoo)
library(tidyr)
library(xlsx)

#Set working directories 
wd.data <- "xx" #add correct path
wd.save <- "xx" #add correct path

#Set input file
fname.data <- "KYSTREV_2024_cleandata_final_v3.RData"

#Set output file
fname.out <- "KYSTREV_2024_90p_nuts_avchl_WFD.RData"
fname.out.xl <- "KYSTREV_2024_90p_nuts_avchl_WFD.xlsx"

#Move to directory with data files
setwd(wd.data)

#Load data
load(fname.data) #df_final_newsnox

#rename
data.final <- df_final_newsnox

#Subset only the portion of the df with parameter values of interest
params.keep <- c("KlfA",
                 "NH4","SNOx","TOTN","NO3","NO2",
                 "TOTP","PO4")
data.sub <- data.final[which(data.final$Parameter %in% params.keep & data.final$WFD == T),]

#Remove the columns with sample specific information
data.sub <- unique(data.sub[,c("Master_Ecoregion","Master_station_code","Master_RegType","Parameter", "Master_value","Month","Year_growth","Sample_date","season","eqr_season")])

#Add column which determines season for nutrients
data.sub$season <- NA

#Determine season for nutrients
data.sub$season[which(data.sub$Month %in% c(12,1,2))] <- "winter"
data.sub$season[which(data.sub$Month %in% c(6,7,8))] <- "summer"

#THIS SETS THE GROWTH SEASONS USED TO CALCULATE THE 90TH PERCENTILE VALUES
#Add a column which determines the season for chl, EQR 
data.sub$eqr_season <- NA
data.sub$eqr_season[which(data.sub$Master_Ecoregion == "Norskehavet Sør" & data.sub$Month %in% c(3,4,5,6,7,8,9) |
                            data.sub$Master_Ecoregion ==  "Norskehavet Nord" & data.sub$Month %in% c(3,4,5,6,7,8,9) |
                            data.sub$Master_Ecoregion ==  "Barentshavet" & data.sub$Month %in% c(3,4,5,6,7,8,9))] <- "eqr"
data.sub$eqr_season[which(data.sub$Master_Ecoregion == "Skagerrak" & data.sub$Month %in% c(2,3,4,5,6,7,8,9,10) |
                            data.sub$Master_Ecoregion ==  "Nordsjøen" & data.sub$Month %in% c(2,3,4,5,6,7,8,9,10) |
                            data.sub$Master_Ecoregion ==  "Nordsjøen Sør" & data.sub$Month %in% c(2,3,4,5,6,7,8,9,10) |
                            data.sub$Master_Ecoregion == "Nordsjøen Nord" & data.sub$Month %in% c(2,3,4,5,6,7,8,9,10))] <- "eqr"




#90TH PERCENTILE GROWTH SEASON VALUES CALCULATED BELOW
#90th percentile chla values

#create a df with average mean chl values by sampling day
data_90p_temp1 <- data.sub %>%
  group_by(Year_growth,Sample_date,Master_station_code) %>%
  filter((Parameter == "KlfA")) %>%
  filter((eqr_season == "eqr")) %>% #Filter for only data within the growth season
  mutate(chl.day.mean = mean(Master_value, na.rm = T)) %>%
  select(!c("Master_value","Month","Year_growth")) %>%
  distinct()

#Calculate 90th percentile growth season values, using mean station-day chla values  
data_90p <- data_90p_temp1 %>%
  group_by(Year_growth, Master_station_code) %>%
  mutate(perc.90 = quantile(chl.day.mean, probs = c(0.9), type = 6, na.rm = T)) %>% #TYPE 6 IS THE SAME TYPE OF 90TH PERCENTILE CALCULATION METHOD AS IS USED CURRENTLY (THERE ARE SLIGHTLY DIFFERENT WAYS TO DO THIS)
  select(!c("chl.day.mean","Sample_date","season","eqr_season","Parameter")) %>%
  distinct()

