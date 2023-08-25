rm(list = ls())

library(gjam)
library(dplyr)

#load('FossilPollen/Data/gjam_data_allyears.RData')
load('FossilPollen/Data/downscaled_processed_xydata.RData')

xdata <- xydata |>
  filter(Year == 50) |>
  select(Downscale_Temperature_1:SD_Precipitation, Year) |>
  rename(Downscale.Temperature.1 = Downscale_Temperature_1,
         Downscale.Temperature.2 = Downscale_Temperature_2,
         Downscale.Temperature.3 = Downscale_Temperature_3,
         Downscale.Temperature.4 = Downscale_Temperature_4,
         Downscale.Temperature.5 = Downscale_Temperature_5,
         Downscale.Temperature.6 = Downscale_Temperature_6,
         Downscale.Temperature.7 = Downscale_Temperature_7,
         Downscale.Temperature.8 = Downscale_Temperature_8,
         Downscale.Temperature.9 = Downscale_Temperature_9,
         Downscale.Temperature.10 = Downscale_Temperature_10,
         Downscale.Temperature.11 = Downscale_Temperature_11,
         Downscale.Temperature.12 = Downscale_Temperature_12,
         Downscale.Precipitation.1 = Downscale_Precipitation_1,
         Downscale.Precipitation.2 = Downscale_Precipitation_2,
         Downscale.Precipitation.3 = Downscale_Precipitation_3,
         Downscale.Precipitation.4 = Downscale_Precipitation_4,
         Downscale.Precipitation.5 = Downscale_Precipitation_5,
         Downscale.Precipitation.6 = Downscale_Precipitation_6,
         Downscale.Precipitation.7 = Downscale_Precipitation_7,
         Downscale.Precipitation.8 = Downscale_Precipitation_8,
         Downscale.Precipitation.9 = Downscale_Precipitation_9,
         Downscale.Precipitation.10 = Downscale_Precipitation_10,
         Downscale.Precipitation.11 = Downscale_Precipitation_11,
         Downscale.Precipitation.12 = Downscale_Precipitation_12,
         Mean.Temperature = Mean_Temperature,
         SD.Temperature = SD_Temperature,
         Mean.Precipitation = Mean_Precipitation,
         SD.Precipitation = SD_Precipitation) |>
  mutate(Year = as.factor(Year))
ydata <- xydata |>
  filter(Year == 50) |>
  select(ash:tamarack) |>
  rename(conifer = other_conifer,
         hardwood = other_hardwood)

form1 <- as.formula(~Mean.Temperature + Mean.Precipitation + SD.Temperature + SD.Precipitation)

mlist = list(ng = 1000, burnin = 200, typeNames = 'FC')

out <- gjam(form1, xdata = xdata, ydata = ydata, modelList = mlist)

gjamPlot(out)

iter <- 'mean_var_071423_3'

outfile <- paste0('FossilPollen/out/out_',iter,'.RData')

save(out, file = outfile)
