rm(list = ls())

library(gjam)

iter <- 'indyear_mean_2_1'

load('FossilPollen/Data/gjam_data_indyears.RData')

form1 <- as.formula(~MeanTemperature + MeanPrecipitation)
form2 <- as.formula(~SDTemperature + SDPrecipitation)

mlist = list(ng = 1000, burnin = 800, typeNames = 'FC')

out <- gjam(form1, xdata = xdata_5, ydata = ydata_5, modelList = mlist)

gjamPlot(out)

outfile <- paste0('FossilPollen/out/out_',iter,'.RData')

save(out, file = outfile)
