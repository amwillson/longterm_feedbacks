rm(list = ls())

library(gjam)

iter <- 'mean_var_1'

load('FossilPollen/Data/gjam_data_allyears.RData')

form1 <- as.formula(~MeanTemperature + MeanPrecipitation)
form2 <- as.formula(~SDTemperature + SDPrecipitation)

mlist = list(ng = 10, burnin = 8, typeNames = 'FC')

out <- gjam(form2, xdata = xdata, ydata = ydata, modelList = mlist)

gjamPlot(out)

outfile <- paste0('FossilPollen/out/out_',iter,'.RData')

save(out, file = outfile)
