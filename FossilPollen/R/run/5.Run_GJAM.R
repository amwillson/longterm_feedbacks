rm(list = ls())

library(gjam)

iter <- '3'

load('FossilPollen/Data/gjam_data_allyears.RData')

form1 <- as.formula(~Temperature + Precipitation)

mlist = list(ng = 1000, burnin = 800, typeNames = 'FC')

out <- gjam(form1, xdata = xdata, ydata = ydata, modelList = mlist)

gjamPlot(out)

outfile <- paste0('FossilPollen/out/out_',iter,'.RData')

save(out, file = outfile)
