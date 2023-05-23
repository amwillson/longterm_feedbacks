## Run script for CRC

rm(list = ls())

require(gjam, lib.loc = '/afs/crc.nd.edu/user/a/awillso2/Rlibs2')

load('~/longterm_feedbacks/gjam_data_allyeras.RData')

form1 <- as.formula(~Temperature + Precipitation)

mlist <- list(ng = 1000, burnin = 800, typeNames = 'FC')

out <- gjam(form1, xdata = xdata, ydata = ydata, modelList = mlist)

save(out, file = '~/longterm_feedbacks/out.RData')