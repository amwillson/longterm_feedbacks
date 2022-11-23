## Simple test of GJAM
## Not intended as an actual analysis

library(tidyverse)
library(gjam)

load('TreeRings/Clim/HARVARD/linkages/bcc.csm1.1_001.01.Rdata')

xdata = matrix(NA, nrow = length(c(1960:2012)), ncol = 3)
ydata = matrix(NA, nrow = nrow(xdata), ncol = 5)

colnames(xdata) = c('Temp', 'Precip', 'Year')
colnames(ydata) = c('ACRU', 'FAGR', 'QURU', 'QUVE', 'TSCA')

rownames(xdata) = seq(from = 1960, to = 2012, by = 1)
rownames(ydata) = rownames(xdata)

temp.mat = tail(temp.mat, n = 56)
temp.mat = head(temp.mat, n = 53)
precip.mat = tail(precip.mat, n = 56)
precip.mat = head(precip.mat, n = 53)

xdata[,1] <- as.numeric(apply(temp.mat, 1, mean))
xdata[,2] <- as.numeric(apply(precip.mat, 1, mean))
xdata[,3] <- as.factor(rownames(xdata))

y <- obs.list_HARVARD[["obs.mean"]]
y <- unlist(y)
y_mat <- matrix(NA, nrow = length(y), ncol = 2)
y_mat[,1] = names(y)
y_mat[,2] = y

y_mat = as.data.frame(y_mat)
colnames(y_mat) <- c('Names', 'MeanAGB')

y_mat <- y_mat %>%
  mutate(Year = substr(Names, 1, 4),
         Species = if_else(grepl('Acer', Names), 'ACRU', 
                           if_else(grepl('Fagus', Names), 'FAGR',
                                 if_else(grepl('Rubra', Names), 'QURU',
                                         if_else(grepl('Velutina', Names), 'QUVE',
                                                 if_else(grepl('Tsuga', Names), 'TSCA', 'None')))))) %>%
  select(-Names) %>%
  mutate(MeanAGB = as.numeric(MeanAGB))

y_mat_wide <- y_mat %>%
  pivot_wider(names_from = Species, values_from = MeanAGB) %>%
  filter(Year %in% c(1960:2012))

ydata[,1] <- y_mat_wide$ACRU
ydata[,2] <- y_mat_wide$FAGR
ydata[,3] <- y_mat_wide$QURU
ydata[,4] <- y_mat_wide$QUVE
ydata[,5] <- y_mat_wide$TSCA

edata = rep(1, nrow(ydata)) ## This is definitely incorrect!!

xdata <- as.data.frame(xdata)
ydata <- as.data.frame(ydata)

elist <- list(columns = 1:ncol(ydata),
              values = edata)

form1 <- as.formula(~ Temp + Precip + Temp*Precip + Year)

## This is a hack to get uninformative priors on every parameter
## Hack starts here ##
spLo <- "ACRU"
sp <- length(spLo)
lo <- vector("list", sp)

# add names to the list
names(lo) <- paste0("Temp", spLo)

# add values to the list
lo[1] <- Inf

spHi <- c('ACRU')
sp <- length(spHi)
hi <- vector("list", sp)

# add names to the list
names(hi) <- paste0("Precip", spHi)

# add values to the list
hi[1:length(hi)] <- Inf
## Hack ends here

prior <- gjamPriorTemplate(formula = form1, xdata = xdata, ydata = ydata,
                           lo = lo, hi = hi)

mlist = list(ng = 10000, burnin = 2000, typeNames = 'CA', betaPrior = prior, effort = elist)

out = gjam(form1, xdata = xdata, ydata = ydata, modelList = mlist)

gjamPlot(out)


