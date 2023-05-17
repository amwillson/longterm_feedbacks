## Run GJAM for a sample region (Sylvania Wilderness)

rm(list = ls())

library(gjam)
library(tidyverse)

# Load data

load('FossilPollen/Data/sylvania_ydata.RData')
load('FossilPollen/Data/sylvania_edata.RData')
load('Climate/xdata_sylvania.RData')

#### Match up data ####

## First, we need to make sure that the rows in each data frame are in the same order
ydata <- ydata |>
  rownames_to_column() |>
  mutate(time = substr(rowname, start = 36, stop = 39)) |>
  mutate(time = as.numeric(time))

xdata <- xdata |>
  rownames_to_column() |>
  mutate(time = substr(rowname, start = 22, stop = 25)) |>
  mutate(time = as.numeric(time))

edata <- elist$values
edata <- edata |>
  rownames_to_column() |>
  mutate(time = substr(rowname, start = 36, stop = 39)) |>
  mutate(time = as.numeric(time))

ydata_colnames <- colnames(ydata)
xdata_colnames <- colnames(xdata)
edata_colnames <- colnames(edata)

joined <- ydata |>
  full_join(xdata, by = 'time') |>
  full_join(edata, by = 'time') |>
  drop_na()

cols <- colnames(joined)
ydata_cols <- which(grepl('.x', cols, fixed = T))
xdata_cols <- which(cols == 'Temperature' | cols == 'Precipitation')
edata_cols <- which(grepl('.y', cols, fixed = T))

ydata <- joined |>
  select(all_of(ydata_cols))
colnames(ydata) <- ydata_colnames[(1:ncol(ydata))]
ydata <- ydata |>
  column_to_rownames(var = 'rowname')

xdata <- joined |>
  select(all_of(xdata_cols))
colnames(xdata) <- xdata_colnames[2:3]
rownames(xdata) <- rownames(ydata)

edata <- joined |>
  select(all_of(edata_cols))
colnames(edata) <- edata_colnames[1:ncol(edata)]
edata <- edata |>
  column_to_rownames(var = 'rowname')

elist$values <- edata

#### Set up GJAM ####
form1 <- as.formula(~Temperature + Precipitation + Temperature * Precipitation)

#### Run GJAM ####

# Prepare & run model
mlist = list(ng = 10000, burnin = 2000, typeNames = 'FC',# betaPrior = prior,
             effort = elist)
out = gjam(form1, xdata = xdata, ydata = ydata, modelList = mlist)
