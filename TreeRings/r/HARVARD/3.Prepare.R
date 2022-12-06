## This script prepares all the data and components for running GJAM
## This includes final formatting, defining the formula being used, and defining priors

## Author: AM Willson

rm(list = ls())

library(gjam)

load('TreeRings/Data/Processed/HARVARD_proc.RData')

#### Effort ####

## Here, you can choose if you want effort to be species specific or not
## Specify in the below variable and the correct if statement will run

effort_type <- 'species' # options: 'species' or 'total'

if(effort_type == 'total'){
  edata <- var_df$precision
}

if(effort_type == 'species'){
  edata <- prec_df
}

# Format xdata & ydata correctly
xdata <- as.data.frame(xdata)
ydata <- as.data.frame(ydata)

# Format effort correctly
elist <- list(columns = 1:ncol(ydata),
              values = edata)

#### Formula ####

## Specify your formula in this section
## Here, I'm assuming that biomass increment is a simple function of average annual
## temperature and precipitation
form1 <- as.formula(~ Temp + Precip)

#### Priors ####

## This section specifies uninformative priors on each model parameter
## Due to a quirk of the GJAM package, even if you don't want to inform your priors with
## data, you have to specify at least a couple priors manually.
## This is a hack to get uninformative priors on every parameter
## Hack starts here ##
spLo <- "ACRU"
sp <- length(spLo)
lo <- vector("list", sp)
names(lo) <- paste0("Temp", spLo)
lo[1] <- Inf
spHi <- c('ACRU')
sp <- length(spHi)
hi <- vector("list", sp)
names(hi) <- paste0("Precip", spHi)
hi[1:length(hi)] <- Inf
## Hack ends here

# Define priors using the GJAM package
prior <- gjamPriorTemplate(formula = form1, xdata = xdata, ydata = ydata,
                           lo = lo, hi = hi)

#### Save ####

save(prior, elist, form1, xdata, ydata, file = 'TreeRings/Data/Processed/HARVARD_prep.RData')