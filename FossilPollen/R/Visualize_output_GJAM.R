## Analysis of GJAM outputs

rm(list = ls())

library(gjam)

# Load output
load('FossilPollen/outfile.RData')

# Make default plots from GJAM
#gjamPlot(out)

#### Take another look at the data ####

# Histograms of the fractional composition at each cell over time by species
ydata |>
  pivot_longer(cols = c(ash:tamarack)) |>
  rename(Taxon = name,
         Fraction = value) |>
  ggplot(aes(x = Fraction)) +
  geom_histogram() +
  facet_wrap(~Taxon)

## Look at distribution of composition by species

#### Observed vs. predicted fractional composition ####

# Take observed and predicted from the output of GJAM
observed <- out$inputs$y
predicted <- out$prediction$ypredMu

observed <- as.data.frame(observed)
predicted <- as.data.frame(predicted)

observed <- observed |>
  rownames_to_column() |>
  pivot_longer(ash:tamarack) |>
  rename(Taxon = name,
         Observed = value)
predicted <- predicted |>
  rownames_to_column() |>
  pivot_longer(ash:tamarack) |>
  rename(Taxon = name, 
         Predicted = value)

observed_predicted <- observed |>
  full_join(predicted, by = c('rowname', 'Taxon'))

# Plot all species together
observed_predicted |>
  ggplot(aes(x = Observed, y = Predicted)) +
  geom_point() +
  geom_abline() +
  xlim(c(0, 1)) + ylim(c(0, 1)) +
  geom_rug(sides = 'br')

# Plot each species in a different facet
observed_predicted |>
  ggplot(aes(x = Observed, y = Predicted, color = Taxon)) +
  geom_point() +
  geom_abline() +
  geom_rug(sides = 'br') +
  facet_wrap(~Taxon) +
  theme(legend.position = 'none')

#### Observed vs. predicted climate ####

# Take observed and predicted climate variables from GJAM output
observed <- out$inputs$xdata
predicted <- out$prediction$xpredMu
predicted <- as.data.frame(predicted)

# Format to combine observed and predicted for plotting
predicted <- predicted |>
  rownames_to_column() |>
  select(-`Temperature:Precipitation`) |>
  pivot_longer(cols = Temperature:Precipitation) |>
  rename(Variable = name,
         Value_Predicted = value)

observed_predicted <- observed |>
  rownames_to_column() |>
  pivot_longer(cols = Temperature:Precipitation) |>
  rename(Variable = name,
         Value_Observed = value) |>
  full_join(predicted, by = c('rowname', 'Variable'))

# Plot temperature and precipitation observed vs predicted
observed_predicted |>
  filter(Variable == 'Temperature') |>
  ggplot(aes(x = Value_Observed, y = Value_Predicted)) +
  geom_point() +
  geom_abline()

observed_predicted |>
  filter(Variable == 'Precipitation') |>
  ggplot(aes(x = Value_Observed, y = Value_Predicted)) +
  geom_point() +
  geom_abline()

#### Sensitivity to the climate drivers ####

sensitivity <- out$parameters$sensTable
sensitivity <- sensitivity |>
  rownames_to_column() |>
  rename(Taxon_Driver = rowname) |>
  mutate(Taxon = sub('_.*', '', Taxon_Driver),
         Driver = sub('.*_', '', Taxon_Driver))

## Notice that for some reason "other conifer" and "other hardwood" were removed

sensitivity |>
  ggplot(aes(Driver)) +
  geom_boxplot(aes(ymin = CI_025, lower = Estimate - SE, middle = Estimate,
                   upper = Estimate + SE, ymax = CI_975), stat = 'identity')
