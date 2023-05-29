## Visualize output

rm(list = ls())

library(tidyverse)
library(corrplot)
library(cowplot)
library(RColorBrewer)

load('FossilPollen/out/combined_sd.RData')

## Correlations between taxa and drivers

# Number of columns we're working with
cols <- ncol(bFacGibbs)

# Remove unnecessary columns and generate summary statistics
bFacGibbs_corr <- bFacGibbs |>
  select(-c(chain, iter))
corr_mean <- apply(bFacGibbs_corr, 2, mean, na.rm = T)
corr_sd <- apply(bFacGibbs_corr, 2, sd, na.rm = T)
corr_lower <- apply(bFacGibbs_corr, 2, quantile, probs = 0.025, na.rm = T)
corr_upper <- apply(bFacGibbs_corr, 2, quantile, probs = 0.975, na.rm = T)

# Formatting our summary statistics
corr <- rbind(corr_mean, corr_sd, corr_lower, corr_upper)
rownames(corr) <- c('mean', 'sd', 'lower', 'upper')
corr <- t(corr)
corr <- as.data.frame(corr)
corr <- corr |>
  rownames_to_column(var = 'beta') |>
  mutate(taxon = sub('_.*', '', beta),
         covariate = sub('.*_', '', beta))


corr |>
  rename(Taxon = taxon) |>
  mutate(Taxon = replace(Taxon, Taxon == 'ash', 'Ash'),
         Taxon = replace(Taxon, Taxon == 'beech', 'Beech'),
         Taxon = replace(Taxon, Taxon == 'birch', 'Birch'),
         Taxon = replace(Taxon, Taxon == 'conifer', 'Other Conifer'),
         Taxon = replace(Taxon, Taxon == 'elm', 'Elm'),
         Taxon = replace(Taxon, Taxon == 'hardwood', 'Other Hardwood'),
         Taxon = replace(Taxon, Taxon == 'hemlock', 'Hemlock'),
         Taxon = replace(Taxon, Taxon == 'maple', 'Maple'),
         Taxon = replace(Taxon, Taxon == 'oak', 'Oak'),
         Taxon = replace(Taxon, Taxon == 'pine', 'Pine'),
         Taxon = replace(Taxon, Taxon == 'spruce', 'Spruce'),
         Taxon = replace(Taxon, Taxon == 'tamarack', 'Tamarack')) |>
  dplyr::filter(covariate == 'SDTemperature') |>
  ggplot() +
  geom_boxplot(aes(x = reorder(Taxon, mean), ymin = lower, lower = mean - sd, middle = mean,
                   upper = mean + sd, ymax = upper, color = reorder(Taxon, -mean)), stat = 'identity') +
  geom_hline(aes(yintercept = 0), color = 'darkgrey', linetype = 'dashed') +
  xlab('') + ylab('Correlation') + labs(color = 'Taxon') +
  ggtitle('Correlations between temperature and fractional composition') +
  theme_minimal() +
  scale_color_viridis_d(option = 'H') +
  theme(axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, face = 'bold', size = 18),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        axis.title = element_text(size = 14))

corr |>
  rename(Taxon = taxon) |>
  mutate(Taxon = replace(Taxon, Taxon == 'ash', 'Ash'),
         Taxon = replace(Taxon, Taxon == 'beech', 'Beech'),
         Taxon = replace(Taxon, Taxon == 'birch', 'Birch'),
         Taxon = replace(Taxon, Taxon == 'conifer', 'Other Conifer'),
         Taxon = replace(Taxon, Taxon == 'elm', 'Elm'),
         Taxon = replace(Taxon, Taxon == 'hardwood', 'Other Hardwood'),
         Taxon = replace(Taxon, Taxon == 'hemlock', 'Hemlock'),
         Taxon = replace(Taxon, Taxon == 'maple', 'Maple'),
         Taxon = replace(Taxon, Taxon == 'oak', 'Oak'),
         Taxon = replace(Taxon, Taxon == 'pine', 'Pine'),
         Taxon = replace(Taxon, Taxon == 'spruce', 'Spruce'),
         Taxon = replace(Taxon, Taxon == 'tamarack', 'Tamarack')) |>
  dplyr::filter(covariate == 'SDPrecipitation') |>
  ggplot() +
  geom_boxplot(aes(x = reorder(Taxon, mean), ymin = lower, lower = mean - sd, middle = mean,
                   upper = mean + sd, ymax = upper, color = reorder(Taxon, -mean)), stat = 'identity') +
  geom_hline(aes(yintercept = 0), color = 'darkgrey', linetype = 'dashed') +
  xlab('') + ylab('Correlation') + labs(color = 'Taxon') +
  ggtitle('Correlations between precipitation and fractional composition') +
  theme_minimal() +
  scale_color_viridis_d(option = 'H') +
  theme(axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, face = 'bold', size = 18),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        axis.title = element_text(size = 16))
  
# Do some cleaning on the sensitivity (fSensGibbs)
  fSensGibbs_sum <- fSensGibbs |>
    select(-c(chain, iter))
sens_mean <- apply(fSensGibbs_sum, 2, mean, na.rm = T)  
sens_sd <- apply(fSensGibbs_sum, 2, sd, na.rm = T)
sens_lower <- apply(fSensGibbs_sum, 2, quantile, probs = 0.025, na.rm = T)
sens_upper <- apply(fSensGibbs_sum, 2, quantile, probs = 0.975, na.rm = T)

sens <- rbind(sens_mean, sens_sd, sens_lower, sens_upper)
rownames(sens) <- c('mean', 'sd', 'lower', 'upper')
sens <- t(sens)
sens <- as.data.frame(sens)
sens <- sens |>
  rownames_to_column(var = 'covar')

sens |>
  ggplot() +
  geom_boxplot(aes(x = reorder(covar, mean),
                   ymin = lower, lower = mean - sd, middle = mean,
                   upper = mean + sd, ymax = upper, color = reorder(covar, -mean)),
               stat = 'identity', size = 1.5) +
  coord_flip() +
  xlab('') + ylab(expression(paste('Sensitivity (', hat(F), ')'))) +
  theme_minimal()  +
  theme(legend.position = 'none') +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 16))

## Correlations between ecosystems

# Remove unnecessary columns
sgibbs_cor <- sgibbs |>
  select(-c(chain, iter))
# Get summary statistics
mean_sgibbs <- apply(sgibbs_cor, 2, mean)
sd_sgibbs <- apply(sgibbs_cor, 2, sd)
lower_sgibbs <- apply(sgibbs_cor, 2, quantile, probs = 0.025)
upper_sgibbs <- apply(sgibbs_cor, 2, quantile, probs = 0.975)

# Need to put into the matrix format
# This gives the index for each entry of the matrix
ind <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
         2, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
         3, 14, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33,
         4, 15, 25, 34, 35, 36, 37, 38, 39, 40, 41, 42,
         5, 16, 26, 25, 43, 44, 45, 46, 47, 48, 49, 50,
         6, 17, 27, 36, 44, 51, 52, 53, 54, 55, 56, 57,
         7, 18, 28, 37, 45, 52, 58, 59, 60, 61, 62, 63,
         8, 19, 29, 38, 46, 53, 59, 64, 65, 66, 67, 68,
         9, 20, 30, 39, 47, 54, 60, 65, 69, 70, 71, 72,
         10, 21, 31, 40, 48, 55, 61, 66, 70, 73, 74, 75,
         11, 22, 32, 41, 49, 56, 62, 67, 71, 74, 76, 77,
         12, 23, 33, 42, 50, 57, 63, 68, 72, 75, 77, 78)

# Now we format the output into a matrix
corr_mat <- mean_sgibbs[ind]
corr_mat <- matrix(corr_mat, nrow = 12, ncol = 12)
corr_mat <- cov2cor(corr_mat)
colnames(corr_mat) <- rownames(corr_mat) <- c('Ash', 'Beech', 'Birch',
                                              'Elm', 'Hemlock', 'Maple',
                                              'Oak', 'Other Conifer', 'Other Hardwood',
                                              'Pine', 'Spurce', 'Tamarack')

# Plot
corrplot(corr_mat, diag = T, type = 'upper', method = 'color',
         tl.col = 'black', col = brewer.pal(n = 11, name = 'PRGn'))

# With upper and lower credible intervals
low_mat <- lower_sgibbs[ind]
low_mat <- matrix(low_mat, nrow = 12, ncol = 12)
low_mat <- cov2cor(low_mat)
low_mat[low_mat < -1] <- -1
low_mat[low_mat > 1] <- 1
colnames(low_mat) <- rownames(low_mat) <- colnames(corr_mat)

upp_mat <- upper_sgibbs[ind]
upp_mat <- matrix(upp_mat, nrow = 12, ncol = 12)
upp_mat <- cov2cor(upp_mat)
upp_mat[upp_mat < -1] <- -1
upp_mat[upp_mat > 1] <- 1
colnames(upp_mat) <- rownames(upp_mat) <- colnames(corr_mat)

corrplot(corr_mat, lowCI.mat = low_mat, uppCI.mat = upp_mat, plotCI = 'rect')
