## Combine chains

rm(list = ls())

library(tidyverse)

files <- list.files(path = 'FossilPollen/out/')
nchains <- length(files)

# Loop over the files to store them in one object
comb_out <- list()
ind <- 1
for(i in files){
  file_name <- paste0('FossilPollen/out/', i)
  load(file_name)
  comb_out[[ind]] <- out
  ind <- ind + 1
}

# Now let's combine chains, which is what we really care about
# Every run should have five outputs in chains:
  # bFacGibbs
  # bgibbs
  # bgibbsUn
  # fSensGibbs
  # sgibbs
# We'll work with these separately

# Number of iterations per chain
niter <- nrow(comb_out[[1]]$chains$bFacGibbs)

# Initialize each matrix with the first chain
# Extract first chain from our list
temp <- comb_out[[1]]

# Take out just the chain corresponding to bFacGibbs
bFacGibbs <- as.data.frame(temp$chains$bFacGibbs)
# Make a new column specifying the chain
# This will be used to remove burnin and for plotting
bFacGibbs$chain <- rep(1, length = niter)
# Make a new column speciying the iteration
# This will be used to remove burnin and for plotting
bFacGibbs$iter <- as.numeric(rownames(bFacGibbs))
# Insert this into our combined matrix
comb_bFacGibbs <- bFacGibbs

# Same with bgibbs
bgibbs <- as.data.frame(temp$chains$bgibbs)
bgibbs$chain <- rep(1, length = niter)
bgibbs$iter <- as.numeric(rownames(bgibbs))
comb_bgibbs <- bgibbs

# gibbsUn
bgibbsUn <- as.data.frame(temp$chains$bgibbsUn)
bgibbsUn$chain <- rep(1, length = niter)
bgibbsUn$iter <- as.numeric(rownames(bgibbsUn))
comb_bgibbsUn <- bgibbsUn

# fSensGibbs
fSensGibbs <- as.data.frame(temp$chains$fSensGibbs)
fSensGibbs$chain <- rep(1, length = niter)
fSensGibbs$iter <- as.numeric(rownames(fSensGibbs))
comb_fSensGibbs <- fSensGibbs

# sgibbs
sgibbs <- as.data.frame(temp$chains$sgibbs)
sgibbs$chain <- rep(1, length = niter)
sgibbs$iter <- as.numeric(rownames(sgibbs))
comb_sgibbs <- sgibbs

# Repeat for all chains after chain 1
for(i in 2:nchains){
  # bFacGibbs
  temp <- comb_out[[i]]
  bFacGibbs <- as.data.frame(temp$chains$bFacGibbs)
  bFacGibbs$chain <- rep(i, length = niter)
  bFacGibbs$iter <- as.numeric(rownames(bFacGibbs))
  comb_bFacGibbs <- rbind(comb_bFacGibbs, bFacGibbs)
  
  # bgibbs
  bgibbs <- as.data.frame(temp$chains$bgibbs)
  bgibbs$chain <- rep(i, length = niter)
  bgibbs$iter <- as.numeric(rownames(bgibbs))
  comb_bgibbs <- rbind(comb_bgibbs, bgibbs)
  
  # bgibbsUn
  bgibbsUn <- as.data.frame(temp$chains$bgibbsUn)
  bgibbsUn$chain <- rep(i, length = niter)
  bgibbsUn$iter <- as.numeric(rownames(bgibbsUn))
  comb_bgibbsUn <- rbind(comb_bgibbsUn, bgibbsUn)
  
  # fSensGibbs
  fSensGibbs <- as.data.frame(temp$chains$fSensGibbs)
  fSensGibbs$chain <- rep(i, length = niter)
  fSensGibbs$iter <- as.numeric(rownames(fSensGibbs))
  comb_fSensGibbs <- rbind(comb_fSensGibbs, fSensGibbs)
  
  # sgibbs
  sgibbs <- as.data.frame(temp$chains$sgibbs)
  sgibbs$chain <- rep(i, length = niter)
  sgibbs$iter <- as.numeric(rownames(sgibbs))
  comb_sgibbs <- rbind(comb_sgibbs, sgibbs)
}

# Remove burn in

# Specify your burn  in period
burnin <- 800

# Remove burn in
bFacGibbs <- comb_bFacGibbs |>
  dplyr::filter(iter > burnin)

bgibbs <- comb_bgibbs |>
  dplyr::filter(iter > burnin)

bgibbsUn <- comb_bgibbsUn |>
  dplyr::filter(iter > burnin)

fSensGibbs <- comb_fSensGibbs |>
  dplyr::filter(iter > burnin)

sgibbs <- comb_sgibbs |>
  dplyr::filter(iter > burnin)

# Now we're ready to plot our chains

bFacGibbs |>
  select(c(colnames(bFacGibbs)[1:20], iter, chain)) |>
  pivot_longer(colnames(bFacGibbs)[1:20],
               names_to = 'beta', values_to = 'estimate') |>
  ggplot(aes(x = iter, y = estimate, color = as.factor(chain))) +
  geom_line() +
  facet_wrap(~beta, scales = 'free') +
  theme(legend.position = 'none') +
  xlab('') + ylab('Estimate')

bFacGibbs |>
  select(c(colnames(bFacGibbs)[21:24], iter, chain)) |>
  pivot_longer(colnames(bFacGibbs)[21:24],
               names_to = 'beta', values_to = 'estimate') |>
  ggplot(aes(x = iter, y = estimate, color = as.factor(chain))) +
  geom_line() +
  facet_wrap(~beta, scales = 'free') +
  theme(legend.position = 'none') +
  xlab('') + ylab('Estimate')

bgibbs |>
  select(c(colnames(bgibbs)[1:20], iter, chain)) |>
  pivot_longer(colnames(bgibbs)[1:20],
               names_to = 'beta', values_to = 'estimate') |>
  ggplot(aes(x = iter, y = estimate, color = as.factor(chain))) +
  geom_line() +
  facet_wrap(~beta, scales = 'free') +
  theme(legend.position = 'none') +
  xlab('') + ylab('Estimate')

bgibbs |>
  select(c(colnames(bgibbs)[21:36], iter, chain)) |>
  pivot_longer(colnames(bgibbs)[21:36],
               names_to = 'beta', values_to = 'estimate') |>
  ggplot(aes(x = iter, y = estimate, color = as.factor(chain))) +
  geom_line() +
  facet_wrap(~beta, scales = 'free') +
  theme(legend.position = 'none') +
  xlab('') + ylab('Estimate ')

bgibbsUn |>
  select(c(colnames(bgibbsUn)[1:20], iter, chain)) |>
  pivot_longer(colnames(bgibbsUn)[1:20],
               names_to = 'beta', values_to = 'estimate') |>
  ggplot(aes(x = iter, y = estimate, color = as.factor(chain))) +
  geom_line() +
  facet_wrap(~beta, scales = 'free') +
  theme(legend.position = 'none') +
  xlab('') + ylab('Estimate')

bgibbsUn |>
  select(c(colnames(bgibbsUn)[21:36], iter, chain)) |>
  pivot_longer(colnames(bgibbsUn)[21:36],
               names_to = 'beta', values_to = 'estimate') |>
  ggplot(aes(x = iter, y = estimate, color = as.factor(chain))) +
  geom_line() +
  facet_wrap(~beta, scales = 'free') +
  theme(legend.position = 'none') +
  xlab('') + ylab('Estimate')

fSensGibbs |>
  pivot_longer(colnames(fSensGibbs)[1:2],
               names_to = 'beta', values_to = 'estimate') |>
  ggplot(aes(x = iter, y = estimate, color = as.factor(chain))) +
  geom_line() +
  facet_wrap(~beta, scales = 'free') +
  theme(legend.position = 'none') +
  xlab('') + ylab('Estimate')

sgibbs |>
  pivot_longer(colnames(sgibbs)[1:20],
               names_to = 'beta', values_to = 'estimate') |>
  ggplot(aes(x = iter, y = estimate, color = as.factor(chain))) +
  geom_line() +
  facet_wrap(~beta, scales = 'free') +
  theme(legend.position = 'none') +
  xlab('') + ylab('Estimate')

sgibbs |>
  pivot_longer(colnames(sgibbs)[21:40],
               names_to = 'beta', values_to = 'estimate') |>
  ggplot(aes(x = iter, y = estimate, color = as.factor(chain))) +
  geom_line() +
  facet_wrap(~beta, scales = 'free') +
  theme(legend.position = 'none') +
  xlab('') + ylab('Estimate')

sgibbs |>
  pivot_longer(colnames(sgibbs)[41:60],
               names_to = 'beta', values_to = 'estimate') |>
  ggplot(aes(x = iter, y = estimate, color = as.factor(chain))) +
  geom_line() +
  facet_wrap(~beta, scales = 'free') +
  theme(legend.position = 'none') +
  xlab('') + ylab('Estimate')

sgibbs |>
  pivot_longer(colnames(sgibbs)[61:78],
               names_to = 'beta', values_to = 'estimate') |>
  ggplot(aes(x = iter, y = estimate, color = as.factor(chain))) +
  geom_line() +
  facet_wrap(~beta, scales = 'free') +
  theme(legend.position = 'none') +
  xlab('') + ylab('Estimate')

#### Save

save(comb_out, bFacGibbs, bgibbs, bgibbsUn,
     fSensGibbs, sgibbs, file = 'FossilPollen/out/combined.RData')
