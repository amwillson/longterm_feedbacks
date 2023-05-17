test_model <- "
model{
for(i in 1:nobs){
y[i,1:ntaxa] ~ ddirch(alpha[1:ntaxa])
}
alpha[1:ntaxa] ~ dmnorm(mu, Sigma)
}
"

nobs <- nrow(data_sub)
ntaxa <- ncol(data_sub)

data <- list(nobs = nobs,
             ntaxa = ntaxa,
             mu = rep(0.5, times = ntaxa),
             Sigma = diag(0.001,
                          nrow = ntaxa,
                          ncol = ntaxa))

jags.test_model <- jags.model(file = textConnection(test_model),
                              data = data,
                              n.chains = 3)

out.test_model <- coda.samples(model = jags.test_model,
                               variable.names = 'alpha',
                               n.iter = 1000)

plot(out.test_model)

test_model <- "
model{
for(i in 1:nobs){
y[i,1:ntaxa] ~ ddirch(alpha[1:ntaxa])
}
alpha[1:ntaxa] ~ dmnorm(mu, Sigma)
mu ~ dmnorm(mu_mu, Sigma_mu)
Sigma ~ dwish(R, k)
}
"

data <- list(nobs = nobs,
             ntaxa = ntaxa,
             y = data_sub,
             mu_mu = rep(0.5, times = ntaxa),
             Sigma_mu = diag(0.001,
                             nrow = ntaxa,
                             ncol = ntaxa),
             R = diag(0.001,
                      nrow = ntaxa,
                      ncol = ntaxa),
             k = ntaxa + 2)

jags.test_model <- jags.model(file = textConnection(test_model),
                              data = data,
                              n.chains = 3, n.adapt = 2000)

out.test_model <- coda.samples(model = jags.test_model,
                               variable.names = c('alpha', 'mu', 'Sigma'),
                               n.iter = 1000)

test_model <- "
model{
for(i in 1:nobs){
y[i,1:ntaxa] ~ ddirch(alpha[i,1:ntaxa])
alpha[i,1:ntaxa] ~ dmnorm(mu[i,1:ntaxa], Sigma)
for(j in 1:ntaxa){
logit(mu[i,j]) <- beta[1] + beta[match[1,j]] * x[i,1] + beta[match[2,j]] * x[i,2]
}
}
Sigma ~ dwish(R, k)
beta ~ dmnorm(mu_beta, Sigma_beta)
}
"

maxparam <- (2 * ntaxa) + 1
match <- seq(from = 2, to = maxparam, by = 1)
match <- matrix(match, nrow = 2, ncol = ntaxa)

data <- list(nobs = nobs,
             ntaxa = ntaxa,
             y = data_sub,
             match = match,
             x = clim_sub,
             R = diag(0.001,
                      nrow = ntaxa,
                      ncol = ntaxa),
             k = ntaxa + 4,
             mu_beta = rep(0, times = maxparam),
             Sigma_beta = diag(0.001,
                               nrow = maxparam,
                               ncol = maxparam))

jags.test_model <- jags.model(file = textConnection(test_model),
                              data = data,
                              n.chains = 3, n.adapt = 2000)

out.test_model <- coda.samples(model = jags.test_model,
                               variable.names = c('beta', 'Sigma'),
                               n.iter = 1000)
plot(out.test_model)
