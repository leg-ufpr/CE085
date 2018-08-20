# Ilustração computacional Lei do Grandes Números --------------------

######################################################################
## Lei dos Grandes Números Chebyshev
######################################################################

######################################################################
# Exemplo 1: Y ~ N(mu = 10, sigma2 = 1) ------------------------------
# E(Y) = mu e Var(Y) = 1 ---------------------------------------------
######################################################################
par(mfrow = c(2,3), mar=c(2.6, 2.8, 1.2, 0.5), mgp = c(1.6, 0.6, 0))

# Uma realização de tamanho 100 da sequência hat(\mu)
media <- c()
for(i in 1:100) {
  media[i] <- mean(rnorm(i, mean = 10, sd = 1))
}
plot(media ~ c(1:100), type = "l", xlab = "n", main = "Gaussian",
     ylab = expression(Y[n]) )
abline(h = 10)

# Uma realização de tamanho 1000 da sequência hat(\mu)
media <- c()
for(i in 1:1000) {
  media[i] <- mean(rnorm(i, mean = 10, sd = 1))
}
plot(media ~ c(1:1000), type = "l", xlab = "n", main = "Gaussian",
     ylab = expression(Y[n]) )
abline(h = 10)

# Uma realização de tamanho 10000 da sequência hat(\mu)
media <- c()
for(i in 1:10000) {
  media[i] <- mean(rnorm(i, mean = 10, sd = 1))
}
plot(media ~ c(1:10000), type = "l", xlab = "n", main = "Gaussian",
     ylab = expression(Y[n]) )
abline(h = 10)

######################################################################
# Exemplo 2: Y ~ Cauchy(loc = 10, scale = 1) -------------------------
# E(Y) = não definida e Var(Y) = não definida ------------------------
######################################################################

# Uma realização de tamanho 100 da sequência hat(\mu)
media <- c()
for(i in 1:100) {
  media[i] <- mean(rcauchy(i, loc = 10, scale = 1))
}
plot(media ~ c(1:100), type = "l", xlab = "n", main = "Cauchy",
     ylab = expression(Y[n]) )
abline(h = 10)

# Uma realização de tamanho 1000 da sequência hat(\mu)
media <- c()
for(i in 1:1000) {
  media[i] <- mean(rcauchy(i, loc = 10, scale = 1))
}
plot(media ~ c(1:1000), type = "l", xlab = "n", main = "Cauchy",
     ylab = expression(Y[n]) )
abline(h = 10)

# Uma realização de tamanho 10000 da sequência hat(\mu)
media <- c()
for(i in 1:10000) {
  media[i] <- mean(rcauchy(i, loc = 10, scale = 1))
}
plot(media ~ c(1:10000), type = "l", xlab = "n", main = "Cauchy",
     ylab = expression(Y[n]) )
abline(h = 10)

######################################################################
## Lei dos Grandes Números de Kolmogorov
######################################################################

######################################################################
# t-Student 2 graus de liberdade Y ~ t(df = 2) -----------------------
# E(Y) = 0 e Var(Y) = inf --------------------------------------------
######################################################################

# Uma realização de tamanho 100 da sequência hat(\mu)
media <- c()
for(i in 1:100) {
  media[i] <- mean(rt(i, df = 2))
}
plot(media ~ c(1:100), type = "l", xlab = "n", main = "t-Student df = 2",
     ylab = expression(Y[n]) )
abline(h = 0)

# Uma realização de tamanho 1000 da sequência hat(\mu)
media <- c()
for(i in 1:1000) {
  media[i] <- mean(rt(i, df = 2))
}
plot(media ~ c(1:1000), type = "l", xlab = "n", main = "t-Student df = 2",
     ylab = expression(Y[n]) )
abline(h = 0)

# Uma realização de tamanho 10000 da sequência hat(\mu)
media <- c()
for(i in 1:10000) {
  media[i] <- mean(rt(i, df = 2))
}
plot(media ~ c(1:10000), type = "l", xlab = "n", main = "t-Student df = 2",
     ylab = expression(Y[n]) )
abline(h = 0)

######################################################################
# t-Student 1 grau de liberdade Y ~ t(df = 1) ------------------------
# E(Y) = não definida e Var(Y) = não definida ------------------------
######################################################################

# Uma realização de tamanho 100 da sequência hat(\mu)
media <- c()
for(i in 1:100) {
  media[i] <- mean(rt(i, df = 1))
}
plot(media ~ c(1:100), type = "l", xlab = "n", main = "t-Student df = 1",
     ylab = expression(Y[n]) )
abline(h = 0)

# Uma realização de tamanho 1000 da sequência hat(\mu)
media <- c()
for(i in 1:1000) {
  media[i] <- mean(rt(i, df = 1))
}
plot(media ~ c(1:1000), type = "l", xlab = "n", main = "t-Student df = 1",
     ylab = expression(Y[n]) )
abline(h = 0)

# Uma realização de tamanho 10000 da sequência hat(\mu)
media <- c()
for(i in 1:10000) {
  media[i] <- mean(rt(i, df = 1))
}
plot(media ~ c(1:10000), type = "l", xlab = "n", main = "t-Student df = 1",
     ylab = expression(Y[n]) )
abline(h = 0)

######################################################################
# Lei dos Grandes Números Markov 
######################################################################

# Seja Y_i ~ N(mu = 10, sigma = 1) e X_i ~ Poisson(lambda = 5). 
# E(Y_i) = 10 e Var(Y_i) = 1. E(X_i) = 5 e Var(X_i) = 5.

Gauss <- c()
Poisson <- c()
for(i in 1:100) {
  Gauss[i] <- mean(rnorm(i, mean = 10, sd = 1))
  Poisson[i] <- mean(rpois(i, lambda = 5))
}

for(i in 1:100) {
  Gauss[i] <- mean(rnorm(i, mean = 10, sd = 1))
  Poisson[i] <- mean(rpois(i, lambda = 5))
}

Gauss2 <- c()
Poisson2 <- c()
for(i in 1:10000) {
  Gauss2[i] <- mean(rnorm(i, mean = 10, sd = 1))
  Poisson2[i] <- mean(rpois(i, lambda = 5))
}

par(mfrow = c(2,3), mar=c(2.6, 2.8, 1.2, 0.5), mgp = c(1.6, 0.6, 0))
plot(Gauss ~ c(1:100), type = "l", xlab = "n", main = "Gaussian",
     ylab = expression(Y[n]) )
abline(h = 10)
plot(Poisson ~ c(1:100), type = "l", xlab = "n", main = "Poisson",
     ylab = expression(Y[n]) )
abline(h = 5)
plot((Gauss+Poisson)/2 ~ c(1:100), type = "l", xlab = "n", 
     main = "Gaussian e Poisson", ylab = expression(Y[n]) )
abline(h = 7.5)


plot(Gauss2 ~ c(1:10000), type = "l", xlab = "n", main = "Gaussian",
     ylab = expression(Y[n]) )
abline(h = 10)
plot(Poisson2 ~ c(1:10000), type = "l", xlab = "n", main = "Poisson",
     ylab = expression(Y[n]) )
abline(h = 5)
plot((Gauss2+Poisson2)/2 ~ c(1:10000), type = "l", xlab = "n", 
     main = "Gaussian e Poisson", ylab = expression(Y[n]) )
abline(h = 7.5)
