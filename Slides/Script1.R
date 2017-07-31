## Aula 7 - Script 1 -------------------------------------------------
## Author: Prof. Wagner Hugo Bonat LEG/UFPR --------------------------
## Date: 03/05/2017 --------------------------------------------------

# Incluindo a amostra
pop <- rnorm(40, mean = 1.70, sd = 0.10)
pop

# Colentando uma amostra
amostra <- pop[sample(1:40, 10)]

# Estimador pontual
mean(amostra)

# Distribuição amostral do estimador min + max / 2
M1 <- c()
for(i in 1:1000) {
  amostra <- pop[sample(1:40, 10)]
  M1[i] <- (min(amostra) + max(amostra))/2
}

# Distribuição amostral estimador média
M2 <- c()
for(i in 1:1000) {
  M2[i] <- mean(pop[sample(1:40, 10)])
}

# Distribuição amostral estimador primeira obs
M3 <- c()
for(i in 1:1000) {
  M3[i] <- pop[sample(1:40, 10)][1]
}

# Histogramas
par(mfrow = c(1,3))
hist(M1)
abline(v = 1.70, lwd = 2)
hist(M2)
abline(v = 1.70, lwd = 2)
hist(M3)
abline(v = 1.70, lwd = 2)

# Viés
mean(M1)
mean(M2)
mean(M3)

# Variância empírica
var(M1)
var(M2) # Mais eficiente
var(M3)

# Efeito do tamanho da amostra ---------------------------------------
n_amostra <- c(3,6,10,20)
M1 <- list("N3" = c(), "N6" = c(), "N10" = c(), "N20" = c())
M2 <- list("N3" = c(), "N6" = c(), "N10" = c(), "N20" = c())
M3 <- list("N3" = c(), "N6" = c(), "N10" = c(), "N20" = c())

# Estimador min + max /2
for(j in 1:4) {
  nn <- n_amostra[j]
  for(i in 1:1000) {
    amostra <- pop[sample(1:40, 10)]
    M1[[j]][i] <- (min(amostra) + max(amostra))/2
  }
}

# Estimador media
for(j in 1:4) {
  nn <- n_amostra[j]
  for(i in 1:1000) {
    M2[[j]][i] <- mean(pop[sample(1:40, nn)])
  }
}

# Estimador media
for(j in 1:4) {
  nn <- n_amostra[j]
  for(i in 1:1000) {
    M3[[j]][i] <- pop[sample(1:40, nn)][1]
  }
}

# Gráficos -----------------------------------------------------------
par(mfrow = c(3,4))
hist(M1[[1]], xlim = c(1.55, 1.90))
hist(M1[[2]], xlim = c(1.55, 1.90))
hist(M1[[3]], xlim = c(1.55, 1.90))
hist(M1[[4]], xlim = c(1.55, 1.90))

hist(M2[[1]], xlim = c(1.55, 1.90))
hist(M2[[2]], xlim = c(1.55, 1.90))
hist(M2[[3]], xlim = c(1.55, 1.90))
hist(M2[[4]], xlim = c(1.55, 1.90))

hist(M3[[1]], xlim = c(1.55, 1.90))
hist(M3[[2]], xlim = c(1.55, 1.90))
hist(M3[[3]], xlim = c(1.55, 1.90))
hist(M3[[4]], xlim = c(1.55, 1.90))

# Ilustração teorema central do limite -------------------------------
# Caso Uniforme
n_amostra <- c(10,30,50)
M_unif <- list("N10" = c(), "N30" = c(), "N50" = c())
for(j in 1:3) {
  nn <- n_amostra[j]
  for(i in 1:1000) {
    M_unif[[j]][i] <- mean(runif(nn))
  }
}

par(mfrow = c(4,3), mar=c(2.6, 2.8, 1.2, 0.5), mgp = c(1.6, 0.6, 0))
hist(M_unif[[1]])
hist(M_unif[[2]])
hist(M_unif[[3]])

M_binomial <- list("N10" = c(), "N30" = c(), "N50" = c())
for(j in 1:3) {
  nn <- n_amostra[j]
  for(i in 1:1000) {
    M_binomial[[j]][i] <- mean(rbinom(nn, size = 10, prob = 0.5))
  }
}
hist(M_binomial[[1]])
hist(M_binomial[[2]])
hist(M_binomial[[3]])

M_exp <- list("N10" = c(), "N30" = c(), "N50" = c())
for(j in 1:3) {
  nn <- n_amostra[j]
  for(i in 1:1000) {
    M_exp[[j]][i] <- mean(rexp(nn, rate =1))
  }
}
hist(M_exp[[1]])
hist(M_exp[[2]])
hist(M_exp[[3]])

M_pois <- list("N10" = c(), "N30" = c(), "N50" = c())
for(j in 1:3) {
  nn <- n_amostra[j]
  for(i in 1:1000) {
    M_pois[[j]][i] <- mean(rpois(nn, 5))
  }
}
hist(M_pois[[1]])
hist(M_pois[[2]])
hist(M_pois[[3]])


# Estimação pontual e intervalar -------------------------------------
# Caso binomial
amostra <- rbinom(10, prob = 0.5, size = 1)
amostra
sum(amostra)

dbinom(sum(amostra), size = 10, prob = 0.1)
dbinom(sum(amostra), size = 10, prob = 0.2)
dbinom(sum(amostra), size = 10, prob = 0.3)
dbinom(sum(amostra), size = 10, prob = 0.4)
dbinom(sum(amostra), size = 10, prob = 0.5)
dbinom(sum(amostra), size = 10, prob = 0.6)
dbinom(sum(amostra), size = 10, prob = 0.7)
dbinom(sum(amostra), size = 10, prob = 0.8)
dbinom(sum(amostra), size = 10, prob = 0.9)

p <- seq(0.01, 0.9, l = 10)
fp <- dbinom(sum(amostra), size = 10, prob = p)
plot(fp ~ p, type = "o", ylab = 'Prob', xlab = "p")

p <- seq(0.1, 0.9, l = 1000)
fp <- dbinom(sum(amostra), size = 10, prob = p)
plot(fp ~ p, type = "l", ylab = 'Prob', xlab = "p")
abline(v = sum(amostra)/10)

# Distribuição amostral do estimador proporção -----------------------
par(mfrow = c(1,3), mar=c(2.6, 2.8, 1.2, 0.5), mgp = c(1.6, 0.6, 0))
pp <- c()
for(i in 1:1000) {
  amostra <- rbinom(10, prob = 0.5, size = 1)
  pp[i] <- sum(amostra)/10
}

hist(pp, main = "n = 10", xlim = c(0,1))
abline(v = 0.5, lwd = 2, col = 2)


pp <- c()
for(i in 1:1000) {
  amostra <- rbinom(100, prob = 0.5, size = 1)
  pp[i] <- sum(amostra)/100
}

hist(pp, main = "n = 100", xlim = c(0, 1))
abline(v = 0.5, lwd = 2, col = 2)

pp <- c()
for(i in 1:1000) {
  amostra <- rbinom(1000, prob = 0.5, size = 1)
  pp[i] <- sum(amostra)/1000
}
var(pp)
hist(pp, main = "n = 1000", xlim = c(0, 1))
abline(v = 0.5, lwd = 1, col = 2)

# Estimação intervalar -----------------------------------------------
ic <- matrix(NA, ncol=2, nrow=100)
for(i in 1:100){
  amostra <- rbinom(100, prob = 0.5, size = 1)
  est <- sum(amostra)/100
  ic[i,] <- c(est - qnorm(0.975)*sqrt((est*(1-est))/100),
            est + qnorm(0.975)*sqrt((est*(1-est))/100))
}
mean(apply(ic, 1, function(x) (x[1] < 0.5 & x[2] > 0.5)))

plot(c(0.2,0.8)~c(1,100),type="n",ylab="p",xlab="Ensaio")
abline(h=0.5)
for(i in 1:100){
  arrows(c(i),ic[i,1],c(i),ic[i,2],code=3,angle=90,length=0.03,
         col=ifelse(ic[i,1] > 1 | ic[i,2] < 1, "darkred","lightgray"))
}
