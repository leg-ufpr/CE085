# Estimação por máxima verossimilhança: Distribuição Weibull ---------
# Professor: Wagner Hugo Bonat LEG/UFPR ------------------------------
# Data: 21/09/2018 ---------------------------------------------------

# Função densidade probabilidade
my_dweibull <- function(y, theta) {
  output = theta*(y^(theta-1))*exp(-(y^theta))
  return(output)
}

# Conferindo se é um fdp
integrate(my_dweibull, lower = 0.01, upper = Inf, theta = 15)

# Gráfico da fdp
curve(my_dweibull(x, theta = 1), 0, 3, ylim = c(0, 4))
curve(my_dweibull(x, theta = 5), 0, 3, add = TRUE)
curve(my_dweibull(x, theta = 10), 0, 3, add = TRUE)

# Simulando do modelo
set.seed(123)
y = rweibull(n = 100, shape = 5, scale = 1)

# Histograma
hist(y)

# Função de verossimilhança
# Opção 1
L1 <- function(theta, y) {
  output = prod(my_dweibull(y = y, theta = theta))
  return(output)
}

# Opção 2
L2 <- function(theta, y) {
  n <- length(y)
  output = (theta^n )*prod(y^(theta-1))*exp(-sum(y^theta))
  return(output)
}
L2 <- Vectorize(L2, vectorize.args = "theta")

# Avaliando no ponto theta = 5
L1(theta = 5, y = y)
L2(theta = 5, y = y)

# Gráfico da verossimilhança
curve(L2(x, y = y), 2, 7)

# Função de log-verossimilhança
ll <- function(theta, y) {
  n <- length(y)
  output = n*log(theta) + (theta - 1)*sum(log(y)) - sum(y^theta)
  return(output)
}
ll <- Vectorize(ll, vectorize.args = "theta")

# Gráfico da log-vero
curve(ll(x, y = y), 2, 7)

# Para obter o EMV precisamos maximizar ll com relação a theta.

# Opção 1: Usar um maximizador numérico
EMV <- optim(par = 4, fn = ll, y = y, control = list(fnscale = -1), 
             method = "Brent", lower = 1, upper = 10, hessian = TRUE)

# Opção 2: Resolver a função escore

# Função escore
U_theta <- function(theta, y) {
  n <- length(y)
  output = n/theta + sum(log(y)) - sum(y^theta * log(y))
  return(output)
}
U_theta <- Vectorize(U_theta, vectorize.args = "theta")
# Conferindo numericamente
require(numDeriv)
U_theta(theta = EMV$par, y = y)
grad(func = ll, x = EMV$par, y = y)

# Gráfico da função escore
curve(U_theta(x, y = y), 3, 7)
abline(h = 0)
abline(v = EMV$par)

# Obtendo o EMV resolvendo a função escore (não linear em theta)
# Precisamos de métodos numéricos !!

# Usando função pronta do R multiroot
require(rootSolve)
sol <- multiroot(U_theta, start = 4, y = y)
sol

# Obtendo a variancia assintotica
Io <- function(theta, y) {
  n <- length(y)
  output = n/(theta^2) + sum( (y^theta)* (log(y)^2) )
  return(output)
}

Io(theta = EMV$par, y = y)
EMV$hessian # Só faltou trocar o sinal ;)
-EMV$hessian

# Então theta_hat ~ N(0, Io^-1)
solve(-EMV$hessian) # Variancia assintótica do EMV

# Propriedades assintóticas do EMV
EMV2 <- c()
Iobs <- c()
for(i in 1:1000) {
  # Simula
  y = rweibull(n = 100, shape = 5, scale = 1)
  # Estima
  EMV <- optim(par = 4, fn = ll, y = y, control = list(fnscale = -1), 
               method = "Brent", lower = 1, upper = 10, hessian = TRUE)
  # Guarda
  EMV2[i] <- EMV$par
  Iobs[i] <- Io(theta = EMV$par, y = y)
}

# Distribuição empirica
par(mfrow = c(1,2))
hist(EMV2, prob = TRUE, ylim = c(0, 1.2))

# Distribuição teórica via TCL
Ie <- mean(1/Iobs)
curve(dnorm(x, mean = 5, sd = sqrt(Ie)), add = TRUE)

# Repetindo com amostra tamanho 10000
EMV2 <- c()
Iobs <- c()
for(i in 1:1000) {
  # Simula
  y = rweibull(n = 10000, shape = 5, scale = 1)
  # Estima
  EMV <- optim(par = 4, fn = ll, y = y, control = list(fnscale = -1), 
               method = "Brent", lower = 1, upper = 10, hessian = TRUE)
  # Guarda
  EMV2[i] <- EMV$par
  Iobs[i] <- Io(theta = EMV$par, y = y)
}

# Distribuição empirica
hist(EMV2, prob = TRUE, ylim = c(0, 12))

# Distribuição teórica via TCL
Ie <- mean(1/Iobs)
curve(dnorm(x, mean = 5, sd = sqrt(Ie)), add = TRUE)

# Taxa de cobertura o IC
EMV2 <- c()
Iobs <- c()
Ic_min <- c()
Ic_max <- c()
for(i in 1:100) {
  # Simula
  y = rweibull(n = 100, shape = 5, scale = 1)
  # Estima
  EMV <- optim(par = 4, fn = ll, y = y, control = list(fnscale = -1), 
               method = "Brent", lower = 1, upper = 10, hessian = TRUE)
  # Guarda
  EMV2[i] <- EMV$par
  Iobs[i] <- solve(-EMV$hessian)
  Ic_min[i] <- EMV$par - qnorm(0.975)*sqrt(Iobs[i])
  Ic_max[i] <- EMV$par + qnorm(0.975)*sqrt(Iobs[i])
}

results = data.frame(EMV2, Iobs, Ic_min, Ic_max)

# Taxa de cobertura empirica
tx_emp <- (dim(results[which(results$Ic_max < 5),])[1] + 
             dim(results[which(results$Ic_min > 5),])[2])/100
tx_emp

ic = results[,c(3,4)]
plot(c(3,7)~c(1,100),type="n",ylab=expression(theta),xlab="Ensaio")
abline(h=5)
for(i in 1:100){
  arrows(c(i),ic[i,1],c(i),ic[i,2],code=3,angle=90,length=0.03,
         col=ifelse(ic[i,1] > 5 | ic[i,2] < 5, "darkred","blue"),
         lty=ifelse(ic[i,1] > 5 | ic[i,2] < 5, 1, 2) )
}
# FIM ----------------------------------------------------------------

