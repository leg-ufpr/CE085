## Script 1 - Estatística Inferencial --------------------------------
## Prof. Wagner Hugo Bonat LEG/UFPR ----------------------------------
## Data: 06/08/2018 --------------------------------------------------

# Carregando os dados
y <- c(rep(1, 17), rep(0, 8))[sample(1:25, 25)]
y
# Proporção de alunos que trabalham
mean(y)

# Amostrando 5 alunos aleatoriamente
y_amostra <- y[sample(1:25, size = 5)]
y_amostra

# Estimando/Aprendendo o valor do parâmetro através da amostra -------

# Função de verossimilhança
L_p <- function(p, y) {
  out <- prod(p^y * (1-p)^(1-y))
  return(out)
}

# Função de log-verossimilhança
l_p <- function(p, y) {
  out <- sum(y)*log(p) + sum(1 - y)*log(1-p)
  return(out)
}

# Função escore
U_p <- function(p, y) {
  out <- sum(y/p) - sum( (1-y))/(1-p)
  return(out)
}

# Gráficos -----------------------------------------------------------
p_grid <- seq(0, 1, l = 100)
Lik <- c()
LogLik <- c()
escore <- c()
for(i in 1:100) {
  Lik[i] <- L_p(p = p_grid[i], y = y_amostra)
  LogLik[i] <- l_p(p = p_grid[i], y = y_amostra)
  escore[i] <- U_p(p = p_grid[i], y = y_amostra)
}

par(mfrow = c(1,3), mar=c(2.6, 2.8, 1.2, 0.5), mgp = c(1.6, 0.6, 0))
plot(Lik ~ p_grid, main = "Verossimilhança", type = "l")
abline(v = mean(y_amostra))
plot(LogLik ~ p_grid, main = "Log-Verossimilhança", type = "l")
abline(v = mean(y_amostra))
plot(escore ~ p_grid, main = "Escore", type = "l")
abline(h = 0)
abline(v = mean(y_amostra))

# Repetindo o procedimento "várias vezes" e guardando o resultado ----
prop_amostra <- c()
for(i in 1:1000) {
  prop_amostra[i] <- mean(y[sample(1:25, size = 5)])
}

## Distribuição empírica
hist(prop_amostra, prob = TRUE)

# Repetindo o procedimento "várias vezes" e guardando o resultado
# Amostra tamanho 10
prop_amostra <- c()
for(i in 1:1000) {
  prop_amostra[i] <- mean(y[sample(1:25, size = 10, 
                                   replace = TRUE)])
}

## Distribuição empírica
hist(prop_amostra, prob = TRUE)

## Tentando imitar com a amostra 
# Amostra tamanho 5
bootstrap <- c()
for(i in 1:1000) {
  bootstrap[i] <- mean(y_amostra[sample(1:5, size = 5, replace = TRUE)])
}
hist(bootstrap, prob = TRUE)

# Amostra tamanho 10
y_10 <- y[sample(1:25, size = 10, replace = FALSE)]
bootstrap <- c()
for(i in 1:1000) {
  bootstrap[i] <- mean(y_10[sample(1:10, size = 10, replace = TRUE)])
}
hist(bootstrap, prob = TRUE)

# Medindo a incerteza ------------------------------------------------
l_p <- Vectorize(FUN = l_p, vectorize.args = "p")
y_amostra <- y[sample(1:25, size = 10)]
pp <- c()
plot(l_p(p = p_grid, y = y_amostra) ~ p_grid, ylim = c(-20,0), type = 'l')
for(i in 1:50) {
  y_amostra <- y[sample(1:25, size = 10)]
  lines(p_grid, l_p(p = p_grid, y = y_amostra), 
        col = i, lty = i, type = 'l')
  pp[i] <- mean(y_amostra)
  abline(v = mean(y_amostra), col = i, lty = i)
  Sys.sleep(0.25)
}

hist(pp)

## Vamos replicar a ideia com uma população maior !!!
y <- rbinom(1000, size = 1, prob = 17/25)
y_amostra <- y[sample(1:1000, size = 30)]
pp <- c()
plot(l_p(p = p_grid, y = y_amostra) ~ p_grid, ylim = c(-50,-10), type = 'l')
for(i in 1:50) {
  y_amostra <- y[sample(1:1000, size = 30)]
  lines(p_grid, l_p(p = p_grid, y = y_amostra), 
        col = i, lty = i, type = 'l')
  pp[i] <- mean(y_amostra)
  abline(v = mean(y_amostra), col = i, lty = i)
  Sys.sleep(0.25)
}
abline(v = 0.68, lwd = 2)
hist(pp)
mean(pp)
var(pp)