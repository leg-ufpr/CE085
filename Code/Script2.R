# Script 2 : Analisando experimento "tamanho do pé" ------------------
# Prof. Wagner Hugo Bonat LEG/UFPR -----------------------------------
# Data: 17/08/2018 ---------------------------------------------------
rm(list=ls())

# Carregando pacotes extra
require(ggplot2)

# Carregando a base de dados
dados <- read.table("dados_pe.csv", header = TRUE, sep = ",")

# Histograma
hist(dados$Pe)

# Histograma por sexo
par(mfrow = c(1,2))
hist(subset(dados, dados$Sexo == "F")$Pe, main = "Feminino")
hist(subset(dados, dados$Sexo == "M")$Pe, main = "Masculino")

# Estimadores pontuais para média e variancia pr sexo
Media <- tapply(dados$Pe, as.factor(dados$Sexo), mean)
Variancia <- tapply(dados$Pe, as.factor(dados$Sexo), var)

Media
Variancia

# Densidades estimadas -----------------------------------------------
par(mfrow = c(1,2))
hist(subset(dados, dados$Sexo == "F")$Pe, prob = TRUE, 
     main = "Feminina", xlim = c(30, 45), xlab = "Tamanho do pé")
curve(dnorm(x, mean = Media[1], sd = sqrt(Variancia[1]) ), 
      from = 30,  to = 45, add = TRUE)

hist(subset(dados, dados$Sexo == "M")$Pe, prob = TRUE,
     ylim = c(0, 0.25), main = "Masculino", xlim = c(30, 50), 
     xlab = "Tamanho do pé")
curve(dnorm(x, mean = Media[2], sd = sqrt(Variancia[2]) ), 
      from = 30,  to = 50, add = TRUE)


pnorm(32, mean = Media[1], sd = sqrt(Variancia[1]))*510000

# Calculando probabilidades
pe_F <- c(30:50)
prob_F <- pnorm(pe_F, mean = Media[1], sd = sqrt(Variancia[1]))
prob_F <- diff(prob_F)
res_F <- data.frame("Pe" = pe_F[-21], "Prob" = prob_F, 
                    "N Sapato" = round(prob_F*510000,0))
res_F
pe_M <- c(30:50)
prob_M <- pnorm(pe_M, mean = Media[2], sd = sqrt(Variancia[2]))
prob_M <- diff(prob_M)
res_M <- data.frame("Pe" = pe_M[-21], "Prob" = prob_M, 
                    "N Sapato" = round(prob_M*490000,0) )
res_M
par(mfrow = c(1,2))
barplot(res_F$N.Sapato, main = "Feminino")
barplot(res_M$N.Sapato, main = "Masculino")

# Intervalo de confiança para media e variancia amostral -------------
n_amostra <- tapply(dados$Pe, as.factor(dados$Sexo), length)
var_media <- Variancia/n_amostra
var_var <- Variancia/(2*n_amostra)

IC_media_F <- Media[1] + c(-1,1)*qnorm(0.975)*sqrt(var_media[1])
IC_media_M <- Media[2] + c(-1,1)*qnorm(0.975)*sqrt(var_media[2])

IC_var_F <- Variancia[1] + c(-1,1)*qnorm(0.975)*sqrt(var_var[1])
IC_var_M <- Variancia[2] + c(-1,1)*qnorm(0.975)*sqrt(var_var[2])

list("IC_F" = IC_media_F, "IC_M" = IC_media_M)
list("IC_F" = IC_var_F, "IC_M" = IC_var_M)

# Intervalo de confiança para as probabilidades ----------------------

# Reamostragem: Usa só os dados observados
amostra_M <- subset(dados, dados$Sexo == "M")$Pe
amostra_F <- subset(dados, dados$Sexo == "F")$Pe

media_M <- c()
var_M <- c()
N_M <- matrix(NA, ncol = c(length(pe_M)-1), nrow = 1000)

for(i in 1:1000) {
  y_temp <- amostra_M[sample(1:length(amostra_M), rep = TRUE)]
  media_M[i] <- mean(y_temp)
  var_M[i] <- var(y_temp)
  prob_M <- pnorm(pe_M, mean = media_M[i], sd = sqrt(var_M[i]))
  N_M[i,] <- round(diff(prob_M)*490000,0)
}

par(mfrow = c(4,5), mar=c(2.6, 2.8, 1.2, 0.5), mgp = c(1.6, 0.6, 0))
for(i in 1:20) {
  hist(N_M[,i], main = paste("Tam", pe_M[i]))
}
cbind(colMeans(N_M), res_M$N.Sapato)

# Distribuição empirica da media amostral
hist(media_M, prob = TRUE)

# Distribuição teórica assintótica da média amostral
curve(dnorm(x, mean = Media[2], sd = sqrt(Variancia[2]/20) ), 
      39, 42, add = TRUE)

hist(var_M, prob = TRUE, ylim = c(0, 1.5))
# Reproduzirem para variância!
curve(dnorm(x, mean = Variancia[2], sd = sqrt(Variancia[2]/40) ), 
      0, 6, add = TRUE)

