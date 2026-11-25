
# Dados do lucro + probabilidades
lucro <- c(15,17,19,21)
prob  <- c(0.0911,0.3341,0.4084,0.1664)

# Somando as probabilidades de lucro
sum(prob)

# Calculando o lucro esperado
mu <- lucro*prob
sum(mu)

# Variância e Desvio padrão
lucro2 <- lucro^2
lucro2b <- lucro2*prob
VarX <- sum(lucro2b) - sum(mu)^2
Desv <- sqrt(VarX)

# Tabela para verificar os valores 
tabela <- data.frame(lucro, prob, lucro2, lucro2b)
tabela

# Distribuição Binomial 
help(dbinom)

#----------- Distribuição Binomial ------------------#

# P(X = 0),P(X = 1),P(X = 2) e  P(X = 3)
dbinom(0,size=3,prob = 0.8)
dbinom(1,size=3,prob = 0.8)
dbinom(2,size=3,prob = 0.8)
dbinom(3,size=3,prob = 0.8)

# Distribuição Binomial usando vetor
dbinom(c(0,1,2,3),size=3,prob = 0.8)
sum(dbinom(c(0,1,2,3),size=3,prob = 0.8))
dbinom(c(0,1,2,3,0,1,2,3),3,c(rep(0.8,4),rep(0.2,4)))

# Gráfico da distribuição binomial
k <- 0:3
probk <- dbinom(0:3,3,0.8)
data.frame(k,probk)
plot(k,probk,type="h", 
     main = "Distribuição Binomial  (n = 3,p = 0.8)",
     ylab = "Probabilidade",
     lwd = 3)


#----------- Distribuição Multinomial ------------------#

# 3 vermelhas, 1 branca e 1 azul 
dmultinom(c(3,1,1), prob = c(0.5,0.3,0.2))
# 2 vermelhas, 3 branca e 0 azul
dmultinom(c(2,3,0), prob = c(0.5,0.3,0.2))

#----------- Distribuição de Poisson ----------------#
dpois(x = 4, lambda = 3)

# Soma das probabilidade
sum(dpois(x = 0:2,lambda=3))

# Probabilidade acumulada
ppois(q=2,lambda=3)

# Pelo menos 3 visitas --- P(X >=3) = 1-P(X<=2)
1-ppois(2,lambda=3)
ppois(2,lambda=3,lower.tail = F)

k <- 0:15
plot(k,dpois(k,lambda = 3),
     type="h",
     main = "Distribuição de Poisson",
     ylab = "Probabilidade",
     xlab = "Quantidade de Distribuição",
     lwd = 3.5)

k <- 0:60
plot(k,dpois(k,lambda = 30),
     type="h",
     main = "Distribuição de Poisson lambda=30",
     ylab = "Probabilidade",
     xlab = "Quantidade de Distribuição",
     lwd = 3.5)

#----------- Distribuição Normal --------------#
mu <- 25.5
sigma <- 4.7
pnorm(23,mean = mu,sd = sigma)

# Padronização (Z-score)
pnorm((23-25.5)/4.7)
pnorm((29-25.5)/4.7)


# Diâmetro entre 22 e 29 cm
a <- pnorm(22,mean = mu, sd = sigma)
b <- pnorm(29,mean = mu, sd = sigma)

# Calculando a probabilidade da área
b - a

# Diâmetro maior do que 27 cm
1 - pnorm(27,mean = mu, sd = sigma)
pnorm(27,mean = mu, sd = sigma, lower.tail = F)

# ------------ Quantil da normal ----------- #

# -------- Limite dos 25% menores -------- # 
qnorm(0.25, mean = mu ,sd = sigma)

# -------- Limite dos 10% maiores -------- # 
qnorm(0.10, mean = mu, sd = sigma, lower.tail = F)
qnorm(0.90, mean = mu, sd = sigma)


# ------------ Distribuição Qui-Quadrado ----------- #
pchisq(15,df=5) # P(X>15)
1 - pchisq(15,df=5)


# Limite dos 5% maiores
qchisq(0.95,df=5)


# ------------ Distribuição t de student ----------- #
pt(2,10)   # P(X <=2)
1-pt(2,10) # P(X > 2)

# Limites de 2.5% e 10% maiores 
qt(0.975,10) # Limites dos 2.5% maiores
qt(0.900,10) # Limites dos 10% maiores 

# ------------ Distribuição F de Snedecor ----------- #

### --------------- Quantil 97.5% ---------------- ###
qf(0.975,5,6)

###--------------- Probabilidade P(X<6.5) ----------------###
pf(6.5,5,6)

###--------------- Probabilidade P(X<6.5) ----------------###
qf(c(0.95,0.90),5,6)



