library(readxl)

# 1) H0 p <= 0.5, H1 p > 0.5 - nilateral a direita 
# 2) Colocar o nível de significância alpha = 0.05 
# 3) Estatística do teste Qui-quadrado - O1 = 45-12,O2 = 12, E1 = E2 = 45 * 0.5 

Ei <- c(45*0.5, 45*0.5)
Oi <- c(45-12,12)
sum((Oi - Ei)^2/Ei)

# 4) Regra de decisão Unilateral a direita \alpha = 0.05, g.l = 2-1 = 1
# RC: Região Crítica, Região de Rejeição H_0, RNR: Região de Não Rejeição H_0

# Regra de decisão  Qui-quadrado = 3.84, Qui-quadrado observado = 9.8
qchisq(0.95, df=1) 
1 - pchisq(9.8,df=1)

# 5) Conclusão: Com nível de significância \alpha = 0.05, rejeitamos H_0, ou seja, 
# a proporção de melhora dos cães com a musicoterapia é maior de 0,5. O valor de 
# p = 0.0017. 

prop.test(45-12,45,correct=FALSE)

# Intervalo de confiança [0.5896;0.8403]


# Slide 13 
# X: Comprimento de peixes 
# mu: média do comprimentos dos peixes  - Parâmetro populacional 
# sigma : desvio-padrão do comprimento dos peixes - Parâmetro Populacional 
# Suposição de Normalidade  X \sim N(mu,sigma²)

# Testes de Hipótese
# 1) H0: mu >= 25 , H1: mu < 25 
# 2) Nível de significância alpha = 0.05 
# 3) Estatística do teste (Distribuição t de student)

dados <- read_excel("Documentos/Disciplinas/Análise no R/Aula 5/05_TH1_dados.xlsx")
attach(dados)
summary(comprimento)

# Histograma do comprimento dos peixes
hist(comprimento, main = "Histograma do comprimento dos peixes",
     xlab ="", ylab = "cm")

# Boxplot do comprimento dos peixes 
boxplot(comprimento, main = "Boxplot do comprimento dos peixes",
        xlab = "",ylab = "cm")

# Tamanho da amostra 
n     <- length(comprimento)
media <- mean(comprimento) 
dpa   <- sd(comprimento)

# Estatística do teste observado = -4.04 (Estatística do teste)
t_obs  <- (media - 25)*sqrt(n)/dpa

# 4) Regra de decisão - unilateral a esquerda
# alpha = 0.05, t de student g.l = n-1 = 15-1 = 14
qt(0.05,14) # = -1.76
# Se a estatística do teste pertence a RC, rejeitamos H0
# Se a estatística do teste não pertence a RC, não rejeitamos H0

pt(-4.04,14) # Valor de p, p-value

# 5) Conclusão: Com nível de significância de 0.05, rejeita-se H0, ou seja, 
# o comprimento médio dos peixes é menor do que 25 cm. 

t.test(comprimento, mu = 25, alternative = "less")

# Slide 18
# H0: o comprimento dos peixes segue Normal mu = 25, sigma = 1 
# H1: o comprimento dos peixes não segue Normal mu = 25, sigma = 1 
# 2) Nível de significância alpha = 0.05 
# 3) Estatística do teste: D = 0.48, p-value = 0.0018 < 0.05 

# Teste Kolmogorov-Smirnov 
ks.test(comprimento,"pnorm",25,1)

# 4) Conclusão: Com nível de significância 0.05, rejeita H0, ou seja, o comprimento dos peixes
# não segue distribuição norma mu = 25 e sigma = 1

# Teste de lilliefors para normalidade
library(nortest)
lillie.test(comprimento)

# H0: comprimento segue distribuição normal 
# H1: comprimento não segue distribuição normal
# 2) Nível de significância alpha = 0.05  
# Estatística do teste: D = 0.10632, p-value = 0.9196

# Conclusão: Com nível de significância de 0.05 não rejeitamos H0, ou seja, o comprimento
# dos peixes segue a distribuição normal 

# Teste de Anderson-Darling
ad.test(comprimento)
# Teste de Cramer-von Mises
cvm.test(comprimento)
# Teste de Shapiro-Wilk
shapiro.test(comprimento)

# Fazendo o qqplot
qqnorm(comprimento)
qqline(comprimento)

# Banda de confiança (Depois instalar)
#install.packages("car", dependencies = TRUE)
#library(car)                
#qqplot(comprimento, dist="norm",envelope=0.95)

## Pacote acima não da para instalar


library(ggplot2)
ggplot(data = dados, aes(sample = comprimento)) +
  stat_qq() +
  stat_qq_line(col = "red") +
  theme_minimal()

# Teste de Wilcoxon (Não paramétrico) 
# Posto = posição que ocupa no banco de dados
data.frame(dados, posto = rank(comprimento),
           dif = comprimento-25)

# H0: comprimento mediano >= 25 cm
# H1: comprimento mediano  < 25 cm
# 2) Nível de significância alpha = 0.05 
# 3) Estatística do teste: 
wilcox.test(comprimento,mu=25,alternative = "l")

# 4) Conclusão: Com n.s de 0.05, rejeita H0, ou seja,o comprimento mediano dos peixes
# é menor do que 25 cm.

# Cálculo do tamanho da amostra 
library(pwr)

piloto <- c(24.6,25.4,23.9,24.5,24.3)
media  <- mean(piloto)
dpa    <- sd(piloto)
efeito <- (media-25)/dpa
pwr.t.test(d=efeito, power = 0.80, type="one.sample",alternative="less")

# Tamanho do efeito = -0.83
# Nível de significâcnia alpha = 0.05 
# Poder = 0.80 unilateral a esquerda ,  n = 10.35 approx 11 peixes 

# Tamanho da amostra: uma proporção
# Amostra piloto de 75%, po = 0.50, Unilateral a direita 
# Nivél de significância alpha = 0.05 e poder = 80% 

(efeito2 <- ES.h(p1 = 0.75,p2=0.50))

# Tamanho do efeito = 0.5236 
pwr.p.test(h=efeito2,sig.level = 0.05,power = 0.80,alternative = "greater")

# Tamanho da amostra = n = 22.55 = 23 cães 























































































