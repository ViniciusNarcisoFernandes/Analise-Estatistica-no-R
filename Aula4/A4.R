############################################################
      ########### Intervalo de Confiança #############
############################################################
library(readxl)

# Tamanho dos peixes 
dadosT <- read_excel("Documentos/Disciplinas/Análise no R/Aula 4/ExemplosIC.xlsx", 
                         sheet = "Tamanho")
head(dadosT)
hist(dadosT$tamanho,ylab="Frequência",
     main="Histograma da população dos peixes do rio",
     xlab = "Tamanho")

# Quantidade de peixes - Tamanho da população 
N      <- length(dadosT$tamanho)

# Média Populacional
mu     <- mean(dadosT$tamanho)
mu

# Variância populacional
sigma2 <- sum((dadosT$tamanho - mu)^2)/N
sigma2

# Desvio-padrão Populacional
sigma3 <- sqrt(sigma2)
sigma3

# Cálculo dos Quantis 
quantile(dadosT$tamanho,c(0.025,0.975))

# ------------------------------------------------- #
#                Distribuição Amostral              #
# ------------------------------------------------- #
# Sequência de 1 a 10
set.seed(123)
sample(1:10)

# Amostra de tamanho de 5 com reposição
set.seed(123)
sample(1:10,5,replace=T)

# Amostra de tamanho 5 dos peixes do rio (Com reposição)
n    <- 100               # Amostra de tamanho 1, 10, 50, 100
Nsim <- 1000            # Número de simulações 
set.seed(123)           # Semente aleatória 
mediasim <- c()         # Vetor de médias simuladas 
for (i in 1:Nsim){
mediasim[i] <- mean(sample(dadosT$tamanho, size = n, replace = TRUE))
}

hist(mediasim,xlab="Tamanho",
     ylab ="Frequência",
     main="Distribuição amostral da média")

mean(mediasim)
sigma3/sqrt(n)
quantile(mediasim,c(0.025,0.975))

# Tamanho da população N = 500, Sem reposição 
# IC para o tamnaho médio com n = 50
# Amostragem sem reposição e distribuição normal
n1 = 50 
sigmap <- sigma3/sqrt(n1)*sqrt((N-n1)/(N-1))
sigmap # Erro padrão  

# -------------------------------------------------------- #

# -------------------------------------------------------- #
set.seed(2612)
amostra <- sample(dadosT$tamanho, 50, replace = T)
media <- mean(amostra)
media
dpa <- sd(amostra)
dpa
qnorm(c(0.025,0.975),media,dpa)

# intervalo de confiança para a média com reposição 
# Desvio-padrão desconhecico # Distribuição de t de student 
t.test(amostra)

# X: variável de comprimento de um peixe
# mu: comprimento médio dos peixes - IC para mu de 95%
# sigma: desvio-padrão do comprimento dos peixes 

# Slide - 15

dados2 <- read_excel("Documentos/Disciplinas/Análise no R/Aula 4/ExemplosIC.xlsx", 
                         sheet = "Comprimento")

attach(dados2) 
n     <- length(comprimento)
media <- mean(comprimento)
dpa   <- sd(comprimento)
quantilt <- qt(0.975,df=14)
inf <- media - quantilt*dpa/sqrt(n)
sup <- media + quantilt*dpa/sqrt(n)

#----------------------- Interpretação ---------------------------#
# Com confiança de 95% e comprimento médio dos peixes deste rio está entre 
# 23,3874 e 24,5059cm. Está abaixo de 25 cm, confirmando a hipótese de interesse

t.test(comprimento, mu = 25)


####################################################
# Musicoterapia - slide 17
####################################################

# X: Número de cães que melhoraram após o tratamento
# p: proporção que melhoraram com o tratamento 

(p.est <- (156-12)/156)                # proporção estimada
(z0975 <- qnorm(0.975))                # Quantil normal
(EP    <- sqrt(p.est*(1 - p.est)/156)) # Erro padrão 

# IC(p,95%) [0,9231 \pm 1,96. 0,0213]
p.est - z0975*EP # LI
p.est + z0975*EP # LS

# Com confianla de 95%, a proporção de melhora dos cães submetidos à 
# musicoterapia está entre 0,8813 e 0,9649

# ------------- Slide 19 ----------------#
dados_glic <- read_excel("Documentos/Disciplinas/Análise no R/Aula 4/ExemplosIC.xlsx", 
                         sheet = "Glicemia")
# Estatística geral
summary(dados_glic)
attach(dados_glic)

# Diferença da glicemia antes do depois 
(difAD       <- Glicemia_Antes - Glicemia_Depois) 
(n_pacientes <- length(difAD))                    # Tamanho da amostra  
(media       <- mean(difAD))                      # Média das diferenças
(dpa         <- sd(difAD))                        # Desvio-padrão da diferença
(qt975       <- qt(0.975,df = 69))                # Quantil de t
media - qt975*dpa/sqrt(n_pacientes) # LI
media + qt975*dpa/sqrt(n_pacientes) # LS


t.test(Glicemia_Antes,Glicemia_Depois,paired = TRUE)
# Com confianla de 95%, a diferença média do nível de glicose antes e depois do
# tratamento esta entre 0.5245 e 3.161 mL/dL, ou seja, houve uma melhora
# significativa dos pacientes 

# ------------- Slide 21 ----------------#
dados4 <- read_excel("Documentos/Disciplinas/Análise no R/Aula 4/ExemplosIC.xlsx", 
                         sheet = "Comprimento2")

attach(dados4)
summary(dados4)
(n1 = length(R1))
(n2 = length(R2))
(med1 = mean(R1))
(med2 = mean(R2))
(dpa1 = sd(R1))
(dpa2 = sd(R2))
(var1 = var(R1))
(var2 = var(R2))

(varpond = ((n1-1)*var1+(n2-1)*var2)/(n1+n2-2))

(qt975 = qt(0.975,28))

(med1-med2-qt975*sqrt((1/n1 + 1/n2))*varpond)
(med1-med2+qt975*sqrt((1/n1 + 1/n2))*varpond)

t.test(R1,R2,var.equal = T)

# Com confiança de 95%, a diferença de comprimento médio dos peixes dos rios 
# R1 e R2 está entre -2.91 e -1.50cm, ou seja, em média os peixes do rio R1
# são menores do que as dos peixes do rio R2


# ------------- Slide 23 ----------------#
(qf1 = qf(0.025,14,14))
(qf2 = qf(0.975,14,14))

(var1 = var(R1))
(var2 = var(R2))

(1/qf2*var1/var2) # LI
(1/qf1*var1/var2) # LS

# Com confiança de 95%, a razão das variâncias dos comprimentos dos peixes dos
# rios R1 e R2 está entre 0.45 e 3.96. Dessa forma, podemos concluir que as 
# variâncias são equivalentes (supondo o comprimento possui distribuição normal)





































