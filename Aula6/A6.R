# Slide 5 

# 1) Hipóteses
# H0: variâncias equivalentes => sigma1 = sigma2         Bilateral 
# H1: variâncias diferentes   => sigma1 != sigma2
# 2) Nível de significância 
# 3) Estatística do teste        Fobs = (sigma1/sigma2)² 

library(readxl)

dados1 <- read_excel("Documentos/Disciplinas/Análise no R/Aula 6/06_dadosR.xlsx")
  
head(dados1)
summary(dados1)
attach(dados1)

boxplot(Comprimento~Rio,
        main="Boxplot do comprimento dos peixes")

# Tamanho da amostra por Rio
tapply(Comprimento, Rio, length)
# Média amostral
tapply(Comprimento, Rio, mean)
# Desvio-padrão 
tapply(Comprimento, Rio, sd)
# Variância 
tapply(Comprimento, Rio, var)

# Suposição de normalidade
# Desvio-padrão 
tapply(Comprimento, Rio, shapiro.test)

# Teste de normalidade 
# 1) Teste de hipóteses
# H0: comprimento dos peixes do rio possui distribuição normal
# H1: comprimento dps peixes d rio não possui distribuição normal 
# Bilateral 
# 2) Nível de significância: alpha = 0.05 
# 3) Estatística do teste e valor de p

library(nortest)
tapply(Comprimento, Rio, lillie.test)
tapply(Comprimento, Rio, ad.test)
tapply(Comprimento, Rio, cvm.test)

# Teste F comparação de variâncias 
s2 <- tapply(Comprimento, Rio, var)

# Estatística do teste
s2[1]/s2[2] 

# Regra de decisão 
qf(0.025,14,14)
qf(0.975,14,14)

# Verificando a comparação das variâncias 
var.test(Comprimento~Rio)

# Conclusão: Com nível de significância alpha = 0.05, não rejeita H0, ou seja,
# as variâncias do comprimento dos peixes dos rios R1 e R2 são equivalentes. 

# Slide 6 
# 1) Hipóteses
# H0: os comprimentos médios dos peixes dos rios R1 e R2 são equivalentes
# H1: os comprimentos médios dos peixes dos rios R1 e R2 não são equivalentes
# 2) Nível de significância: alpha = 0.05
# 3) Estatística do teste - Vimos no teste de igualdade de variâncias que
# as variãncias são equivalentes, dessa forma, vamos calcular a variância 
# ponderada conforme


# Estatística do teste t
ni  <- tapply(Comprimento, Rio, length)
ni[1]
varpond <- (s2[1]*(ni[1]-1) + s2[2]*(ni[2]-1))/(sum(ni-1)) 
media  <- tapply(Comprimento, Rio, mean)
(media[1] - media[2])/sqrt((1/ni[1]+1/ni[2])*varpond)

# 4) Regra de decisão bilateral, alpha = 0.05 
# gl = n1 + n2 - 2 = 28

# Regra de decisão 
qt(0.025,28)
qt(0.975,28)

# Valor de p 
2*pt(-6.393652,28) 

# Teste t para as variâncias
t.test(Comprimento~Rio, var.equal = TRUE)

# Conclusão: com nível de significância de 0.05, rejeita H0, ou seja, os
# comprimentos médios dos peixes dos rios R1 e R2 são diferentes 


# ------------------------------------------------------
# Tamanho da amostra 

library(pwr)

(ES <- (media[1] - media[2])/sqrt(varpond))
pwr.t.test(d=ES,
           power = 0.80,
           type = "two.sample",
           alternative = "two.sided")

# Poder de 80%, duas amostras independentes 
# Teste bilateral 
# n = 4.10 ou seja 5 peixes por rio 
pwr.t.test(d = ES, 
           n = 15, 
           type="two.sample", 
           alternative = "two.sided"
           )

# Slide 11

# H0: mu1 <= mu2, não houve melhora com o tratamento 
# H1: mu1 > mu2 , houve melhora com o tratamento 

# H0: mu1 - mu2 <= 0 
# H1: m1 - mu2 > 0

# H0: mu2 <= 0 
# H1: mu1 > 0

# 2) Nível de significância alpha = 0.05

# Dados de Glicemia 
dados3 <- read_excel("Documentos/Disciplinas/Análise no R/Aula 6/06_dadosR.xlsx", 
                     sheet = "Glicemia")
summary(dados3)
attach(dados3)

# Suposição de normalidade 
shapiro.test(Glicemia_Antes)
shapiro.test(Glicemia_Depois)

# 1) Teste de hipótese
# H0: a glicemia tem distribuição normal
# H1: a glicemia não tem distribuição normal

# 2) Nível de significância: alpha 0.05 
# 3) Estimativa do teste e valor de p
# Conclusão: com n.s 0.05, não rejeita H0, ou seja, a glicemia possui 
# distribuição normal nos dois momentos 


# Diferença da glicemia por paciente 
dif       <- Glicemia_Antes - Glicemia_Depois
(media_dif <- mean(dif))
(dpa_dif   <- sd(dif))
(n_dif     <- length(dif))

# Estatística dos teste - T calculado
(Est <- media_dif*sqrt(n_dif)/dpa_dif)

# 4) Regra de decisão: unilateral a direita, alpha = 0.05, g.l = n-1 = 69

# Valor crítico 
qt(0.95,69)

# Valor de p
1-pt(3.067699,69)

# Conclusão: com n.s de 0.05, rejeita H0, ou seja, houve melhora com o tratamento
# pois a glicemia media depois do tratamento é menor do que antes do tratamento 

# Utilizando a função 
t.test(Glicemia_Antes,Glicemia_Depois,paired=TRUE, alternative = 'greater')


# Tamanho amostral 
# Poder de 80%, N.S = 0.05 e Tamanho do efeito 
library(pwr)

ES <-  media_dif/dpa_dif

pwr.t.test(d=ES, sig.level = 0.05, 
           power = 0.80,
           type = "paired",
           alternative = "greater")

# 1) Teste de hipóteses para duas proporções 
# H0: pA  = pB
# H1: pA != pB 
# 2) Nível se significância
# 3) Estatística do teste, Valores observados e Método A e B
round(217*100/330,1) # Valores esperados
round(113*100/330,1) # Valores esperados 

n <- c(217,113)
k <- c(17,1)

teste1 <- prop.test(n-k,n,correct=FALSE)
Obs    <- matrix(c(n-k,k),ncol=2)

# Teste de qui-quadrado 
teste2 <- chisq.test(Obs)
# Valores esperados 
Esp    <-teste2$expected
# Valores observados 
teste2$observed

# Estatística sem correção
(Estat1 <- sum((Obs-Esp)^2/Esp))

# Estatística com correção 
(Estat2 <-sum((abs(Obs-Esp)-0.5)^2/Esp))

# 4) Regra de decisão bilateral, alpha = 0.05, gl = (I-1)(J-1) 

qchisq(0.95,1)
1-pchisq(Estat2,1)

# Conclusão: com n.s de 0.05 rejeita H0, ou seja, há diferença na detecção do 
# tumor entre os dois métodos A e B 
prop.test(n-k,n)

chisq.test(Obs)

library(pwr)
ES = Es.h(p1=0.92,p2=0.99)
pwr.2p.test(h = ES,
            sig.level = 0.05,
            power = 0.80)

# aproximandamente 117 tumores para cada método 
pwr.2p2n.test(h=ES,n1=217,n2=113)


# Parei aqui - depois pego das anotações da Miriam
# Distribuição hipergeométrica 
dhyper(2,m=4,n=11,k=3)
dhyper(3,m=4,n=11,k=3)

# Teste exato de fisher
(Obs2 <- matrix(c(8,3,2,7),ncol=2))

chisq.test(Obs2)
chisq.test(Obs2)$expected 
fisher.test(Obs2)

# Conclusão: Com n.s de 0.05, não rejeita H0, ou seja, a proporção de melhora 
# não difere entre os testes A e B


# Slide 22 
# 1) Teste de hipótese
# H0: mediana dos vacinados  maior ou igual não vacinados 
# H1: mediana vacinados menor não vacinados
# 2) Nível de significância: alpha = 0.05 
# 3) Estatística do teste e valor p

# Teste de Wilcoxon
library(readxl)
dados4 <- read_excel("Documentos/Disciplinas/Análise no R/Aula 6/06_dadosR.xlsx", 
                     sheet = "Gravidade_Pacientes")

head(dados4)
summary(dados4)
attach(dados4)
postos <- rank(Gravidade2)
dados4 <- cbind(dados4,postos)
dados4

# Soma dos postos
(somapostos <- tapply(postos, Vacina, sum))
# Número de pacientes por vacina 
(tapply(postos, Vacina, length))

25173.5-151*(151)/2

wilcox.test(Gravidade2~Vacina,
            alternativa = "greater")

wilcox.test(Gravidade2~Vacina,
            alternativa = "less")

# Conclusão: com n.s de 0.05 rejeita H0, ou seja, a mediana da gravidade 
# vacinados menor do que dos não vaciandos 

boxplot(Gravidade2~Vacina,
        main = "Boxplot da gravidade por vacina")

# Ordenar os fatores 
Gravidade3 <- factor(Gravidade, levels = c('Não_urgente',
                             'Pouco_urgente',
                             'Urgente',
                             'Muito_urgente',
                             'Emergencia'))
Gravidade3

# Converter para vetor numérico 
as.numeric(Gravidade3)
head(Gravidade3)
Gravidade2 





    