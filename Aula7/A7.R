# Libraries  
library(readxl)
library(tidyverse)
library(carData)
library(car)


# importando os dados 
dados <- read_excel("Documentos/Disciplinas/Análise no R/Aula 7/07_dadosR.xlsx", 
                         sheet = "dados_peso")

attach(dados)
anova_peso <- aov(Peso~Dieta)

# Sumário da anova
summary(anova_peso)

# Valor-p 
1-pf(4.619,3,56)

# Outra forma
pf(4.619,3,56,lower.tail = FALSE)

# Abra x11 para usar esse comando 
par(mfrow=c(2,2))
plot(anova_peso)
qqnorm(residuals(anova_peso))
qqline(residuals(anova_peso))

# Estrutura dos dados 
mean(Peso)

# Testes para homogeneidade de variância
bartlett.test(Peso, Dieta)
# Pacote car / Levene (mais robusto a não-normalidade)
leveneTest(Peso,factor(Dieta))
leveneTest(Peso,factor(Dieta))
leveneTest(Peso,factor(Dieta))
























