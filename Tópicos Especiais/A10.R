library(readxl) # Leitura de dados
library(knitr) # Tabela de MakDown
library(ggplot2) # Gráficos 
library(olsrr) # CP de Mallows
library(MPV) # Estatística de PRESS
library(glmnet) # Ridge, Lasso e Elastic-Net
library(pls)  # Mínimos quadrados parciais
library(Plot3D) # Gráfico em 3 dimensões 

pneus <-  read_excel("Documentos/Disciplinas/Análise no R/Aula 10/dados.xlsx")

attach(pneus)

ggplot(pneus, aes(x = carga, y = area, color = factor(tipo))) +
  geom_point(alpha = 0.5, size = 1.5) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  labs(color = "Tipo")

# Modelo de 3 retas paralelas (com interceptos distintos e mesma inclinação)
reta_pneus <- lm(area ~ carga + factor(tipo))
summary(reta_pneus)  

# Intercepto, carga e fator (Diagonal e Radial)
kable(model.matrix(reta_pneus))  

# Montando as retas 
coefs <- coefficients(reta_pneus)
intercepts <- c(coefs[1], coefs[1] + coefs[3:length(coefs)])
niveis <- levels(factor(tipo))
retas <- data.frame(
  tipo
  = niveis,
  intercept = intercepts,
  slope
  = coefs[2]
)
ggplot(pneus, aes(x = carga, y = area, color = factor(tipo))) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_abline(data = retas,
              aes(intercept = intercept, slope = slope, color = factor(tipo)),
              linewidth = 1.2) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  labs(color = "Tipo")+
  xlim(0, 25)+
  ylim(0.1,0.4)

# Modelo de 3 retas (com interceptos e inclinações distintas)
reta_pneus <- lm(area ~ carga + factor(tipo)+carga*factor(tipo))
summary(reta_pneus)

kable(model.matrix(reta_pneus))


coefs<- coefficients(reta_pneus)
intercepts <- c(coefs[1], coefs[1] + coefs[3:4])
slopes <- c(coefs[2],coefs[2] + coefs[5:6])
niveis <- levels(factor(tipo))
retas <- data.frame(
  tipo
  = niveis,
  intercept = intercepts,
  slopes
  = slopes
)
ggplot(pneus, aes(x = carga, y = area, color = factor(tipo))) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_abline(data = retas,
              aes(intercept = intercepts, slope = slopes, color = factor(tipo)),
              linewidth = 1.2) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  labs(color = "Tipo")+
  xlim(0, 25)+
  ylim(0.1,0.4)





