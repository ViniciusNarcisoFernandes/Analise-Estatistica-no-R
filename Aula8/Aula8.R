rm(list=ls())
library(ggplot2)
# --- Parameters ---
set.seed(34)
kcat  <- 5        # s^-1
Km    <- 2        # mM
S     <- 3        # mM (fixed substrate concentration)
E     <- seq(1, 20, by=2)   # enzyme concentration (mM)
noise_sd <- 5   # measurement noise

# --- Generate data from Michaelis-Menten ---
# v = Vmax * [S] / (Km + [S])  where Vmax = kcat * [E]
v_obs <- round(kcat * E * S / (Km + S+rnorm(10)),2)

dat <- data.frame(E = E, v = v_obs)

# --- Pearson correlation ---
r <- cor(dat$E, dat$v)
cat("Pearson r =", round(r, 4), "\n")

# --- Hypothesis test: is r > 0.9? ---
# Use the t-test for correlation
n <- length(E)
t_stat <- r * sqrt((n - 2) / (1 - r^2))
p_val  <- 2 * pt(-abs(t_stat), df = n - 2)

threshold <- 0.95
cat("Threshold =", threshold, "\n")
cat("r > threshold?", r > threshold, "\n")
cat("p-value =", p_val, "\n")

# --- Plot ---
plot(dat$E, dat$v,
     pch = 19, col = "steelblue",
     xlab = "[E] (mM)", ylab = "v (mM/s)",
     main = "Correlação [E] vs. v")

# Reta de regressão
abline(lm(v ~ E, data = dat), col = "red", lwd = 2)

x11()
par(mfrow=c(2,2))
plot(lm(v ~ E, data = dat))

shapiro.test(dat$E)
shapiro.test(dat$v)

# Anotar r e p na figura
rho0 <- 0.95
n    <- length(E)

# z-score da observação e do limite
z_obs <- atanh(r)
z_0   <- atanh(rho0)

# Estatística de teste (assintoticamente Normal(0,1) sob H0)
z_stat <- (z_obs - z_0) / sqrt(1 / (n - 3))

# p-value (teste unilateral: ρ > ρ₀)
p_val <- pnorm(z_stat, lower.tail = FALSE)

cat("r =", round(r, 4), "\n")
cat("ρ₀ =", rho0, "\n")
cat("z_stat =", round(z_stat, 4), "\n")
cat("p-value =", p_val, "\n")
cat("Rejeito H0 (r > 0.95)?", p_val < 0.05, "\n")

text(0.1, max(dat$v) * 0.9,
     labels = sprintf("r = %.3f\np = %.4f", r, p_val),
     pos = 4, cex = 0.9, col = "black")

#Exemplo 2
idade_gestacional <- c(34, 35, 37, 32, 42, 40, 41, 39, 28, 38)
peso_ao_nascer    <- c(1.60, 1.70, 2.00, 1.55, 4.30, 3.00, 3.40, 3.30, 1.25, 2.35)

shapiro.test(idade_gestacional)
shapiro.test(peso_ao_nascer)

r<-cor(idade_gestacional,peso_ao_nascer)
r
cor.test(idade_gestacional,peso_ao_nascer,exact=TRUE)
cor.test(idade_gestacional,peso_ao_nascer,exact=FALSE)

qt(c(0.025,0.975),8)

# Caso do Tabagismo
Tabagismo = c("Nao_fumante", "Nao_fumante", "Passivo", "Passivo",
              "Ativo", "Ativo", "Nao_fumante", "Passivo",
              "Ativo", "Ativo")
Glicemia = c(85, 90, 98, 102, 110, 115, 88, 100, 112, 118)
rank(Glicemia)
fator_Tabagismo <- as.numeric(factor(Tabagismo,levels=c('Nao_fumante','Passivo','Ativo')))
spearman_tabagismo_glicemia<-cor(fator_Tabagismo,Glicemia,method='spearman')
tanh(atanh(spearman_tabagismo_glicemia)+qnorm(c(0.025,0.975))/sqrt(10-3))

# Com empates, exato não é usado mesmo que especificado
cor.test(fator_Tabagismo,
         Glicemia,
         method='spearman',
         alternative='greater',
         exact = FALSE)

set.seed(1241)
Investimento<-sample(c('Baixo','Médio','Alto'),50,TRUE,c(0.25,0.25,0.5))
sortear_produtividade<-function(i){
 return(round(ifelse(i=='Alto',rnorm(1,70,5),ifelse(i=='Médio',rnorm(1,60,5),rnorm(1,55,5)))))
}
Produtividade<-sapply(Investimento,sortear_produtividade)

investimento_produtividade<-data.frame(
  'Investimento'=Investimento,
  'Produtividade'=Produtividade
)

ggplot(data = investimento_produtividade, aes(x = factor(Investimento,levels=c('Baixo','Médio','Alto')), y = Produtividade)) +
  geom_boxplot(fill = "#3a86d4", alpha = 0.7) +
  labs(x = "Investimento", y = "Produtividade (sc/ha)") +
  theme_minimal()
rank(as.numeric(factor(Investimento,levels=c('Baixo','Médio','Alto'))))
rank(as.numeric(Produtividade))
cor(as.numeric(factor(Investimento,levels=c('Baixo','Médio','Alto'))),Produtividade,method='spearman')
cor.test(as.numeric(factor(Investimento,levels=c('Baixo','Médio','Alto'))),Produtividade,type='Spearman',alternative = 'greater')

# Exemplo regressão linear simples
dados <- data.frame(
  'Temperatura'=c(30,30,50,50,50,70,70,70,90,90),
  'Potencia'=c(38,43,32,26,33,19,27,23,14,21)
)

attach(dados)
modelo <- lm(Potência~Temperatura,data = dados)
summary(modelo)

# Gráfico
plot(Temperatura,Potencia,xlim = c(0,100), ylim = c(0,55))
abline(a = modelo$coefficients[1], b = modelo$coefficients[2])

# S - erro padrão 
# Estimativa para sigma dos resíduos 
sigma(modelo)
sigma(modelo)^2  # Estimativa para variância 

# Intervalo de confiança 
confint(modelo)
confint(modelo,level=0.90)

# Predição
x_pred = seq(0,100,by=0.1)
x_unicos = unique(Temperatura)
predict(modelo,
        newdata = data.frame('Temperatura' = x_pred))

# Intervalo de Predição 

#############
#Ajuste usando os estimadores
#############

# Calculando quantidade anteriores
sum_xy=sum(dados$Temperatura*dados$Potência)
sum_x=sum(dados$Temperatura)
mean_x=mean(dados$Temperatura)
sum_y=sum(dados$Potência)
mean_y=mean(dados$Potência)
sum_x2=sum(dados$Temperatura**2)
sum_squares_yybar=sum((dados$Potência-mean(dados$Potência))**2)
sum_squares_xxbar=sum((dados$Temperatura-mean(dados$Temperatura))**2)
x_unicos<-unique(dados$Temperatura)

#Calculando parâmetros
n=10
beta_estimado<-(sum_xy-n*mean_x*mean_y)/(sum_x2-n*mean_x**2)
alpha_estimado<-mean_y-beta_estimado*mean_x
y_estimado<-alpha_estimado+beta_estimado*dados$Temperatura
SQResiduos<-sum((dados$Potência-y_estimado)^2)
sigma_estimado<-sqrt(SQResiduos/8)
SQResiduos/8

cairo_pdf('regressao.pdf',width=10,height=7)
plot(Potência~Temperatura,data=dados)
abline(a=alpha_estimado,b=beta_estimado)
dev.off()

x_pred<-seq(0,100,by=0.01)
lower_predinterval<-alpha_estimado+beta_estimado*x_pred+qt(0.025,8)*sqrt(sigma_estimado)*sqrt(1+1/10+(x_pred-mean_x)**2/(sum_squares_xxbar))
upper_predinterval<-alpha_estimado+beta_estimado*x_pred+qt(0.975,8)*sqrt(sigma_estimado)*sqrt(1+1/10+(x_pred-mean_x)**2/(sum_squares_xxbar))

reta<-lm(Potência~Temperatura,data=dados)
summary(reta)
coef(reta)
sigma(reta)
confint(reta)

previsoes<-predict(reta,newdata=data.frame(
  'Temperatura'=x_pred
  ))
ic_media<-data.frame(data.frame(predict(reta,newdata=data.frame(
  'Temperatura'=x_unicos),interval='confidence', level = c(0.95)
)))
ic_previsoes<-data.frame(predict(reta,newdata=data.frame(
  'Temperatura'=x_pred),interval='prediction', level = c(0.95)
))

{
  cairo_pdf('regressao_ic.pdf',width=10,height=7)
  plot(Potência~Temperatura,data=dados,xlim=c(0,100),ylim=c(-15,82))
  abline(a=alpha_estimado,b=beta_estimado)
  lines(x_pred,ic_previsoes$lwr,col='red')
  lines(x_pred,ic_previsoes$upr,col='red')
  
  for(i in 1:4){
    arrows(x_unicos[i],ic_media$lwr[i],x_unicos[i],ic_media$upr[i],angle=90,col='blue')
    arrows(x_unicos[i],ic_media$upr[i],x_unicos[i],ic_media$lwr[i],angle=90,col='blue')
  }
  abline(v=mean_x)
  
  legend(0,0,c('Intervalo de predição',expression(paste('Intervalo de confiança para ',mu(x[i]))))
         ,lty = c(1,1), col=c('red','blue'))
  axis(3,60,expression(bar(x)))
  dev.off()
}

#Calculando estimativas
y_estimado<-alpha_estimado+beta_estimado*dados$Temperatura


#Resíduos
residuos<-dados$Potência-y_estimado
round(residuos,2)
residuos_padronizados<-residuos/sigma_estimado
round(residuos_padronizados,2)
v_ii<-1/10+(dados$Temperatura-mean_x)**2/sum_squares_xxbar
residuos_estudentizados<-residuos/(sqrt(sigma_estimado)*sqrt(1-v_ii))
round(residuos_estudentizados,2)

#Gráficos de resíduos
{
  cairo_pdf('residuos.pdf',width=6,height=4.5)
  layout(matrix(c(1,2,3,4),ncol=2))
  plot(residuals(reta),xlab = "Índice", ylab = "Brutos")
  abline(h=0,lty=2)
  plot(residuals(reta)/sigma(reta),xlab = "Índice", ylab = "Padronizados")
  abline(h=0,lty=2)
  plot(rstandard(reta),xlab = "Índice", ylab = "Internamente")
  abline(h=0,lty=2)
  plot(rstudent(reta),xlab = "Índice", ylab = "Externamente")
  abline(h=0,lty=2)
  dev.off()
}

# Diagnostico padrao
{
  cairo_pdf('stddiag.pdf',width=6,height=4.5)
  par(mfrow=c(2,2))
  plot(reta)
  dev.off()
}

qqPlot(reta)

library(ggplot2)
ggplot(reta, aes(x = .fitted, y = .resid)) + 
  geom_point()+
  geom_line(y=0,linetype = "dashed")+
  labs(title = "Gráfico valores ajustados vs Resíduos", x="Ajustados" ,y = "Resíduos")

ggplot(reta, aes(x = .fitted, y = .stdresid)) + 
  geom_point()+
  geom_line(y=0,linetype = "dashed")+
  labs(title = "Gráfico valores ajustados vs Resíduos estudentizados internamente", x="Ajustados" ,y = "Resíduos estudentizados internamente")

ggplot(ANOVA_cafe1947, aes(x = .fitted, y = rstudent(reta))) + 
  geom_point()+
  geom_line(y=0,linetype = "dashed")+
  labs(title = "Gráfico valores ajustados vs Resíduos estudentizados externamente", x="Ajustados" ,y = "Resíduos estudentizados externamente")

library(lmtest)
# Breusch-Pagan
bptest(reta,studentize=F)
# Koenker-Basset
bptest(reta)
# White
bptest(reta,~dados$Temperatura+I(dados$Temperatura^2))
gqtest(reta)

sort(residuos)
p_i<-(c(1,2,3,4,5,6,7,8,9,10)-0.5)/10
qnorm(p_i)
plot(qnorm(p_i),sort(residuos_padronizados))
abline(a=0,b=1)
qqnorm(residuos_padronizados)
qqline(residuos_padronizados, col = "red", lwd = 2)
plot(modelo)
shapiro.test(residuos)
shapiro.test(residuos_padronizados)

# Diagnósticos padrões
plot(reta)

# Resíduos brutos
residuals(reta)

# Resíduos padronizados
residuals(reta)/sigma(reta)

# Resíduos estudentizados internamente
rstandard(reta)

# Resíduos estudentizados externamente
rstudent(reta)

library(car)
influencePlot(reta)
influenceIndexPlot(reta)
outlierTest(reta)

set.seed(42)
n <- 50
x <- seq(1,100,length.out=50)
y <- 2 + 3 * x + rnorm(n, sd = 1)

# Injeta um outlier na observação 10
y[20] <- 40
y[49] <- 25

fit <- lm(y ~ x)
outlierTest(fit)

influencePlot(fit)
influenceIndexPlot(fit)
