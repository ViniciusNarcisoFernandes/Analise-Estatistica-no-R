#Aula 8

E<-seq(1,20,by=2)
V<-c(3.09,7.26,17.64,23.73,28.50,36.31,34.39,
     54.20,42.03,57.09)
correlacao_pearson<-cor(E,V)
correlacao_pearson

#Estatistica do teste
#Arco tangente hiperbólico da correlação
estatistica_do_teste<-atanh(correlacao_pearson)
#z_0 arco tangente de rho_0 (correlação populacional sob H_0)
z_0 <- atanh(0.95)

#RC{ Estatistica do teste > z_0+ z_{1-alpha}*erro_padrao(Estatistica sob H_0) 
estatistica_do_teste #1.99
z_0+qnorm(0.95)*(1/sqrt(10-7)) #valor crítico
#atanh(correlacao_pearson)< z_0+qnorm(0.95)*(1/sqrt(10-7))
# Estatistica do teste < valor crítico (não pertence a RC)
# Não rejeita-se H_0

estatistica_normalizada<-(z_calc-z_0)/(sqrt(1/(length(E)-3)))
# RC {estatistica_normalizada > Z_{1-alpha}}
estatistica_normalizada #0.432
qnorm(0.95) #1.645
# estatistica_normalizada < Z_{1-alpha}
# Não rejeita-se H_0

#Não rejeita-se H_0
atanh(0.95)+qnorm(0.95)*(1/sqrt(10-7))
#Valor p

#P(Z>estatistica_normalizada)
1-pnorm(estatistica_normalizada)              

#H_0: rho = 0 contra H_1: rho\neq 0 
idade_gestacional<-c(34,35,37,32,42,40,41,39,28,38)
peso_ao_nascer<-c(1.60,1.70,2.00,1.55,4.30,3.00,3.40,3.30,1.25,2.35)
r2<-cor(idade_gestacional,peso_ao_nascer)

#Teste
cor.test(idade_gestacional,peso_ao_nascer)
#alternative= "two.sided"(bilateral rho\neq 0)
# "greater" (rho>0)
# "less"

# IC 95% Correlação E e V
tanh(atanh(correlacao_pearson)+qnorm(c(0.025,0.975))*(1/sqrt((10-3))))
# IC 95% Correlação Idade Gestacional e Peso ao Nascer
tanh(atanh(r2)+qnorm(c(0.025,0.975))*(1/sqrt((10-3))))

#Correlação de Spearman
Tabagismo = c("Nao_fumante", "Nao_fumante", "Passivo", "Passivo",
              "Ativo", "Ativo", "Nao_fumante", "Passivo",
              "Ativo", "Ativo")
Glicemia = c(85, 90, 98, 102, 110, 115, 88, 100, 112, 118)
fator_Tabagismo<-factor(Tabagismo,
          levels=c('Nao_fumante','Passivo','Ativo'))
fator_Tabagismo<-as.numeric(fator_Tabagismo)
#Correlação de Spearman
cor_spearman<-cor(fator_Tabagismo,Glicemia,method='spearman')
cor(rank(fator_Tabagismo),rank(Glicemia))
rank(fator_Tabagismo)
rank(Glicemia)

# IC 95%
tanh(atanh(cor_spearman)+qnorm(c(0.025,0.975))*(1/sqrt((10-3))))
# Verificar se existe uma associação positiva
# alternativa rho>0
cor.test(fator_Tabagismo,Glicemia,
         alternative = "greater",
         method='spearman',exact = F)
# padrão teste exato
# exceção é quando há empates
# (mesmo rank em uma variável)

# Regressão Linear
dados <- data.frame(
  'Temperatura'=c(30,30,50,50,50,70,70,70,90,90),
  'Potencia'=c(38,43,32,26,33,19,27,23,14,21)
)

attach(dados)
reta<-lm(Potencia ~ Temperatura)
parametros<-coefficients(reta)
parametros
plot(Temperatura,Potencia,xlim=c(0,100),ylim=c(0,55))
abline(a=parametros[1],b=parametros[2])

summary(reta)

#S - erro padrão 
#corrigir slide 32
#estimativa para sigma dos resíduos
sigma(reta)
sigma(reta)^2 #estimativa para a variância

# IC para os parâmetos
confint(reta)
confint(reta,level=0.9)

#Summary (resultados do teste)
summary(reta)
#Predição
x_pred=seq(0,100,by=0.1)
x_unicos=unique(Temperatura)

#Intervalo para predição
previsoes<-predict(reta,newdata=data.frame(
  'Temperatura'=x_pred
))
previsoes

ic_media<-data.frame(data.frame(predict(reta,newdata=data.frame(
  'Temperatura'=x_unicos),interval='confidence', level = c(0.95)
)))
ic_media

ic_previsoes<-data.frame(predict(reta,newdata=data.frame(
  'Temperatura'=x_pred),interval='prediction', level = c(0.95)
))
ic_previsoes

plot(Potencia~Temperatura,xlim=c(0,100),ylim=c(-15,82))
abline(a=parametros[1],b=parametros[2])
lines(x_pred,ic_previsoes$lwr,col='red')
lines(x_pred,ic_previsoes$upr,col='red')

for(i in 1:4){
  arrows(x_unicos[i],ic_media$lwr[i],x_unicos[i],ic_media$upr[i],angle=90,col='blue')
  arrows(x_unicos[i],ic_media$upr[i],x_unicos[i],ic_media$lwr[i],angle=90,col='blue')
}
abline(v=mean(x_unicos))

legend(0,0,c('Intervalo de predição',expression(paste('Intervalo de confiança para ',mu(x[i]))))
       ,lty = c(1,1), col=c('red','blue'))
axis(3,60,expression(bar(x)))

# Resíduos brutos
residuals(reta)
# Resíduos padronizados
residuals(reta)/sigma(reta)
# Resíduos estudentizados internamente
rstandard(reta)
# Resíduos estudentizados externamente
rstudent(reta)

x11()
layout(matrix(c(1,2,3,4),ncol=2))
plot(residuals(reta),xlab = "Índice", ylab = "Brutos")
abline(h=0,lty=2)
plot(residuals(reta)/sigma(reta),xlab = "Índice", ylab = "Padronizados")
abline(h=0,lty=2)
plot(rstandard(reta),xlab = "Índice", ylab = "Internamente")
abline(h=0,lty=2)
plot(rstudent(reta),xlab = "Índice", ylab = "Externamente")
abline(h=0,lty=2)

plot(reta)

library(ggplot2)
ggplot(reta, aes(x = .fitted, y = .resid)) + 
  geom_point()+
  geom_line(y=0,linetype = "dashed")+
  labs(title = "Gráfico valores ajustados vs Resíduos", x="Ajustados" ,y = "Resíduos")

ggplot(reta, aes(x = .fitted, y = .stdresid)) + 
  geom_point()+
  geom_line(y=0,linetype = "dashed")+
  labs(title = "Gráfico valores ajustados vs Resíduos estudentizados internamente", x="Ajustados" ,y = "Resíduos estudentizados internamente")

library(car)
qqPlot(reta)
influenceIndexPlot(reta)

set.seed(42)
n <- 50
x <- seq(1,100,length.out=50)
y <- 2 + 3 * x + rnorm(n, sd = 1)

# Injeta um outlier na observação 10
y[20] <- 40
y[49] <- 25

fit <- lm(y ~ x)
influenceIndexPlot(fit)
influencePlot(fit)
influencePlot(reta)

x_novo<-x[-49]
y_novo<-y[-49]

fit_novo<-lm(y_novo~x_novo)
influencePlot(fit_novo)
influenceIndexPlot(fit_novo)

#Teste de Shapiro
shapiro.test(residuals(reta))

library(lmtest)
# Breusch-Pagan
bptest(reta,studentize=F)
# Koenker-Basset
bptest(reta)
# White
bptest(reta,~dados$Temperatura+I(dados$Temperatura^2))
# Goldfeldt-Quandt
gqtest(reta)
gqtest(reta,fraction=0.2)
gqtest(reta,alternative='two.sided')
gqtest(reta,alternative='less')
gqtest(reta,alternative='greater')

#Durbin-Watson
dwtest(reta,alternative='two.sided')
Box.test(residuals(reta),lag=2,type="Ljung-Box")
Box.test(residuals(reta),lag=4,type="Ljung-Box")

# Teste outlier
outlierTest(reta)
outlierTest(fit)
outlierTest(fit_novo)
