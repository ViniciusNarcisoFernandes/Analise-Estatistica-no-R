#########################################
# Aula 7: ANOVA e comparações múltiplas #
#########################################
library(readxl)
dados <- read_excel("07_dadosR.xlsx", sheet = "dados_peso")

attach(dados)
mean(Peso)
tapply(Peso,Dieta,length)
tapply(Peso,Dieta,mean)
round(tapply(Peso,Dieta,mean),2)
tapply(Peso,Dieta,summary)

library(ggplot2)
ggplot(dados, aes(x=Dieta, y=Peso))+
  geom_boxplot()+
  labs(x="Dieta", y="Peso", title="Boxplot do peso por dieta")

library(ggplot2)
ggplot(dados, aes(x=Dieta, y=Peso))+
  geom_point(size=2) +
  labs(x="Dieta", y="Peso")+
  geom_hline(yintercept=32.16, linetype="dashed", color = "blue")+
  stat_summary(geom="point",fun="mean",shape=24,fill=rgb(1,0,0,1),size=4)



modelo<-lm(Peso ~ Dieta, data=dados)
anova <- aov(modelo)
summary(anova)

### Analise de residuos


####################################
# Gráficos padrões do R para análise de resíduos
x11() #Abre nova janela de plotagem
par(mfrow=c(2,2)) #gráficos em 2 linhas e 2 colunas
#use par(mfrow=c(1,1)) para voltar a uma linha e uma coluna no gráfico
plot(anova)
####################################

shapiro.test(anova$residuals)
res_pad<-rstandard(modelo)
summary(res_pad)
tapply(res_pad,Dieta,summary)

#plot(anova$fitted.values,anova$residuals, xlab="Ajustados", ylab="Residuos")
#plot(anova$fitted.values,res_pad, xlab="Ajustados", ylab="Residuos Padronizados")
#abline(h=0) # abline(0,0)

qqnorm(anova$residuals)

plot(anova, 1 ) # grafico ajustados vs residuos
tapply(anova$residuals,round(anova$fitted,1),mean)
# valores medios dos residuos (linha vermelha)
plot(anova, 2 ) # qqplot dos residuos padronizados

#round(anova$residuals,4)     # Residuos do ajuste
#round(sum(anova$residuals),4)
#round(anova$fitted.values,4) # Valores ajustados

#install.packages("broom")
library(broom)
ggplot(modelo, aes(x = .fitted, y = .resid)) + 
  geom_point()+
  geom_line(y=0,linetype = "dashed")+
  labs(title = "Gráfico valores ajustados vs Resíduos", x="Ajustados" ,y = "Resíduos")

ggplot(modelo, aes(x = .fitted, y = .stdresid)) + 
  geom_point()+
  geom_line(y=0,linetype = "dashed")+
  labs(title = "Gráfico valores ajustados vs Resíduos padronizados", x="Ajustados" ,y = "Resíduos")

dados <- data.frame(Dieta,Peso,residuos=anova$residuals, ajustados=anova$fitted.values)
head(dados)

ggplot(dados, aes(sample = anova$residuals)) +
  stat_qq() +
  stat_qq_line()+
  labs(title = "Gráfico Quantil-quantil",x="Quantil amostral" ,y = "Quantil teórico")

bartlett.test(Peso ~ Dieta, data=dados)
#http://www.sthda.com/english/wiki/compare-multiple-sample-variances-in-r
# Baseado nas variancias e tamanho amostrais - Comparacao entre a variancia global e variancias para cada tratamento
# Estatistica do teste tem dist Qui-Quadrado com I-1 g.l.

# install.packages("car")
library(car)
leveneTest(Peso, group=as.factor(Dieta), data=dados)
fligner.test(Peso ~ as.factor(Dieta), data=dados)
# Alternativa ao Bartlett para suspeita de nao normalidade dos residuos

# Teste Durbin-Watson
# https://www.marsja.se/durbin-watson-test-in-r-step-by-step-incl-interpretation/
# install.packages("lmtest")
library(lmtest)
dwtest(Peso~as.factor(Dieta))
#ggplot(dados, aes(y =Peso)) + 
#  geom_point(col=Dieta)

### Comparacoes multiplas: Tukey e Dunnet
library(car)
TukeyHSD(anova)
plot(TukeyHSD(anova))

pairwise.t.test(Peso, Dieta,
                p.adjust.method ="bonferroni")


#install.packages("DescTools")
library(DescTools)
DunnettTest(x=Peso, g=as.factor(Dieta), control = "C")

### Tamanho da amostra


library(pwr)
(eta2<-108.1/(108.1+436.9)) # porcentagem da variância total explicada pelo tratamnento (SQTrat/SQTotal)

(efeito<-sqrt(eta2/(1-eta2)))
pwr.anova.test(k=4,f=efeito,sig.level = 0.05,power=0.80)
# k: numero de grupos



### Teste nao paramétrico de Kruskall-Wallis

library(pgirmess) 
teste<-kruskal.test(Peso,g=factor(Dieta))
teste
kruskalmc(Peso ~ Dieta)

library(dunn.test)
dunn.test(Peso, factor(Dieta),  method="bonferroni")
detach(dados)

##################################################################
# qq-plot Prof. Dr. Gilberto Alvarenga de Paula
# Distribuição normal
# https://www.ime.usp.br/~giapaula/envel_norm.txt
#################################################################

#------------------------------------------------------------------------------------------#
# Comandos:
#       fit.model = ajuste
#       attach(dados)
#       source("envel_norm.txt")
#------------------------------------------------------------------------------------------#
#par(mfrow=c(1,1))
fit.model=anova
X =   model.matrix(fit.model)
n =   nrow(X)
p =   ncol(X)
H =   X%*%solve(t(X)%*%X)%*%t(X)
h =   diag(H)
si =   lm.influence(fit.model)$sigma
r =   resid(fit.model)
tsi =   r/(si*sqrt(1-h))
#
ident =   diag(n)
epsilon =   matrix(0,n,100)
e =   matrix(0,n,100)
e1 =   numeric(n)
e2 =   numeric(n)
#
for(i in 1:100){
  epsilon[,i] =   rnorm(n,0,1)
  e[,i] =   (ident - H)%*%epsilon[,i]
  u =   diag(ident - H)
  e[,i] =   e[,i]/sqrt(u)
  e[,i] =   sort(e[,i]) }
#
for(i in 1:n){
  eo =   sort(e[i,])
  e1[i] =   (eo[2]+eo[3])/2
  e2[i] =   (eo[97]+eo[98])/2 }
#
med =   apply(e,1,mean)
faixa =   range(tsi,e1,e2)
#
x11() # Abrir nova janela para não desconfigurar os padrões de plotagem
par(pty="s")
qqnorm(tsi,xlab="Quantil da N(0,1)",
       ylab="Resíduo Studentizado", ylim=faixa, pch=16, main="", cex=2, cex.axis=1.5, cex.lab=1.5)
par(new=TRUE)
qqnorm(e1,axes=FALSE,xlab="",ylab="",type="l",ylim=faixa,lty=1, main="",lwd=2)
par(new=TRUE)
qqnorm(e2,axes=FALSE,xlab="",ylab="", type="l",ylim=faixa,lty=1, main="",lwd=2)
par(new=TRUE)
qqnorm(med,axes=FALSE,xlab="",ylab="",type="l",ylim=faixa,lty=2, main="",lwd=2)
#------------------------------------------------------------------------------------------#

#################################################################
##################################################################
#Box-cox
##################################################################
boxcox_transform<-function(y,lambda){
  if(lambda!=0){
    return((y^lambda-1)/lambda)
  }else{
    return(log(y))
  }
    }
  

library(readxl)
cafe <- read_excel("07_dadosR.xlsx", sheet = "dados_cafe")

attach(cafe)
library(MASS)
anova_cafe<-aov(Colheita~Variedade)
plot(anova_cafe)
boxcox_cafe<-boxcox(aov(Colheita~Variedade))

lambda_boxcox<-boxcox_cafe$x[which.max(boxcox_cafe$y)]
boxcox_cafe<-aov(boxcox_transform(Colheita,lambda_boxcox)~Variedade)
plot(boxcox_cafe)
