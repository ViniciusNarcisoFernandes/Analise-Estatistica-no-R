library(pwr)
library(ggplot2)

# Montar a tabela
animais_observado<-c(116,15,20,29)
p0_modelo<-c(8,1,1,2)/sum(c(8,1,1,2))

(animais_esperado<-p0_modelo*180)

soma <- (animais_observado-animais_esperado)^2/animais_esperado  

# Teste do qui-quadrado
(Estatistica_animais<-sum((soma)))

# Valor crítico
(critico_animais<-qchisq(1-0.05,3))
(critico_animais<-qchisq(0.05,3,lower.tail = F))

# p-value dos animais
(valor_p_animais <-(1-pchisq(Estatistica_animais,4-1)))
(valor_p_animais <-(pchisq(Estatistica_animais,4-1,lower.tail = F)))

#Montar a tabela
animais_observado<-c(116,15,20,29)

#Teste
(teste_animais<-chisq.test(animais_observado,p=p0_modelo))
#Frequencias observadas
teste_animais$observed
#Frequencias esperadas
teste_animais$expected
#Estatistica
teste_animais$statistic
#Residuos
teste_animais$residuals

# índice w (efeito)
# pwr
pH1_animais<-animais_observado/sum(animais_observado)
pH0_animais<-animais_esperado/sum(animais_esperado)

(w_animais<-sqrt(sum((pH1_animais-pH0_animais)^2/pH0_animais)))

#Poder do teste
pwr.chisq.test(w=w_animais,N=180,df=3,sig.level=0.05)

#Tamanho da amostra
pwr.chisq.test(w=w_animais,N=NULL,df=3,sig.level=0.05,power=0.8)

#Exemplo 2 - Mamão (slide 7)
mamao_observados<-c(321,306,368,680,282,43)
(mamao_esperados<-c(pnorm(300,mean=400,sd=100), #classe1
                    pnorm(350,400,100)-pnorm(300,400,100),
                    pnorm(400,400,100)-pnorm(350,400,100),
                    pnorm(500,400,100)-pnorm(400,400,100),
                    pnorm(600,400,100)-pnorm(500,400,100),
                    1-pnorm(600,400,100)
)*2000)

#Estatistica do teste
(Estatistica_mamao<-sum((mamao_observados-mamao_esperados)^2
                        /mamao_esperados))
#Valor-p
(valor_p_mamao<-(1-pchisq(Estatistica_mamao,5)))

#Critico 
(critico_mamao<-qchisq(0.95,5))

#Probabilidades sob H0
(pH0_mamao<-c(pnorm(300,mean=400,sd=100), #classe1
              pnorm(350,400,100)-pnorm(300,400,100),
              pnorm(400,400,100)-pnorm(350,400,100),
              pnorm(500,400,100)-pnorm(400,400,100),
              pnorm(600,400,100)-pnorm(500,400,100),
              1-pnorm(600,400,100)
))

# Teste
(teste_mamao<-chisq.test(mamao_observados,p=pH0_mamao))

# índice w
pH1_mamao<-mamao_observados/sum(mamao_observados)
(w_mamao<-sqrt(sum((pH1_mamao-pH0_mamao)^2/pH0_mamao)))

#Poder do teste
(pwr.chisq.test(w=w_mamao,N=2000,df=5,sig.level=0.05))
#Tamanho da amostra
(pwr.chisq.test(w=w_mamao,N=NULL,df=5,sig.level=0.05,power=0.8))






#Exemplo 3 - Crime (slide 11)

Crime_esperado<-as.table(
  matrix(
    c(645*60/1000,645*500/1000,645*440/1000,
      355*60/1000,355*500/1000,355*440/1000),
    nrow=2,
    byrow = TRUE
  )
)
Crime_esperado
colnames(Crime_esperado)<-c('Homicídio','Roubo','Assalto')
rownames(Crime_esperado)<-c('Estranho','Conhecido')
names(dimnames(Crime_esperado))<-c('Criminoso','Crime')
Crime_esperado


Crime_observado<-as.table(
  matrix(
    c(15,400,230,
      45,100,210),
    nrow=2,
    byrow = TRUE
  )
)
colnames(Crime_observado)<-c('Homicídio','Roubo','Assalto')
rownames(Crime_observado)<-c('Estranho','Conhecido')
names(dimnames(Crime_observado))<-c('Criminoso','Crime')
Crime_observado

#Totais por margem
#Totais linhas
margin.table(Crime_observado,1)
prop.table(margin.table(Crime_observado,1))
#Totais colunas
margin.table(Crime_observado,2)
prop.table(margin.table(Crime_observado,2))

#Estatística do teste
(Estatistica_crime<-sum((Crime_observado-Crime_esperado)^2/
                          Crime_esperado))
#Valor crítico
(critico_crime<-qchisq(p=1-0.05,df=(2-1)*(3-1)))
#Valor-p
(valor_p_crime<-(1-pchisq(q=Estatistica_crime,df=(2-1)*(3-1))))

#Teste
(teste_crime<-chisq.test(Crime_observado))
teste_crime$statistic #estatística do teste
teste_crime$p.value #valor de p
teste_crime$expected #frequencias esperadas
teste_crime$observed #frequencias observadas

#índice w efeito
pH0_crime<-teste_crime$expected/sum(teste_crime$expected)
pH1_crime<-teste_crime$observed/sum(teste_crime$observed)
(w_crime<-sqrt(sum((pH1_crime-pH0_crime)^2/pH0_crime)))

#Poder do teste
pwr.chisq.test(w=w_crime,N=1000,df=2,sig.level = 0.05)

#Tamanho da amostra
pwr.chisq.test(w=w_crime,N=NULL,df=2,sig.level = 0.05,power=0.8)
pwr.chisq.test(w=w_crime,N=NULL,df=2,sig.level = 0.05,power=0.99)

data.frame(Crime_observado)
p<-ggplot(data.frame(Crime_observado),
          aes(x=Crime,y=Criminoso))
p+geom_point(aes(size=Freq),shape=21,colour='black')+
  scale_size_area(max_size = 20)+
  labs(x='Crime',y='Criminoso')+
  guides(size=guide_legend(title='Frequência'))+
  geom_text(aes(label=Freq))


































#Exemplo 4 - Tabagismo (slide 13)
Oi_tabagismo<-as.table(matrix(
  c(49,16,
    106,79),ncol=2,byrow=T
))
colnames(Oi_tabagismo)<-c('Tabagista','Não tabagista')
rownames(Oi_tabagismo)<-c('Tabagista','Não tabagista')
names(dimnames(Oi_tabagismo))<-c('Pais','Filhos')
Oi_tabagismo
addmargins(Oi_tabagismo,FUN=list(Total=sum))


Ei_tabagismo<-as.table(matrix(
  c(65*155/250,65*95/250,
    185*155/250,185*95/250),ncol=2,byrow=T
))
rownames(Ei_tabagismo)<-c('Tabagista','Não tabagista')
names(dimnames(Ei_tabagismo))<-c('Pais','Filhos')
Ei_tabagismo
addmargins(Ei_tabagismo,FUN=list(Total=sum))

#Estatística do teste
(Estatistica_tabagismo<-sum(
  (abs(Oi_tabagismo-Ei_tabagismo)-0.5)^2/
    Ei_tabagismo))
#Valor crítico
qchisq(0.95,1)
#Valor-p
1-pchisq(Estatistica_tabagismo,1)

#Teste
help("chisq.test")
chisq.test(Oi_tabagismo)
#sem correção
chisq.test(Oi_tabagismo,correct=F)

#índice w efeito
(pH0_tabagismo<-Ei_tabagismo/sum(Ei_tabagismo))
(pH1_tabagismo<-Oi_tabagismo/sum(Oi_tabagismo))
(w_tabagismo<-sqrt(sum((pH1_tabagismo-pH0_tabagismo)^2/pH0_tabagismo)))

#Poder do teste
pwr.chisq.test(w=w_tabagismo,N=250,df=1,sig.level=0.05)
#Tamanho da amostra
pwr.chisq.test(w=w_tabagismo,N=NULL,df=1,sig.level=0.05,power=0.8)























