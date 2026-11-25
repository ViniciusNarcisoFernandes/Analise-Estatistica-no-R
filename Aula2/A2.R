# Library 
library(readxl)
library(ggplot2)

# Banco de dados 
dados <- read_excel("Documentos/Disciplinas/Análise no R/Aula 2/Exemplos.xlsx",sheet = "ExemploA2")
View(dados)

# Aparece o nome das variáveis do banco de dados 
names(dados)

# Adicionando dados no comando ATTACH
attach(dados)

# Fazendo uma tabela de frequência da var. SEXO
sexo <- ordered(sexo,levels=c("F","M","I"))

# Comando que me dará o total de variáveis 
addmargins(table(sexo), FUN = list(Total=sum))

# Criando um novo banco de dados
dados2 <- data.frame(table(sexo))
dados2

# Gráfico de barras 
ggplot(dados2, aes(x = sexo, y = Freq)) +
  geom_bar(stat="identity",fill=c("lightblue","darkorange","lightgreen"),
           color = "black") + 
  labs(title = "Distribuição das aves por sexo", x = "Sexo",
       y = "Frequência") + scale_x_discrete(
         labels = c("F" = "Fêmea", "M" = "Macho", 
                    "I" = "Indeterminado")
       ) 
  
# Gráfico relacionando duas variáveis qualitativas 
dados3 <- data.frame(table(Obito,sexo))

# Gráfico 
ggplot(dados3, aes(x = sexo, y = Freq)) + 
  geom_bar(stat="identity",aes(fill = sexo)) +
  facet_grid(~Obito) +
  labs(title = "Distribuição das aves por sexo e óbito",
       x = "Óbito", y = "Frequência") + 
  scale_x_discrete(labels=c("F" = "Fêmea", "M" = "Macho",
                            "I" = "Indeterminado"))

# Gráfico de setores 
ggplot(dados2, aes(x = "", y = Freq, fill = sexo)) + 
  geom_bar(stat="identity", width = 1,color="white") + 
  coord_polar("y",start=0) +
  theme_void() + 
  labs(title = "Gráfico de setores de aves por sexo e óbito") 

# Gráfico para variáveis contínuas 
head(dados)

# Histograma são gráficos de barras para variáveis quantitativas 
ggplot(dados,aes(x = Tempo_dias)) + 
  geom_histogram(binwidth=2, fill = "darkorange") + 
  labs(title="Distribuição do tempo de Permanência",
       x = "Tempo(dias)", y = "Frequência")

# Outra forma de fazer o gráfico de barras 
dados4 <- data.frame(table(Tempo_dias))
ggplot(dados4, aes(x = Tempo_dias, y = Freq, fill = Freq)) + 
  geom_bar(stat='identity')

# Dividindo em subgrupos # Dividindo emFreq subgrupos 
ggplot(dados,aes(x = Tempo_dias)) + 
  geom_histogram(binwidth=2, fill = "darkgreen", color ="black", alpha = 0.5) + 
  labs(title="Distribuição do tempo de Permanência",
       x = "Tempo(dias)", y = "Frequência") + 
  facet_grid(sexo~Obito)

# Boxplot 
ggplot(dados,aes(y = Tempo_dias))+
  geom_boxplot(fill="darkblue", alpha=0.2) + 
  labs(title = "Boxplot do tempo em permanência",
       x="",y = "Tempo(dias)")  

ggplot(dados,aes(x = Obito, y = Tempo_dias))+
  geom_boxplot(fill="orange", alpha=0.2) + 
  labs(title = "Boxplot do tempo em permanência",
       x="Obito",y = "Tempo(dias)") 

ggplot(dados,aes(x = Obito, y = Peso))+
  geom_boxplot(fill="purple", alpha=0.2) + 
  labs(title = "Boxplot do ganho de peso por Obito",
       x="Obito",y = "Gramas")  

# Gráfico de dispersão 
ggplot(dados, aes(x = Tempo_dias, y = Peso)) + 
  geom_point(aes(colour = Obito,size = Peso, stroke = 1)) + 
  labs(title = "Gráfico de Dispersão do Tempo e Peso",
       x="Tempo(dias)",y = "Peso") + 
  theme(plot.title = element_text(hjust = 0.5))

# Dividindo em vários gráficos 
ggplot(dados, aes(x = Tempo_dias, y = Peso)) + 
  geom_point(color = "blue") + 
  labs(title = "Gráfico de Dispersão do Tempo e Peso",
       x="Tempo(dias)",y = "Peso") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  facet_grid(sexo~Obito)

# Estatística Descritiva 
length(Peso)   # Tamanho da amostra 
mean(Peso)     # Média 
median(Peso)   # Mediana 
round(quantile(Peso, probs=c(0.25,0.5,0.75)),2) # Quantil 
max(Peso)      # Máximo  
min(Peso)      # Mínimo 
sd(Peso)       # Desvio-padrão
summary(Peso)  # Summary - Sumariza todas as estatísticas 

# Coeficiente de Variação (CV)
cv <- sd(Peso)/mean(Peso)*100
cv

# Usando o tapply - Para relacionar duas variáveis 
tapply(Peso,Obito,length)
tapply(Peso,Obito,mean)
tapply(Peso,Obito,summary)
round(tapply(Peso,Obito, sd),2)













