# Operações Básicas (Soma, Divisão e Potênciação)
2 + 2 
2/3  
2^3   

# Número em notação científica 
1/10000

# Raiza quadrada de 25
sqrt(25)

# Exponencial com base Neperiana 
exp(2)

# Construção de objetos 
x <- 2 
x*3 

# Vetores (Conjunto de números)
y <- c(20,15,13,34)
y[1:2] <- 30
y[3:4] <- 15
y*10 

# Matriz com números 3 x 2 
M <- c(3,4,2,1,7,3)
MA <- matrix(M,nrow=3,ncol=2)
MA 

# Somando um valor a todos os elementos
MA + 2 

# Strings 
nomes <- c("Ana","Beatriz","Carlos","Daniel") 

# Importação do banco de dados
library(readxl)
dadosR <- read_excel("Documentos/Disciplinas/Análise no R/Aula 1 /Exemplo.xlsx", 
                      sheet = "DadosR")
# Verificar a tabela de dados 
View(dadosR)

# Cabeçalho e Calda do banco de dados
head(dadosR)
tail(dadosR)

# Escolhendo as variáveis
dadosR$Nome[1:10]
dadosR

# Análise descritiva dos dados 
dadosR$Sexo

# Disponibilizar as variáveis no R
attach(dadosR)
Sexo

# Tabela da quant. de F, M e I
table(Sexo)

# Ordenando as variáveis Qualitativas Nominais
Sexo <- ordered(Sexo, levels=c("F","M","I"))
tabela1 <- table(Sexo) 

# Proporção e Proporção em porcentagem 
prop.table(tabela1)
round(prop.table(tabela1)*100,1) # Arredondando o valor 

# Análise de duas variáveis
tabela2 <- table(Sexo,Destino)
round(prop.table(tabela2)*100,2)

# Proporção por linha(margin=1) e coluna(margin=2)
round(prop.table(tabela2,margin=1)*100,2)
round(prop.table(tabela2,margin=2)*100,2)

# Estatística Descritiva das variáveis Quantitativas 
length(Tempo_dias)          # Tamanho da amostra 
min(Tempo_dias)             # Mínimo
max(Tempo_dias)             # Máximo
round(mean(Tempo_dias),2)   # Média
round(sd(Tempo_dias),2)     # Desvio-padrão 






















































