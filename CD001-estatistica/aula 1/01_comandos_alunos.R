#O Sistema de Monitoramento de Fatores de Risco Comportamental 
#("Behavioral Risk Factor Surveillance System", BRFSS) é um survey 
# anual realizado por telefone com 350.000 pessoas nos Estados Unidos
#BRFSS foi desenvolvido para identificar fatores de risco na população 
#adulta e relatar tendências emergentes na saúde.
#Nós nos focaremos numa amostra aleatória de 20.000 pessoas do BRFSS conduzido em 2000.

source("http://www.openintro.org/stat/data/cdc.R")

#nome das variáveis
names(cdc)

#Exercício 1 Há quantos casos neste conjunto de dados? Quantas variáveis? 
#Para cada variável, identifique seu tipo de dado (p.e., categorial, discreta).
dim(cdc)

# R. 20.000 casos e 9 variáveis
# R. genhlth - categórica ordinal
# R. exerany - categórica nominal
# R. hlthplan - categórica nominal
# R. smoke100 - categórica nominal
# R. height - numérica contínua
# R. weight - numérica contínua
# R. wtdesire - numérica contínua
# R. age - numérica discreta
# R. gender - categórica nominal

#Definir variáveis 0/1 como categóricas
cdc$exerany<-as.factor(cdc$exerany)
cdc$hlthplan<-as.factor(cdc$hlthplan)
cdc$smoke100<-as.factor(cdc$smoke100)

#primeiras 6 linhas
head(cdc)

#últimas 6 linhas
tail(cdc)

#resumo dos 5 números e média
summary(cdc$weight)

#amplitude interquartílica
190 - 140

#medidas descritivas
mean(cdc$weight)
var(cdc$weight)
sd(cdc$weight)
median(cdc$weight)

#número de pessoas que fumaram 100 cigarros ao longo de sua vida
table(cdc$smoke100)

#distribuição de frequência relativa
table(cdc$smoke100)/20000

#gráfico de barras para as entradas na tabela
barplot(table(cdc$smoke100))

#ou utilizando a criação de um objeto antes
smoke <- table(cdc$smoke100)/20000
barplot(smoke)

#Exerício 2
#Crie um sumário numérico para height (altura) e age (idade), e 
#calcule o intervalo interquartílico para cada um. 
summary(cdc$height)
IQR(cdc$height)
70-64
summary(cdc$age)
IQR(cdc$age)
57-31
#Calcule a distribuição de frequência relativa para gender e exerany. 
table(cdc$gender)/20000
table(cdc$exerany)/20000
table(cdc$genhlth)/20000

#Quantos homens compõem a amostra? 
table(cdc$gender)
# R. 9569 homens
#Qual proporção da amostra diz estar com saúde excelente?
table(cdc$genhlth)/20000
# R. 23%

#tabela cruzada
table(cdc$gender,cdc$smoke100)
table(cdc$gender,cdc$smoke100)/20000

#gráfico mosaico
mosaicplot(table(cdc$gender,cdc$smoke100))

# Exercício 3 O que o gráfico de mosaico revela sobre os hábitos de fumar e gênero?
# R. A proporção de homens que fumam é maior que a de mulheres

#tamanho da base de dados
dim(cdc)

#acessar um subconjunto da base
#visualizar a sexta variável do 567o respondente
cdc[567,6]
cdc$weight[567]

#nomes das vaiáveis - variável 6 é peso
names(cdc)

#visualizar os pesos para os primeiros 10 respondentes
cdc[1:10,6]
cdc$weight[1:10]

#todos os dados dos 10 primeiros respondentes
cdc[1:10,]

#ver o peso de todos os 20.000
cdc[,6]
cdc$weight

#Formação de Subconjuntos
cdc$gender == "m"
cdc$age > 30

#extrair apenas os dados para homens na amostra
mdata <- subset(cdc, cdc$gender == "m")
head(mdata)

#extrair apenas os dados para homens E acima de 30 anos
m_and_over30 <- subset(cdc, cdc$gender == "m" & cdc$age > 30)

#extrair apenas os dados para homens OU acima de 30 anos
m_or_over30 <- subset(cdc, cdc$gender == "m" | cdc$age > 30)

#Exerício 4
#Crie um novo objeto denominado under23_and_smoke (ou, se preferir, abaixo23_e_fuma) que contém todas as 
#observações dos respondentes com menos de 23 anos que fumaram pelo menos 100 cigarros ao longo de sua vida.
under23_and_smoke <- subset(cdc, cdc$age < 23 & cdc$smoke100 == 1)


#gráfico de caixas - boxplot
boxplot(cdc$height)

#estatísticas sumárias - medidas descritivas
summary(cdc$height)

#boxplot para comparar as alturas de homens e mulheres
boxplot(cdc$height ~ cdc$gender)

#calcular Índice de Massa Corporal (IMC)
cdc$bmi <- (cdc$weight / cdc$height^2) * 703
boxplot(cdc$bmi ~ cdc$gender)

#boxplot do IMC
boxplot(cdc$bmi ~ cdc$genhlth)

# Exercício 5 O que este gráfico de caixas mostra? Escolha outra variável categorial do conjunto
# de dados e verifique como ela se relaciona ao IMC. Liste a variável que você escolheu, por que
# você pensou que ela poderia ter relação com o IMC e indique o que o gráfico parece sugerir.
# R. IMC aumenta com a piora da saúde
# R. Excolhi exerany - quem faz exercicio fisico, tem menor IMC.
#####boxplot(cdc$bmi ~ cdc$genhlth)
boxplot(cdc$bmi ~ cdc$exerany)
boxplot(cdc$bmi ~ cdc$genhlth)

#histograma da idade
hist(cdc$age)

#histograma do IMC
hist(cdc$bmi)
hist(cdc$bmi, breaks = 50)




#DESCRITIVAS - OUTROS PACOTES
install.packages("Hmisc")
library(Hmisc)
Hmisc::describe(cdc)


install.packages("DataExplorer")
library(DataExplorer)
introduce(cdc)
plot_intro(cdc)
plot_missing(cdc)
plot_histogram(cdc)
plot_bar(cdc)
plot_qq(cdc)
plot_correlation(cdc)
plot_scatterplot(cdc, by = "wtdesire")
plot_boxplot(cdc, by = "gender")


install.packages("gmodels")
library(gmodels)
CrossTable(cdc$smoke100, cdc$gender, digits=1, expected= T, prop.r=T, prop.c=T, prop.t=F, prop.chisq=F, asresid=T, chisq=T)



install.packages("Rcmdr")
library("Rcmdr")
command()


# SUA VEZ


