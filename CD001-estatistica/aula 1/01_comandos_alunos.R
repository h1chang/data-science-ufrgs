#O Sistema de Monitoramento de Fatores de Risco Comportamental 
#("Behavioral Risk Factor Surveillance System", BRFSS) � um survey 
# anual realizado por telefone com 350.000 pessoas nos Estados Unidos
#BRFSS foi desenvolvido para identificar fatores de risco na popula��o 
#adulta e relatar tend�ncias emergentes na sa�de.
#N�s nos focaremos numa amostra aleat�ria de 20.000 pessoas do BRFSS conduzido em 2000.

source("http://www.openintro.org/stat/data/cdc.R")

#nome das vari�veis
names(cdc)

#Exerc�cio 1 H� quantos casos neste conjunto de dados? Quantas vari�veis? 
#Para cada vari�vel, identifique seu tipo de dado (p.e., categorial, discreta).
dim(cdc)

# R. 20.000 casos e 9 vari�veis
# R. genhlth - categ�rica ordinal
# R. exerany - categ�rica nominal
# R. hlthplan - categ�rica nominal
# R. smoke100 - categ�rica nominal
# R. height - num�rica cont�nua
# R. weight - num�rica cont�nua
# R. wtdesire - num�rica cont�nua
# R. age - num�rica discreta
# R. gender - categ�rica nominal

#Definir vari�veis 0/1 como categ�ricas
cdc$exerany<-as.factor(cdc$exerany)
cdc$hlthplan<-as.factor(cdc$hlthplan)
cdc$smoke100<-as.factor(cdc$smoke100)

#primeiras 6 linhas
head(cdc)

#�ltimas 6 linhas
tail(cdc)

#resumo dos 5 n�meros e m�dia
summary(cdc$weight)

#amplitude interquart�lica
190 - 140

#medidas descritivas
mean(cdc$weight)
var(cdc$weight)
sd(cdc$weight)
median(cdc$weight)

#n�mero de pessoas que fumaram 100 cigarros ao longo de sua vida
table(cdc$smoke100)

#distribui��o de frequ�ncia relativa
table(cdc$smoke100)/20000

#gr�fico de barras para as entradas na tabela
barplot(table(cdc$smoke100))

#ou utilizando a cria��o de um objeto antes
smoke <- table(cdc$smoke100)/20000
barplot(smoke)

#Exer�cio 2
#Crie um sum�rio num�rico para height (altura) e age (idade), e 
#calcule o intervalo interquart�lico para cada um. 
summary(cdc$height)
IQR(cdc$height)
70-64
summary(cdc$age)
IQR(cdc$age)
57-31
#Calcule a distribui��o de frequ�ncia relativa para gender e exerany. 
table(cdc$gender)/20000
table(cdc$exerany)/20000
table(cdc$genhlth)/20000

#Quantos homens comp�em a amostra? 
table(cdc$gender)
# R. 9569 homens
#Qual propor��o da amostra diz estar com sa�de excelente?
table(cdc$genhlth)/20000
# R. 23%

#tabela cruzada
table(cdc$gender,cdc$smoke100)
table(cdc$gender,cdc$smoke100)/20000

#gr�fico mosaico
mosaicplot(table(cdc$gender,cdc$smoke100))

# Exerc�cio 3 O que o gr�fico de mosaico revela sobre os h�bitos de fumar e g�nero?
# R. A propor��o de homens que fumam � maior que a de mulheres

#tamanho da base de dados
dim(cdc)

#acessar um subconjunto da base
#visualizar a sexta vari�vel do 567o respondente
cdc[567,6]
cdc$weight[567]

#nomes das vai�veis - vari�vel 6 � peso
names(cdc)

#visualizar os pesos para os primeiros 10 respondentes
cdc[1:10,6]
cdc$weight[1:10]

#todos os dados dos 10 primeiros respondentes
cdc[1:10,]

#ver o peso de todos os 20.000
cdc[,6]
cdc$weight

#Forma��o de Subconjuntos
cdc$gender == "m"
cdc$age > 30

#extrair apenas os dados para homens na amostra
mdata <- subset(cdc, cdc$gender == "m")
head(mdata)

#extrair apenas os dados para homens E acima de 30 anos
m_and_over30 <- subset(cdc, cdc$gender == "m" & cdc$age > 30)

#extrair apenas os dados para homens OU acima de 30 anos
m_or_over30 <- subset(cdc, cdc$gender == "m" | cdc$age > 30)

#Exer�cio 4
#Crie um novo objeto denominado under23_and_smoke (ou, se preferir, abaixo23_e_fuma) que cont�m todas as 
#observa��es dos respondentes com menos de 23 anos que fumaram pelo menos 100 cigarros ao longo de sua vida.
under23_and_smoke <- subset(cdc, cdc$age < 23 & cdc$smoke100 == 1)


#gr�fico de caixas - boxplot
boxplot(cdc$height)

#estat�sticas sum�rias - medidas descritivas
summary(cdc$height)

#boxplot para comparar as alturas de homens e mulheres
boxplot(cdc$height ~ cdc$gender)

#calcular �ndice de Massa Corporal (IMC)
cdc$bmi <- (cdc$weight / cdc$height^2) * 703
boxplot(cdc$bmi ~ cdc$gender)

#boxplot do IMC
boxplot(cdc$bmi ~ cdc$genhlth)

# Exerc�cio 5 O que este gr�fico de caixas mostra? Escolha outra vari�vel categorial do conjunto
# de dados e verifique como ela se relaciona ao IMC. Liste a vari�vel que voc� escolheu, por que
# voc� pensou que ela poderia ter rela��o com o IMC e indique o que o gr�fico parece sugerir.
# R. IMC aumenta com a piora da sa�de
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


