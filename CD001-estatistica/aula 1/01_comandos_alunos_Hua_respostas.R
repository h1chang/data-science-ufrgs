#O Sistema de Monitoramento de Fatores de Risco Comportamental 
#("Behavioral Risk Factor Surveillance System", BRFSS) ? um survey 
# anual realizado por telefone com 350.000 pessoas nos Estados Unidos
#BRFSS foi desenvolvido para identificar fatores de risco na popula??o 
#adulta e relatar tend?ncias emergentes na sa?de.
#N?s nos focaremos numa amostra aleat?ria de 20.000 pessoas do BRFSS conduzido em 2000.

source("http://www.openintro.org/stat/data/cdc.R")

#nome das vari?veis
names(cdc)

#Exerc?cio 1 H? quantos casos neste conjunto de dados? Quantas vari?veis? 
#Para cada vari?vel, identifique seu tipo de dado (p.e., categorial, discreta).
dim(cdc)

# R. 20.000 casos e 9 vari?veis
# R. genhlth - categ?rica ordinal - saúde geral
# R. exerany - categ?rica nominal - se exercitou no último mês
# R. hlthplan - categ?rica nominal - cobertura plano de saúde
# R. smoke100 - categ?rica nominal - fumou pelo menos 100 cigarros
# R. height - num?rica cont?nua - altura
# R. weight - num?rica cont?nua - peso atual
# R. wtdesire - num?rica cont?nua - peso desejado
# R. age - num?rica discreta
# R. gender - categ?rica nominal

#primeiras 6 linhas
head(cdc)

#?ltimas 6 linhas
tail(cdc)

#resumo dos 5 n?meros e m?dia
summary(cdc$weight)
# 3quartil = 190
# 1quartil = 140

#amplitude interquart?lica
# 3quartil - 1quartil
190 - 140

#medidas descritivas
mean(cdc$weight) # media
var(cdc$weight) # variância
sd(cdc$weight) #devio padrão
median(cdc$weight) # mediana

#n?mero de pessoas que fumaram 100 cigarros ao longo de sua vida
table(cdc$smoke100)

#distribui??o de frequ?ncia relativa
table(cdc$smoke100)/20000

#gr?fico de barras para as entradas na tabela
barplot(table(cdc$smoke100))

#ou utilizando a cria??o de um objeto antes
smoke <- table(cdc$smoke100)
barplot(smoke)

#Exer?cio 2
#Crie um sum?rio num?rico para height (altura) e age (idade), e 
#calcule o intervalo interquart?lico para cada um. 
summary(cdc$height)
IQR(cdc$height)
70-64
summary(cdc$age)
IQR(cdc$age)
57-31
#Calcule a distribui??o de frequ?ncia relativa para gender e exerany. 
table(cdc$gender)/20000
table(cdc$exerany)/20000
table(cdc$genhlth)/20000

#Quantos homens comp?em a amostra? 
table(cdc$gender) # tabela frequencia
# R. 9569 homens

#Qual propor??o da amostra diz estar com sa?de excelente?
table(cdc$genhlth)/20000 # tabela frequencia relativa
# R. 23%

#tabela cruzada
table(cdc$gender,cdc$smoke100)
table(cdc$gender,cdc$smoke100)/20000

#gr?fico mosaico
mosaicplot(table(cdc$gender,cdc$smoke100))

# Exerc?cio 3 O que o gr?fico de mosaico revela sobre os h?bitos de fumar e g?nero?
# R. A propor??o de homens que fumam ? maior que a de mulheres

#tamanho da base de dados
dim(cdc)

#acessar um subconjunto da base
#visualizar a sexta vari?vel do 567o respondente
cdc[567,6]
cdc$weight[567]

#nomes das vai?veis - vari?vel 6 ? peso
names(cdc)

#visualizar os pesos para os primeiros 10 respondentes
cdc[1:10,6]
cdc$weight[1:10]

#todos os dados dos 10 primeiros respondentes
cdc[1:10,]

#ver o peso de todos os 20.000
cdc[,6]
cdc$weight

#Forma??o de Subconjuntos
cdc$gender == "m"
cdc$age > 30

#extrair apenas os dados para homens na amostra
mdata <- subset(cdc, cdc$gender == "m")
head(mdata)

#extrair apenas os dados para homens E acima de 30 anos
m_and_over30 <- subset(cdc, cdc$gender == "m" & cdc$age > 30)

#extrair apenas os dados para homens OU acima de 30 anos
m_or_over30 <- subset(cdc, cdc$gender == "m" | cdc$age > 30)

#Exer?cio 4
#Crie um novo objeto denominado under23_and_smoke (ou, se preferir, abaixo23_e_fuma) que cont?m todas as 
#observa??es dos respondentes com menos de 23 anos que fumaram pelo menos 100 cigarros ao longo de sua vida.
under23_and_smoke <- subset(cdc, cdc$age < 23 & cdc$smoke100 == 1)


#gr?fico de caixas - boxplot
boxplot(cdc$height)

#estat?sticas sum?rias - medidas descritivas
summary(cdc$height)

#boxplot para comparar as alturas de homens e mulheres
boxplot(cdc$height ~ cdc$gender)

#calcular ?ndice de Massa Corporal (IMC)
bmi <- (cdc$weight / cdc$height^2) * 703

#boxplot do IMC
boxplot(bmi ~ cdc$genhlth)

# Exerc?cio 5 O que este gr?fico de caixas mostra? Escolha outra vari?vel categorial do conjunto
# de dados e verifique como ela se relaciona ao IMC. Liste a vari?vel que voc? escolheu, por que
# voc? pensou que ela poderia ter rela??o com o IMC e indique o que o gr?fico parece sugerir.
# R. IMC aumenta com a piora da sa?de
# R. Excolhi exerany - quem faz exercicio físico, tem menor IMC.
boxplot(bmi ~ cdc$exerany)
boxplot(bmi ~ cdc$genhlth)


#histograma da idade
hist(cdc$age)

#jistograma do IMC
hist(bmi)
hist(bmi, breaks = 50)




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
# Sua Vez - Laboratório 01

source("http://www.openintro.org/stat/data/cdc.R")

# 1. Crie um gráfico de dispersão da variável peso em relação ao peso desejado. Defina a relação entre
# essas duas variáveis.

install.packages("DataExplorer")
library(DataExplorer)
cdc_weight <- cdc[, c("weight", "wtdesire")] # subconjunto somente com colunas interessadas
plot_scatterplot(cdc_weight, by = "wtdesire")

## R. Observando o eixo x = y, é possível observar que os pesos desejados são na maioria das vezes menores que os pesos reais.


# 2. Vamos considerar uma nova variável: a diferença entre o peso desejado (wtdesire) e o peso atual
# (weight). Crie esta nova variável subtraindo as duas colunas na base de dados e atribuindo-as a um
# novo objeto chamado wdiff.
wdiff <- cdc$wtdesire - cdc$weight

## R. Criada a variável wdiff

# 3. Que tipo de dado está contido na variável wdiff? Se uma observação de wdiff é 0, o que isso implica
# com relação ao peso atual e desejado de uma pessoas? E se o valor de wdiff for positivo ou negativo?
summary(wdiff)
## R. a variável 'wdiff' representa o peso que a pessoa deseja ganhar ou perder para atingir o peso ideal.
## O valor positivo indica indica que a pessoa deseja ganhar peso, e no caso negativo perder peso.

# 4. Descreva a distribuição de wdiff em termos de seu centro, forma e variação, incluindo qualquer
# gráfico que você usar. O que isso nos diz sobre como as pessoas se sentem a respeito do seu peso
# atual?
plot(wdiff) # gráfico de dispersão
summary(wdiff) # resumo
boxplot(wdiff) 
## R. No gráfico de dispersão, notamos a maioria dos valores estão abaixo do eixo wdiff = 0, indicando qua maioria dos casos o desejo é perder peso. No boxplot. a proximidade dos quartis (Q1 e Q3) da mediana indica pouca variância entre os valores de perda de peso desejado.

#   5. Utilizando sumários numéricos e um gráfico de caixas lado-a-lado, determine se homens tendem a ver seu peso diferentemente das mulheres.
man_weight <- subset(cdc, cdc$gender == "m")
woman_weight <- subset(cdc, cdc$gender == "f")

man_wdiff <- man_weight$wtdesire - man_weight$weight
woman_wdiff <- woman_weight$wtdesire - woman_weight$weight

summary(man_wdiff)
summary(woman_wdiff)

boxplot(man_wdiff, woman_wdiff, names=c("man","woman"), outline = FALSE)

## R. Analisando os valores de média e mediana, é possível perceber que os homens tem em média desejo de perder menos peso que as mulheres. E analisando no gráfico de caixaso valor do 1Quartil das mulheres e menos que o dos homens indicando que as mulheres desejam perder mais peso que os homens.

# 6. Agora chegou a hora de usar a criatividade. Encontre a média e o desvio padrão de weight e determine qual a proporção de pesos que estão a um desvio padrão da média.
weight <- cdc$weight
summary(weight) # média, mediana, quartis, etc.
mean(weight)
media <- mean(weight)

sd(weight) # desvio padrão
weight_desviopadrao <- sd(weight)

media - weight_desviopadrao # limite inferior
media + weight_desviopadrao # limite superior


plot(weight) # observando os dados
abline(h=media - weight_desviopadrao, col="red") # exibe linha limite inferior
abline(h=media + weight_desviopadrao, col="red") # exibe linha limite superior

elementos_dentro_faixa <- subset(cdc, cdc$weight >= media - weight_desviopadrao & cdc$weight <= media + weight_desviopadrao) # elementos >= media - desvio & <= media + desvio
dim(elementos_dentro_faixa) # 14152
14152/20000 # faixa / total = %

## R.  Média = 169.7. Desvio padrão = 40.08097.
# Elementos dentro da faixa = 70.76%

