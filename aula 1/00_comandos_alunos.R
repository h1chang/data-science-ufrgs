#O conjunto de dados Arbuthnot se refere ao Dr. John Arbuthnot, um médico, escritor e matemático do
#século 18. Ele se interessou pela razão de meninos e meninas recém-nascidos, e para isso ele coletou os
#registros de batismo de crianças nascidas em Londres todos os anos entre 1629 e 1710

source("http://www.openintro.org/stat/data/arbuthnot.R")

#mostra todo o banco de dados
arbuthnot

#número de casos e variáveis
dim(arbuthnot)

#nomes das variáveis
names(arbuthnot)

#mostra dados de meninos
arbuthnot$boys

# exercício1
#somente dados de meninas
arbuthnot$girls

#gráfico do número de meninas batizadas por ano
plot(x = arbuthnot$year, y = arbuthnot$girls)
plot(x = arbuthnot$year, y = arbuthnot$girls, type = "l")
plot(arbuthnot$year, arbuthnot$girls, type = "b")
?plot

#Exercício 2 Há alguma tendência aparente no número de meninas batizadas ao longo dos
#anos? Como você a descreveria?
# R. Caiu bastante entre 1640 e 1660 e depois seguiu aumentando (Guerra Civil na Inglaterra)

#número total de batismos
5218 + 4683  #em 1629
arbuthnot$total<-arbuthnot$boys + arbuthnot$girls

#gráfico com o total de batismos por ano
plot(arbuthnot$year, arbuthnot$boys + arbuthnot$girls, type = "l")
plot(arbuthnot$year, arbuthnot$total, type = "l")


#razão entre meninos e meninas batizados
5218 / 4683  #em 1629
arbuthnot$boys / arbuthnot$girls

#proporção de recém-nascidos que são meninos
5218 / (5218 + 4683)  #em 1629
arbuthnot$boys / (arbuthnot$boys + arbuthnot$girls)

# exercício3
#gráfico das proporções dos meninos com relação ao tempo
plot(x = arbuthnot$year, y = arbuthnot$boys / (arbuthnot$boys + arbuthnot$girls), type = "l")

#número de meninos é maior que de meninas em cada ano?
arbuthnot$boys > arbuthnot$girls




# SUA VEZ




