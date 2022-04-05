#O conjunto de dados Arbuthnot se refere ao Dr. John Arbuthnot, um m?dico, escritor e matem?tico do
#s?culo 18. Ele se interessou pela raz?o de meninos e meninas rec?m-nascidos, e para isso ele coletou os
#registros de batismo de crian?as nascidas em Londres todos os anos entre 1629 e 1710

source("http://www.openintro.org/stat/data/arbuthnot.R")

#mostra todo o banco de dados
arbuthnot

#n?mero de casos e vari?veis
dim(arbuthnot)

#nomes das vari?veis
names(arbuthnot)

#mostra dados de meninos
arbuthnot$boys

# exerc?cio1
#somente dados de meninas
arbuthnot$girls

#gr?fico do n?mero de meninas batizadas por ano
plot(x = arbuthnot$year, y = arbuthnot$girls)
plot(x = arbuthnot$year, y = arbuthnot$girls, type = "l")
plot(arbuthnot$year, arbuthnot$girls, type = "b")
?plot

#Exerc?cio 2 H? alguma tend?ncia aparente no n?mero de meninas batizadas ao longo dos
#anos? Como voc? a descreveria?
# R. Caiu bastante entre 1640 e 1660 e depois seguiu aumentando (Guerra Civil na Inglaterra)

#n?mero total de batismos
5218 + 4683  #em 1629
arbuthnot$boys + arbuthnot$girls
#criação de objetos
total<-arbuthnot$boys + arbuthnot$girls
total

#gr?fico com o total de batismos por ano
plot(arbuthnot$year, arbuthnot$boys + arbuthnot$girls, type = "l")
plot(arbuthnot$year, total, type = "l")

#raz?o entre meninos e meninas batizados
5218 / 4683  #em 1629
arbuthnot$boys / arbuthnot$girls
razao <- arbuthnot$boys / arbuthnot$girls
plot(arbuthnot$year, razao, type = "l")

#propor??o de rec?m-nascidos que s?o meninos
5218 / (5218 + 4683)  #em 1629
arbuthnot$boys / (arbuthnot$boys + arbuthnot$girls)

# exerc?cio3
#gr?fico das propor??es dos meninos com rela??o ao tempo
plot(x = arbuthnot$year, y = arbuthnot$boys / (arbuthnot$boys + arbuthnot$girls), type = "l")

#n?mero de meninos ? maior que de meninas em cada ano?
arbuthnot$boys > arbuthnot$girls




# SUA VEZ




