############################################################################
# Disciplina: Mineracao de Dados 
# Profa: Karin Becker
#
# Atividade Pratica: Laboratorio 03 - Mineracao de dados usando 
#                                     Agrupamento
#
# Conjunto de dados: Iris 
#
############################################################################

setwd("C:/Dev/github/data-science-ufrgs/CD002-mineracao-dados/06-estudo-agrupamento")

# Carrega biblioteca para leitura de arquivos
library(readr)

# Dataset Iris

#carregando o dataset original
iris <- read.csv("iris-lab03.csv", sep=";", dec=".", header=TRUE)
summary(iris)
boxplot(iris[,1:4],las=1)
barplot(table(iris$Species))

#criando um dataset equivalente, mas sem o rotulo
#copiar todos atributos de ??ris, exceto o ??ltimo
iris1 <- iris[-5]
summary(iris1)

# Visualizando as distancias entre os objetos
#
library(factoextra)
distance <- get_dist(iris1)
#Q1
fviz_dist(distance, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#
# Iris: Usando o K-means
#

# rodando o kmeans com 3 grupos, o melhor resultado de 10 e visualizando o resultado
kmi1 <- kmeans(iris1, centers=3, nstart = 10)
kmi1

#Q2
# acessando variaveis do modelo resultante
kmi1$centers
kmi1$size
kmi1$tot.withinss
kmi1$cluster

#
# analisando os resultados 
#

# plotando os resultados
#Q3
# plotando os agrupamentos de objetos, no caso de mais de duas variaveis, de acordo com seus componentes principais
fviz_cluster(kmi1, data = iris1)


# plotando os agrupamentos de objetos, no caso de mais de duas variaveis, de acordo com seus componentes principais
plot(iris1, col=kmi1$cluster, 
     main = paste("k-means clustering com 3 grupos; Total do SSE = ",kmi1$tot.withinss))
#Q4

##############################################

# visualizando a relacao do centro do cluster em relacao as petalas
plot(iris1[,c("Petal.Length", "Petal.Width")], 
     col = kmi1$cluster,
     main= "Agrupamento k-means com 3 grupos",
     xlab = "Petal.Length", ylab = "Petal.Width")
# plot cluster centers
points(kmi1$centers[,c("Petal.Length", "Petal.Width")], col = 1:3,
       pch = 8, cex=2)

# o centro em relacao aos comprimentos de Petala e Sepala
plot(iris1[c("Sepal.Length", "Petal.Length")], col = kmi1$cluster)
# plot cluster centers
points(kmi1$centers[,c("Sepal.Length", "Petal.Length")], col = 1:3,
       pch = 8, cex=2)

#
# 6. avaliando os resultados com indicadores externos
#
# gerando uma matriz de contingencia e avaliando o SSE intra-clusters e total
table(iris$Species, kmi1$cluster)

#Q5
###########################################################

kmi1$withinss
kmi1$tot.withinss

#Q6
###########################################################

#
# comparando agrupamentos com numero diferentes de grupos
#

## gerando agrupamentos com diferentes numeros de clusters
k2 <- kmeans(iris1, centers = 2, nstart=10)
k3 <- kmeans(iris1, centers = 3, nstart=10)
k4 <- kmeans(iris1, centers = 4, nstart=10)
k5 <- kmeans(iris1, centers = 5, nstart=10)

# visualizando os resultados 
p1 <- fviz_cluster(k2, geom = "point", data = iris1) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = iris1) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = iris1) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = iris1) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

#comparando as matrizes de contingencia e relacinando com os SSE dos clusters
table(iris$Species, k2$cluster)
k2$withinss
k2$tot.withinss

table(iris$Species, k3$cluster)
k3$withinss
k3$tot.withinss

table(iris$Species, k4$cluster)
k4$withinss
k4$tot.withinss

table(iris$Species, k5$cluster)
k5$withinss
k5$tot.withinss

# Q7
##################################################

# buscando o melhor numero de clusters
# Elbow method
set.seed(123)
fviz_nbclust(iris1, kmeans, method = "wss")

# Q8
##################################################


#
# Iris: agrupamento hierarquico
#

# testando sobre uma amostra
# seleciona alteatoriamente uma amostra de 50 flores
# remove o atributo Species
idx <- sample(1:dim(iris)[1], 50)
irisSample <- iris[idx,-5]

# cria um agrupamento hierarquico usando o metodo de medias
hc <- hclust(dist(irisSample), method="average")
# plota o dendograma
plot(hc, hang = -1, labels=iris$Species[idx])

# gerando grupos a partir da ??rvore
# plota o dendograma e divide arvore em tres particoes
plot(hc, hang = -1, labels=iris$Species[idx])
rect.hclust(hc, k=3, border = 2:4)
# plota o dendograma e divide arvore em cinco particoes
plot(hc, hang = -1, labels=iris$Species[idx])
rect.hclust(hc, k=5, border = 2:6)

# cria os grupos a partir de uma medida de corte (nro de grupos)
groupos3 <- cutree(hc,3)
groupos5 <- cutree(hc,5)

# avaliando os grupos pelo coeficiente silhoutte
library(cluster)
ss3 <- silhouette(groupos3,dist(irisSample))
ss5 <- silhouette(groupos5,dist(irisSample))

summary(ss3)
summary(ss5)

# Q9
##################################################

#
#testando diferentes medidas de similaridade entre grupos
# vamos testar diferentes metodos : "single", "complete", e "average"
#

# media de todos os pontos nos grupos
hc1 <- hclust(dist(iris1), method="average")
# grupos mais pr??ximos s??o definidos em termos dos pontos mais pr??ximos  - MIN
hc2 <- hclust(dist(iris1), method="single")
# grupos mais pr??ximos s??o definidos em termos dos pontos mais distantes  - MAX
hc3 <- hclust(dist(iris1), method="complete")

plot(hc1, hang = -1, labels=iris1$Species)
plot(hc2, hang = -1, labels=iris1$Species)
plot(hc3, hang = -1, labels=iris1$Species)

gruposavg <-cutree(hc1,3)
gruposmin <-cutree(hc2,3)
gruposmax <-cutree(hc3,3)

table(iris$Species, gruposavg)
table(iris$Species, gruposmin)
table(iris$Species, gruposmax)

# avaliando os grupos pelo coeficiente silhoutte
ssavg3 <- silhouette(gruposavg,dist(iris1))
ssmin3 <- silhouette(gruposmin,dist(iris1))
ssmax3 <- silhouette(gruposmax,dist(iris1))

summary(ssavg3)
summary(ssmin3)
summary(ssmax3)

