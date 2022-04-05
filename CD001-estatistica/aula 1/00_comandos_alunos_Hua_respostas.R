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
plot(x = arbuthnot$year,
     y = arbuthnot$girls,
     type = "l")
plot(arbuthnot$year, arbuthnot$girls, type = "b")
? plot

#Exerc?cio 2 H? alguma tend?ncia aparente no n?mero de meninas batizadas ao longo dos
#anos? Como voc? a descreveria?
# R. Caiu bastante entre 1640 e 1660 e depois seguiu aumentando (Guerra Civil na Inglaterra)

#n?mero total de batismos
5218 + 4683  #em 1629
arbuthnot$boys + arbuthnot$girls
#criação de objetos
total <- arbuthnot$boys + arbuthnot$girls
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
plot(
  x = arbuthnot$year,
  y = arbuthnot$boys / (arbuthnot$boys + arbuthnot$girls),
  type = "l"
)

#n?mero de meninos ? maior que de meninas em cada ano?
arbuthnot$boys > arbuthnot$girls

# Sua Vez - Laboratório 00

# Nas páginas anteriores, você recriou algumas das exposições e análises preliminares dos dados de batismo
# de Arbuthnot. Sua tarefa consiste repetir essas etapas, mas para os registros atuais de nascimento dos
# Estados Unidos. Carregue os dados atuais com o seguinte comando.
source("http://www.openintro.org/stat/data/present.R")

# Os dados serão armazenados num banco de dados chamado present.

# 1. Quais anos estão incluídos neste conjunto de dados? Quais são as dimensões da base de dados e
# quais são os nomes das colunas ou variáveis?
present # visualiza dados 
dim(present) # exibe dimensões
names(present) # exibe nomes das colunas
## R. A base de dados apresenta informações sobre os anos de 1940 até 2002. 
## A base de dados apresenta 63 linhas e 3 colunas, que são: "year", "boys", "girls"

#   2. Como estas contagens se comparam aos dados de Arbuthnot? Eles estão numa escala similar?
## R. Apesar de apresentarem um período parecido em quantidade de anos, a ordem de grandeza dos números de nascimentos é muito maior na base de dados "present".

#   3. A observação de Arbuthnot de que os meninos nascem numa proporção maior que as meninas se
# mantém nos EUA?
present$boys > present$girls
## R. Sim. O número de nascimentos de meninos é maior que o de meninas durante todo o período.

#   4. Crie um gráfico que mostre a razão de meninos para meninas para cada ano do conjunto de dados.
# O que você pode verificar?
razao <- present$boys / present$girls
plot(present$year, razao, type = "l")

## R. A razão entre menino e meninas tem diminuído ao longo dos anos.

#   5. Em qual ano se verifica o maior número de nascimentos nos EUA? Você pode utilizar os arquivos de
# ajuda ou o cartão de referência do R (http://cran.r-project.org/doc/contrib/Short-refcard.pdf ) para encontrar
# comandos úteis.
soma <- present$boys + present$girls
plot(present$year, soma, type = "l")
max(soma) # valor mais alto
## R. 4268326

