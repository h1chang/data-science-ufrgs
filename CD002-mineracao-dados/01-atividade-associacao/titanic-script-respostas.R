############################################################################
# Disciplina: Mineração de Dados
# Profa: Karin Becker
#
# Atividade Pratica: Estudo Dirigido 01 - 
#                    Mineracao de dados usando regras de associacao
#                                     
##
############################################################################

# Voce precisa o pacote arules e arulesViz. Use as funcoes do ambiente para instala-los.
#
# 
# Carregar o arquivo de dados
# Pressupoe que voce já definiu seu diretorio de trabalho (Session/Set Working Directory/seudiretorio).
# setwd("/cloud/project/titanic")
load("titanic.raw.rdata")

# Visualizando conjunto de dados
View(titanic.raw)

# Analisando a estrutura do objeto titanic criado
str(titanic.raw)

# Realizando analise estatistica sobre os dados
summary(titanic.raw)

#Q1: Informe o número de registros do arquivo, o número de atributos, e o tipo de dado de cada atributo?
## R. O conjunto de dados possui 2201 registros.
## Com os seguintes atributos:
## Class - categórica com valores: "1st", "2nd, 3rd",  "Crew"
## Sex - categórica com valores: "Female", "Male"
## Age - categórica com valores:  "Adult", "Child"
## Survived - categórica com valores: "No", "Yes"
  

#Q2: Diga pelo menos dois fatos que o summary revelou sobre os passageiros do Titanic.
## R.
## 1) Havia muito mais adultos do que crianças no navio
## 2) Havia muito mais homens do que mulheres no navio.

# Visualizando a distribuicao dos passageiros por classe
plot(titanic.raw$Class, main="Histograma - Distribuicao dos passageiros em classes", 
     xlab="Classes", ylab="Total passageiros",
     ylim=c(0,1000), las=0)

# Visualizando a distribuicao dos passageiros por idade
plot(titanic.raw$Age, main="Histograma - Distribuicao dos passageiros por idade", 
     xlab="Idade", ylab="Total passageiros",
     ylim=c(0,2000), las=0)


#se desejar, pode tentar outras visualizacoes, mas nao e o ponto deste estudo dirigido.

#
# Carregando biblioteca para usar regras de associacao
#
library("arules")

# Encontrando regras de associacao com configuracao padrao da funcao
rules.all <- apriori(titanic.raw)

# Inspecionando as regras de associacao geradas
inspect(rules.all)

#Q3: Quantas regras o algoritmo apriori encontrou para este conjunto de dados? Analise a saída, e veja qual o valor de suporte e de confiança mínimos adotados pelo algoritmo apriori (parâmetros default)
## R. O algoritmo apriori encontrou 27 regras de associação no conjunto de dados

## Analisando a saída:
## Parameter specification:
## confidence minval smax arem  aval originalSupport maxtime support minlen maxlen target  ext
## 0.8    0.1    1 none FALSE            TRUE       5     0.1      1     10  rules TRUE

## o mínimo valor (minlen)  de suporte (Support) é 1
## o mínimo valor (minval) de confiança (Confidence) é 0.1


# Ordenando as regras por suporte e inspecionando novamente o resultado
rules.sorted <- sort(rules.all, by="support")
inspect(rules.sorted)

#Q4: Observe as duas primeiras regras listadas após o ordenamento. Você considera que essas regras são uma associação relevante para responder sua pergunta de negócio? Justifique sua resposta.
#Q5: Expresse em português como você lê as regras [2] e [3], incluindo a interpretação do suporte e da confiança.
#Q6: Analise as demais regras, e selecione uma que você considera relevante em relação ao problema de negócio definido. Informe o número da regra escolhida, e justifique sua escolha considerando as métricas de suporte e confiança. 

# Ordenando as regras por confianca e inspecionando novamente o resultado
rules.sorted <- sort(rules.all, by="confidence")
inspect(rules.sorted)
#Q7: Observe as quatro primeiras regras listadas após o ordenamento por confiança. Você considera que, porque estas regras têm confiança máxima, elas são mais relevantes para sua questão de negócio? Justifique sua resposta.

# Executando o apriori com suporte e confianca de 90% e inspecinando as regras de associacao ordenadas
rules.all <- apriori(titanic.raw, parameter = list(supp=0.9, conf=0.9))
rules.sorted <- sort(rules.all, by="support")
inspect(rules.sorted)

#Q8: Para suporte e confiança iguais a 90%, alguma regra relevante foi retornada? 

# Reduzindo o suporte e confianca para 10% e 30%, respectivamente
rules.all <- apriori(titanic.raw, parameter = list(supp=0.1, conf=0.3))
rules.sorted <- sort(rules.all, by="support")
inspect(rules.sorted)

# Restringindo a busca de regras de associacao para retornar somente
# regras que contenham o atributo "sobrevivente"
# usando valores intermediarios para suporte e confianca
rules.all <- apriori(titanic.raw, control = list(verbose=F),
                     parameter = list(minlen=2, supp=0.3, conf=0.6),
                     appearance = list(rhs=c("Survived=No", "Survived=Yes"), default="lhs"))
quality(rules.all) <- round(quality(rules.all), digits=2)
rules.sorted <- sort(rules.all, by="support")
inspect(rules.sorted)

#Q9: Baixando o suporte e confiança mínimos, apareceram mais regras? 
#Q10: As regras [1] a [5] são relevantes para sua questão de negócio? Explique sua resposta.
#
# Re-executando com suporte mais baixo, e Removendo regras redundantes
#

rules.all <- apriori(titanic.raw, control = list(verbose=F),
                     parameter = list(minlen=2, supp=0.05, conf=0.6),
                     appearance = list(rhs=c("Survived=No", "Survived=Yes"), default="lhs"))
quality(rules.all) <- round(quality(rules.all), digits=3)
rules.sorted <- sort(rules.all, by="support")
inspect(rules.sorted)

#Q11: As regras resultantes parecem ser orientadas ao problema de negócio? Explique sua resposta.
#Q12: Por que não temos regras que explicam a sobrevivência? Que parâmetro você poderia alterar para conseguir regras que expliquem a sobrevivência?
#Q13: Existem regras que lhe parecem redundantes entre si? Quais?


# Encontrando e removendo as regras duplicadas (mesmo procedimento acima)
subset.matrix <- is.subset(rules.sorted, rules.sorted, sparse = FALSE)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

#Q14: Comparadas com as regras da execução anterior (Q11), cite dois exemplos distintos de redundâncias que foram eliminadas? Explique sua resposta em termo dos itens que aparecem nas regras,  e suas métricas

## Usando o lift para avaliar as regras
## Quando o lift e 
# - > 1, a presenta do valor a esquerda aumenta a probabilidade de ocorrencia do valor a direita (positivamente correlacionados).
# - = 1, a ocorrencia do valor a esquerda nao afeta a ocorrencia do valor a direita (estatisticamente independentes).
# - < 1, a ocorrencia do valor a esquerda diminui a probabilidade de ocorrencia do valor a direita (negativamente correlacionados).
rules.all <- apriori(titanic.raw, control = list(verbose=F),
                     parameter = list(minlen=2,supp=0.01,conf=0.5),
                     appearance = list(rhs=c("Survived=No", 
                                             "Survived=Yes"), 
                                       default="lhs"))
quality(rules.all) <- round(quality(rules.all), digits=3)
rules.sorted <- sort(rules.all, by="lift")
# Eliminar as regras duplicadas
subset.matrix <- is.subset(rules.sorted,rules.sorted,sparse=FALSE)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules.sorted[!redundant]
# Examine os resultados
inspect(rules.pruned)

#Q15: Compare as regras [4] e [17]. Observe que têm confianças parecidas, mas lifts distintos. O que estas regras nos dizem sobre a sobrevivência?
#Q16: Compare as regras [3] e [18]. Embora elas tenham confiança um pouco distintas, o que os respectivos lifts nos dizem sobre a não sobrevivência?
#Q17: Com base nestas regras, liste 5 coisas que podem ser inferidas dos dados sobre sobreviventes e não sobreviventes do Titanic com o uso de regras de associação
#Q18: crianças da segunda classe tiveram mais chances de sobrevivência do que as da primeira classe?

# Interpretando regras de sobrevivencia de criancas
rules <- apriori(titanic.raw,
                 parameter = list(minlen=3, supp=0.002, conf=0.3),
                 appearance = list(rhs=c("Survived=Yes", "Survived=No"),
                                   lhs=c("Class=1st", "Class=2nd", 
                                         "Class=3rd",
                                         "Age=Child", "Age=Adult"),
                                   default="none"),
                 control = list(verbose=F))
rules.sorted <- sort(rules, by="confidence")
inspect(rules.sorted)
#Q19: Responda novamente à questão Q18 utilizando as métricas de suporte/confiança para justificar sua reposta.
#Q20: E as crianças da terceira classe? Como sua situação se compara (a) à das demais crianças? (b) dos adultos da primeira classe? 

#
# explorando recursos de visualizacao
#
library(arulesViz)
#geracao de regras 
rules.all <- apriori(titanic.raw, control = list(verbose=F),
                     parameter = list(minlen=2,supp=0.005,conf=0.2),
                     appearance = list(rhs=c("Survived=No", 
                                             "Survived=Yes"), 
                                       default="lhs"))
quality(rules.all) <- round(quality(rules.all), digits=2)
rules.sorted <- sort(rules.all, by="lift")
# Eliminar as regras duplicadas
subset.matrix <- is.subset(rules.sorted,rules.sorted,sparse=FALSE)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules.sorted[!redundant]
# Examine os resultados
inspect(rules.pruned)

# Scatterplot por lift/confianca
#Plot SubRules
# plot estatico
plot(rules.pruned)
# plot dinamico
plot(rules.pruned,engine = "htmlwidget")

#Q20: Qual a regra com suporte mais alto?  Qual suporte resultaria em regras com maior confiança?

# Selecionar apenas um conjunto de regras
subrules<-rules.pruned[quality(rules.pruned)$confidence>0.5]
inspect(subrules)

# plot na forma de gr??fos (limitado a 100 regras)
# plot estatico
plot(subrules, method = "graph")
# plot dinamico
plot(subrules, method = "graph",  engine = "htmlwidget")


