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
?apriori

# Inspecionando as regras de associacao geradas
inspect(rules.all)

#Q3: Quantas regras o algoritmo apriori encontrou para este conjunto de dados? Analise a saída, e veja qual o valor de suporte e de confiança mínimos adotados pelo algoritmo apriori (parâmetros default)
## R. O algoritmo apriori encontrou 27 regras de associação no conjunto de dados.

## Analisando a saída:
## Parameter specification:
## confidence minval smax arem  aval originalSupport maxtime support minlen maxlen target  ext
## 0.8    0.1    1 none FALSE            TRUE       5     0.1      1     10  rules TRUE

## o valor default mínimo de suporte é 0.1
## o valor default mínimo de confiança é 0.8


# Ordenando as regras por suporte e inspecionando novamente o resultado
rules.sorted <- sort(rules.all, by="support")
inspect(rules.sorted)

#Q4: Observe as duas primeiras regras listadas após o ordenamento. Você considera que essas regras são uma associação relevante para responder sua pergunta de negócio? Justifique sua resposta.
# lhs                                     rhs           support   confidence coverage  lift      count
# [1]  {}                                   => {Age=Adult}   0.9504771 0.9504771  1.0000000 1.0000000 2092 
# [2]  {Sex=Male}                           => {Age=Adult}   0.7573830 0.9630272  0.7864607 1.0132040 1667
# Não, as duas primeiras regras demonstram fatos óbvios que já tinham sido observados no resumo (summary) da base de dados.


#Q5: Expresse em português como você lê as regras [2] e [3], incluindo a interpretação do suporte e da confiança.
# [2]  {Sex=Male}                           => {Age=Adult}   0.7573830 0.9630272  0.7864607 1.0132040 1667 
# [3]  {Survived=No}                        => {Age=Adult}   0.6533394 0.9651007  0.6769650 1.0153856 1438
# Regra 2: Quando se encontra um tripulante do sexo masculino (Sex=Male) em uma ocorrência, é esperado que ele seja adulto (Age=Adult),
# com valor de suporte 0.76 e confiança 0.96. 
# Suporte 0.76, indica que em 76% de todas transações encontradas na base se encontra {Sex=Male} e {Age=Adult}.
# Confiança 0.96, indica que em 96% das transações que se encontra  {Age=Adult}, também se encontra {Sex=Male}.

# Regra 3: Quando se encontra um tripulante não sobrevivente (Survived=No) em uma ocorrência, é esperado que ele seja adulto (Age=Adult),
# com valor de suporte 0.65 e confiança 0.96. 
# Suporte 0.65, indica que em 65% de todas transações encontradas na base se encontra {Survived=No} e {Age=Adult}.
# Confiança 0.96, indica que em 96% das transações que se encontra  {Survived=No}, também se encontra {Sex=Male}.

#Q6: Analise as demais regras, e selecione uma que você considera relevante em relação ao problema de negócio definido. Informe o número da regra escolhida, e justifique sua escolha considerando as métricas de suporte e confiança. 
# [20] {Class=3rd, Sex=Male}                => {Survived=No} 0.1917310 0.8274510  0.2317129 1.2222950  422 
# A regra 20, indica que em 83% dos casos em que uma pessoa não sobreviveu (Survived=No), ele era homem (Sex=Male) e da terceira classe (Class=3rd).


# Ordenando as regras por confianca e inspecionando novamente o resultado
rules.sorted <- sort(rules.all, by="confidence")
inspect(rules.sorted)
#Q7: Observe as quatro primeiras regras listadas após o ordenamento por confiança. Você considera que, porque estas regras têm confiança máxima, elas são mais relevantes para sua questão de negócio? Justifique sua resposta.
# lhs                                     rhs           support   confidence coverage  lift      count
# [1]  {Class=Crew}                         => {Age=Adult}   0.4020900 1.0000000  0.4020900 1.0521033  885 
# [2]  {Class=Crew, Survived=No}            => {Age=Adult}   0.3057701 1.0000000  0.3057701 1.0521033  673 
# [3]  {Class=Crew, Sex=Male}               => {Age=Adult}   0.3916402 1.0000000  0.3916402 1.0521033  862 
# [4]  {Class=Crew, Sex=Male, Survived=No}  => {Age=Adult}   0.3044071 1.0000000  0.3044071 1.0521033  670
# Não necessariamente apresentaam dados importantes de negócio, mas sem dúvida apresentam características interessantes sobre a
# base de dados que devem ser levadas em consideração.

# Executando o apriori com suporte e confianca de 90% e inspecinando as regras de associacao ordenadas
rules.all <- apriori(titanic.raw, parameter = list(supp=0.9, conf=0.9))
rules.sorted <- sort(rules.all, by="support")
inspect(rules.sorted)

#Q8: Para suporte e confiança iguais a 90%, alguma regra relevante foi retornada? 
# Não. A ínica regra retornada foi a regra que indica a quantidades de adultos (Age=Adult) na base.

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
# Sim. Foram retornadas 7 regras.

#Q10: As regras [1] a [5] são relevantes para sua questão de negócio? Explique sua resposta.
# [1] {Age=Adult}                       => {Survived=No} 0.65    0.69       0.95     1.02 1438 
# [2] {Sex=Male}                        => {Survived=No} 0.62    0.79       0.79     1.16 1364 
# [3] {Sex=Male, Age=Adult}             => {Survived=No} 0.60    0.80       0.76     1.18 1329 
# [4] {Class=Crew}                      => {Survived=No} 0.31    0.76       0.40     1.12  673 
# [5] {Class=Crew, Age=Adult}           => {Survived=No} 0.31    0.76       0.40     1.12  673 

# Sim. São muito relevantes. Indicam características acerca dos casos de não sobreviventes (Survived=No)

#
# Re-executando com suporte mais baixo, e Removendo regras redundantes
#

rules.all <- apriori(titanic.raw, control = list(verbose=F), # não exibe detalhes do progresso do algoritmo
                     parameter = list(minlen=2, supp=0.05, conf=0.6), # mínimo de itesn, para ignorar regras com lado esquerdo vazio
                     appearance = list(rhs=c("Survived=No", "Survived=Yes"), default="lhs")) # só interessa regras indicando "survived" no lado direito
quality(rules.all) <- round(quality(rules.all), digits=3) #3 casas após a vírgula
rules.sorted <- sort(rules.all, by="support") # ordenação
inspect(rules.sorted)

#Q11: As regras resultantes parecem ser orientadas ao problema de negócio? Explique sua resposta.
# Sim. As regras são relevantes pois indicam características que podem ter indicar as chances de sobrevivência ou não dos tripulantes.

#Q12: Por que não temos regras que explicam a sobrevivência? Que parâmetro você poderia alterar para conseguir regras que expliquem a sobrevivência?
# Existem sim, Visto que foi incluído "Survived=Yes" nos valores possíveis para rhs.
# Por exemplo a regra {Class=1st, Sex=Female} => {Survived=Yes}. 

#Q13: Existem regras que lhe parecem redundantes entre si? Quais?
# Sim. As regras 4 e 5 são redundantes, visto que, como vimos anteriormente para toda ocorrência de Class=Crew, sabemos que ele é sempre Age=Adult

# [4]  {Class=Crew}                       => {Survived=No}  0.306   0.760      0.402    1.123  673 
# [5]  {Class=Crew, Age=Adult}            => {Survived=No}  0.306   0.760      0.402    1.123  673 


# Encontrando e removendo as regras duplicadas (mesmo procedimento acima)
subset.matrix <- is.subset(rules.sorted, rules.sorted, sparse = FALSE)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

#Q14: Comparadas com as regras da execução anterior (Q11), cite dois exemplos distintos de redundâncias que foram eliminadas? Explique sua resposta em termo dos itens que aparecem nas regras,  e suas métricas
# [4]  {Class=Crew}                       => {Survived=No}  0.306   0.760      0.402    1.123  673 
# [5]  {Class=Crew, Age=Adult}            => {Survived=No}  0.306   0.760      0.402    1.123  673 
# A regra 5 foi eliminada e a regra 4 mantida, apresentava métricas exatamente iguais à regra 4.

# [8]  {Class=3rd}                        => {Survived=No}  0.240   0.748      0.321    1.105  528 
# [9]  {Class=3rd, Age=Adult}             => {Survived=No}  0.216   0.759      0.285    1.121  476 
# [10] {Class=3rd, Sex=Male}              => {Survived=No}  0.192   0.827      0.232    1.222  422 
# [11] {Class=3rd, Sex=Male, Age=Adult}   => {Survived=No}  0.176   0.838      0.210    1.237  387 
# As regras 9, 10 e 11 foram eliminadas e a regra 8 foi mantida. A característica "Class" é a principal para avaliar relação com a sobrevivência.
# Não houve muita alteração nos valores das métricas ao adicionarmos outras características ao lado direito da relação.



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
# lhs                                 rhs            support confidence coverage lift  count
# [4]  {Sex=Female, Age=Adult}          => {Survived=Yes} 0.144   0.744      0.193    2.302  316 
# [17] {Age=Adult}                      => {Survived=No}  0.653   0.687      0.950    1.015 1438 
# A regra 4, com lift 2.302, indica que a chance de sobrevivência aumenta quando se é mulher. 
# Já a regra 17 indica que as características adulto (Age=Adult) e não sobrevivente (Survived=No) são independentes.


#Q16: Compare as regras [3] e [18]. Embora elas tenham confiança um pouco distintas, o que os respectivos lifts nos dizem sobre a não sobrevivência?
# lhs                                 rhs            support confidence coverage lift  count
# [3]  {Class=2nd, Sex=Female}          => {Survived=Yes} 0.042   0.877      0.048    2.716   93 
# [18] {Class=2nd}                      => {Survived=No}  0.076   0.586      0.129    0.866  167 
# Para a regra 18, sendo o lift 0.866 menor que 1, indica que não sobrevivência diminui quando a segunda classe aparece.
# E a regra 3 indica que a chance de sobrevivência aumenta quando se é da segunda classe e mulher.

#Q17: Com base nestas regras, liste 5 coisas que podem ser inferidas dos dados sobre sobreviventes e não sobreviventes do Titanic com o uso de regras de associação
# Ser criança tem relação direta com aumento da chance de sobrevivência 
# Ser mulher tem relação direta com aumento da chance de sobrevivência
# Ser classes mais altas tem relação direta com aumento da chance de sobrevivência
# Ser homem adulto tem relação inversa com a chance de sobrevivência
# Ser de classe mais baixa tem relação inversa com a chance de sobrevivência


#Q18: crianças da segunda classe tiveram mais chances de sobrevivência do que as da primeira classe?
# Não é possível chegar a essa conclusão analisando as regras filtradas no momento.


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
# [1] {Class=2nd, Age=Child} => {Survived=Yes} 0.010904134 1.0000000  0.010904134 3.0956399  24  
# [2] {Class=1st, Age=Child} => {Survived=Yes} 0.002726034 1.0000000  0.002726034 3.0956399   6  


#Q20: E as crianças da terceira classe? Como sua situação se compara (a) à das demais crianças? (b) dos adultos da primeira classe? 
# [3] {Class=3rd, Age=Adult} => {Survived=No}  0.216265334 0.7591707  0.284870513 1.1214326 476  
# [4] {Class=3rd, Age=Child} => {Survived=No}  0.023625625 0.6582278  0.035892776 0.9723218  52  

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


