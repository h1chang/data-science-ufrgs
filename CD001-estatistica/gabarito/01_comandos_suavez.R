# LABORATÓRIO 01 - SUA VEZ

#1. Crie um gráfico de dispersão da variável peso em relação ao peso desejado. Defina a relação entre essas duas variáveis.
plot (cdc$weight, cdc$wtdesire)
# R. Há uma relação positiva entre o peso atual da pessoa e o quanto ela deseja ter.

#2. Vamos considerar uma nova variável: a diferença entre o peso desejado (wtdesire) e o peso atual (weight). 
# Crie esta nova variável subtraindo as duas colunas na base de dados e atribuindo-as a um novo objeto chamado wdiff.
wdiff <- cdc$weight-cdc$wtdesire

#3. Que tipo de dado está contido na variável wdiff? 
# R. Uma variável numerica com o desejo de perda de peso.
# Se uma observação de wdiff é 0, o que isso implica com relação ao peso atual e desejado de uma pessoas?
# R. Significa que a pessoa está satisfeita com seu peso.
# E se o valor de wdiff for positivo ou negativo?
# R. Se for positivo, quer dizer que a pessoa quer perder peso.
mean(wdiff)

#4. Descreva a distribuição de wdiff em termos de seu centro, forma e variação, incluindo qualquer
#gráfico que você usar. O que isso nos diz sobre como as pessoas se sentem a respeito do seu peso atual?
summary(wdiff)
sd(wdiff)
hist(wdiff)
boxplot(wdiff)
# R. As pessoas estão insatisfeitas, pois querem perder algum peso.


#5. Utilizando sumários numéricos e um gráfico de caixas lado-a-lado, determine se homens tendem a ver seu peso diferentemente das mulheres.
summary(wdiff[cdc$gender=="m"])
summary(wdiff[cdc$gender=="f"])
boxplot(wdiff~cdc$gender)
# R. As mulheres estão mais insatisfeitas com seu peso.

#6. Agora chegou a hora de usar a criatividade. Encontre a média e o desvio padrão de weight e
#determine qual a proporção de pesos que estão a um desvio padrão da média.
meanweight <- mean(cdc$weight)
sdweight <- sd(cdc$weight)
sum((cdc$weight > meanweight-sdweight) & (cdc$weight < meanweight+sdweight))/20000
# R. 70,76%
sum((cdc$weight > meanweight-2*sdweight) & (cdc$weight < meanweight+2*sdweight))/20000
sum((cdc$weight > meanweight-3*sdweight) & (cdc$weight < meanweight+3*sdweight))/20000

