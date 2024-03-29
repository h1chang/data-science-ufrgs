#dados do setor imobili?rio da cidade de Ames, no estado de Iowa, Estados Unidos
#vendas de casa em Ames entre 2006 e 2010

download.file("http://www.openintro.org/stat/data/ames.RData", destfile = "ames.RData")
load("ames.RData")

#focaremos no tamanho da casa, representada pela vari?vel Gr.Liv.Area
#amostra aleat?ria simples de 60 elementos da popula??o
population <- ames$Gr.Liv.Area
samp <- sample(population, 60)

#Exerc?cio 1 Descreva a distribui??o da sua amostra. 
#Qual ? o tamanho "t?pico" dentro da sua amostra? 
#Procure esclarecer tamb?m como voc? interpretou o significado de "t?pico".
summary(samp)
# R. O tamanho t?pico pode ser descrito pela m?dia = 1526

#Exerc?cio 2 Voc? acha que a distribui??o de outro aluno seria id?ntica a sua? 
#Voc? acha que ela seria similar? Por qu?, ou por qu? n?o?
# R. Esperaria-se valor pr?ximo, pela quest?o da amostra ser representativa 
# e pelo que vimos na distribui??o da m?dias amostrais tendendo a distribui??o normal.

#valor t?pico - m?dia
sample_mean <- mean(samp)
sample_mean

#Baseado nesta amostra, o que podemos inferir sobre a popula??o?
#A m?dia amostral serve como uma boa estimativa pontual, mas seria
#interessante tamb?m deixar claro quanta incerteza temos desta estimativa. 
#Isso pode ser feito pelo uso de um intervalo de confian?a.

#intervalo de confian?a de 95% para a m?dia amostral
se <- sd(samp)/sqrt(60)
lower <- sample_mean - 1.96 * se
upper <- sample_mean + 1.96 * se
c(lower, upper)

#temos 95% de confian?a de que a m?dia verdadeira do tamanho das casas em Ames 
#se encontra entre os valores lower (limite inferior do intervalo de confian?a) 
#e upper (limite superior do intervalo de confian?a).

#de onde vem o 1,96?
qnorm(0.975, mean = 0, sd = 1, lower.tail = T)
qnorm(0.975)


#Exerc?cio 3 
#Para o intervalos de confian?a ser v?lido, a m?dia amostral precisa ter distribui??o normal e ter um erro padr?o igual a s/raiz(n). 
#Quais condi??es precisam ser atendidas para isso ser verdadeiro?
# R. dados originais com distribui??o normal ou
# tamanho de amostra grande para uso do TCL (n>30)
hist(population)
qqnorm(population)
qqline(population)
#mas n=60... ufa!!!


# Exerc?cio 4 O que significa "95% de confian?a"?
# R. 95% de chance de o intervalo conter a verdadeira m?dia populacional

#m?dia da popula??o
mean(population)


# Exerc?cio 5 O seu intervalo de confian?a cont?m a verdadeira m?dia do tamanho 
#das casas em Ames? Se voc? est? trabalhando neste laborat?rio em uma sala de aula, 
#o intervalo de seus colegas tamb?m cont?m esse valor?
# R. Sim, e espera-se que 95% dos intervalor constru?dos contenham a verdadeira m?dia.


# Exerc?cio 6 Cada aluno de sua turma deve ter obtido um intervalo de confian?a um pouco
# diferente. Que propor??o desses intervalos voc? espera que contenha a verdadeira m?dia
# populacional? Por qu?? Se voc? est? trabalhando neste laborat?rio em um sala de aula, re?na
# informa??es sobre os intervalos criados pelos outros alunos da turma e calcule a propor??o de
# intervalos que cont?m a verdadeira m?dia populacional.
# R. Enquete no Mentimeter


#criar v?rias amostras para aprender um pouco mais a respeito de como as m?dias
#amostrais e os intervalos de confian?a variam de uma amostra para outra.

#primeiro criar os vetores vazios de m?dias e desvios e depois fazemos o loop
samp_mean <- rep(NA, 50)
samp_sd <- rep(NA, 50)
n <- 60
for(i in 1:50){
  samp <- sample(population, n) # obt?m uma amostra de n = 60 elementos da popula??o
  samp_mean[i] <- mean(samp) # salva a m?dia amostral no i-?simo elemento de samp_mean
  samp_sd[i] <- sd(samp) # salva o dp da amostra como o i-?simo elemento de samp_sd
}

#constru?mos os intervalos de confian?a.
lower_vector <- samp_mean - 1.96 * samp_sd / sqrt(n)
upper_vector <- samp_mean + 1.96 * samp_sd / sqrt(n)

#Vamos visualizar o primeiro intervalo.
c(lower_vector[1],upper_vector[1])



#Sua Vez

# Sua Vez - Laborat�rio 04B

# 1. Utilizando a seguinte fun��o (que foi carregada junto com o conjunto de dados), crie gr�ficos de todos
# os intervalos. Que propor��o dos intervalos de confian�a cont�m a verdadeira m�dia populacional?
# Essa propor��o � exatamente igual ao n�vel de confian�a? Se n�o, explique por qu�.
# plot_ci(lower_vector, upper_vector, mean(population))

plot_ci(lower_vector, upper_vector, mean(population)) # tr�s linhas vermelhas no gr�fico gerado
3/50 # 94% < 95%

## R. Das 50 amostras geradas 3 delas, representadas por linhas vermelhas no gr�fico, n�o cont�m
## a m�dia real da popula��o. A propor��o encontrada, 94% � inferior ao n�vel de confian�a de 95%.
## Provavelmente � pelo fato de serem somente 50 amostras de tamnho pequeno.

# 2. Escolha um intervalo de confian�a de sua prefer�ncia, desde que n�o seja de 95%. Qual � o valor
# cr�tico apropriado?

x <- (99/100) # confian�a desejada 95%
y <- (1-x)/2 # area desejada em cada ponta da cauda
1-y # par�metro para a fun��o qnorm
qnorm(1-y, mean = 0, sd = 1, lower.tail = T)
qnorm(1-y)

# transformando em fun��o
valorCritico <- function (confianca)  {
  x <- (confianca/100) # confian�a desejada 95%
  y <- (1-x)/2 # area desejada em cada ponta da cauda
  1-y # par�metro para a fun��o qnorm
  qnorm(1-y, mean = 0, sd = 1, lower.tail = T)
  qnorm(1-y) # valor cr�tico
}
valorCritico(95)

# aplicando a forma para 97% de confian�a
valorCritico(97) # 2.17009

## R. Para 97% de confian�a desejada o valor cr�tico � 2.17009
  
# 3. Calcule 50 intervalos de confian�a utilizando o n�vel de confian�a que voc� escolheu na quest�o anterior.
# Voc� n�o precisa obter novas amostras: simplesmente calcule os novos intervalos baseado nas
# m�dias amostrais e desvios padr�o que voc� j� coletou. Utilizando a fun��o plot_ci, crie gr�ficos de
# todos os intervalos e calcule a propor��o de intervalos que cont�m a verdadeira m�dia populacional.
# Compare essa propor��o com o n�vel de confian�a escolhido para os intervalos.

lower_vector <- samp_mean - valorCritico(97) * samp_sd / sqrt(n)
upper_vector <- samp_mean + valorCritico(97) * samp_sd / sqrt(n)
c(lower_vector[1],upper_vector[1])

plot_ci(lower_vector, upper_vector, mean(population)) # gr�fico apresentou 1 linha vermelha
1 - 1/50 # 98% > 97% (confian�a desejada)

## R. A propor��o encontrada foi 98%, bem pr�xima a confian�a escolhida de 97%.
