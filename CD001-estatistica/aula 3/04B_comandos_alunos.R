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

# Sua Vez - Laboratório 04B

# 1. Utilizando a seguinte função (que foi carregada junto com o conjunto de dados), crie gráficos de todos
# os intervalos. Que proporção dos intervalos de confiança contém a verdadeira média populacional?
# Essa proporção é exatamente igual ao nível de confiança? Se não, explique por quê.
# plot_ci(lower_vector, upper_vector, mean(population))

# 2. Escolha um intervalo de confiança de sua preferência, desde que não seja de 95%. Qual é o valor
# crítico apropriado?
  
# 3. Calcule 50 intervalos de confiança utilizando o nível de confiança que você escolheu na questão anterior.
# Você não precisa obter novas amostras: simplesmente calcule os novos intervalos baseado nas
# médias amostrais e desvios padrão que você já coletou. Utilizando a função plot_ci, crie gráficos de
# todos os intervalos e calcule a proporção de intervalos que contém a verdadeira média populacional.
# Compare essa proporção com o nível de confiança escolhido para os intervalos.

