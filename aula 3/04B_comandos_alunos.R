#dados do setor imobiliário da cidade de Ames, no estado de Iowa, Estados Unidos
#vendas de casa em Ames entre 2006 e 2010

download.file("http://www.openintro.org/stat/data/ames.RData", destfile = "ames.RData")
load("ames.RData")

#focaremos no tamanho da casa, representada pela variável Gr.Liv.Area
#amostra aleatória simples de 60 elementos da população
population <- ames$Gr.Liv.Area
samp <- sample(population, 60)

#Exercício 1 Descreva a distribuição da sua amostra. 
#Qual é o tamanho "típico" dentro da sua amostra? 
#Procure esclarecer também como você interpretou o significado de "típico".
summary(samp)
# R. O tamanho típico pode ser descrito pela média = 1526

#Exercício 2 Você acha que a distribuição de outro aluno seria idêntica a sua? 
#Você acha que ela seria similar? Por quê, ou por quê não?
# R. Esperaria-se valor próximo, pela questão da amostra ser representativa 
# e pelo que vimos na distribuição da médias amostrais tendendo a distribuição normal.

#valor típico - média
sample_mean <- mean(samp)
sample_mean

#Baseado nesta amostra, o que podemos inferir sobre a população?
#A média amostral serve como uma boa estimativa pontual, mas seria
#interessante também deixar claro quanta incerteza temos desta estimativa. 
#Isso pode ser feito pelo uso de um intervalo de confiança.

#intervalo de confiança de 95% para a média amostral
se <- sd(samp)/sqrt(60)
lower <- sample_mean - 1.96 * se
upper <- sample_mean + 1.96 * se
c(lower, upper)

#temos 95% de confiança de que a média verdadeira do tamanho das casas em Ames 
#se encontra entre os valores lower (limite inferior do intervalo de confiança) 
#e upper (limite superior do intervalo de confiança).

#de onde vem o 1,96?
qnorm(0.975, mean = 0, sd = 1, lower.tail = T)
qnorm(0.975)


#Exercício 3 
#Para o intervalos de confiança ser válido, a média amostral precisa ter distribuição normal e ter um erro padrão igual a s/raiz(n). 
#Quais condições precisam ser atendidas para isso ser verdadeiro?
# R. dados originais com distribuição normal ou
# tamanho de amostra grande para uso do TCL (n>30)
hist(population)
qqnorm(population)
qqline(population)
#mas n=60... ufa!!!


# Exercício 4 O que significa "95% de confiança"?
# R. 95% de chance de o intervalo conter a verdadeira média populacional

#média da população
mean(population)


# Exercício 5 O seu intervalo de confiança contém a verdadeira média do tamanho 
#das casas em Ames? Se você está trabalhando neste laboratório em uma sala de aula, 
#o intervalo de seus colegas também contém esse valor?
# R. Sim, e espera-se que 95% dos intervalor construídos contenham a verdadeira média.


# Exercício 6 Cada aluno de sua turma deve ter obtido um intervalo de confiança um pouco
# diferente. Que proporção desses intervalos você espera que contenha a verdadeira média
# populacional? Por quê? Se você está trabalhando neste laboratório em um sala de aula, reúna
# informações sobre os intervalos criados pelos outros alunos da turma e calcule a proporção de
# intervalos que contém a verdadeira média populacional.
# R. Enquete no Mentimeter


#criar várias amostras para aprender um pouco mais a respeito de como as médias
#amostrais e os intervalos de confiança variam de uma amostra para outra.

#primeiro criar os vetores vazios de médias e desvios e depois fazemos o loop
samp_mean <- rep(NA, 50)
samp_sd <- rep(NA, 50)
n <- 60
for(i in 1:50){
  samp <- sample(population, n) # obtém uma amostra de n = 60 elementos da população
  samp_mean[i] <- mean(samp) # salva a média amostral no i-ésimo elemento de samp_mean
  samp_sd[i] <- sd(samp) # salva o dp da amostra como o i-ésimo elemento de samp_sd
}

#construímos os intervalos de confiança.
lower_vector <- samp_mean - 1.96 * samp_sd / sqrt(n)
upper_vector <- samp_mean + 1.96 * samp_sd / sqrt(n)

#Vamos visualizar o primeiro intervalo.
c(lower_vector[1],upper_vector[1])



#Sua Vez

