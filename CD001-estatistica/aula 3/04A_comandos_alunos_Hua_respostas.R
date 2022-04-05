#dados do setor imobili?rio da cidade de Ames, no estado de Iowa, Estados Unidos
#vendas de casas em Ames entre 2006 e 2010 => representa nossa popula??o de interesse.

download.file("http://www.openintro.org/stat/data/ames.RData", destfile = "ames.RData")
load("ames.RData")

#restringiremos nossa aten??o para somente duas vari?veis: a ?rea habit?vel
#da casa acima do n?vel do solo em p?s quadrados (Gr.Liv.Area) e o pre?o da venda (SalePrice).
area <- ames$Gr.Liv.Area
price <- ames$SalePrice

#distribui??o da ?rea populacional
summary(area)
hist(area)

# Exerc?cio 1 Descreva a distribui??o da popula??o.
# R. Assim?trica positiva


# Neste laborat?rio n?s temos acesso ? popula??o inteira, 
#mas isso raramente acontece na vida real.

#retirando uma amostra n=50 de area
samp1 <- sample(area, 50)

# Exerc?cio 2 Descreva a distribui??o desta amostra. Como ela se compara ? distribui??o da popula??o?
summary(samp1)
hist(samp1)
# R. O objetivo ? que ela seja representativa da popula??o

#estimar a ?rea habit?vel m?dia
mean(samp1)

#Exerc?cio 3 Retire uma segunda amostra, tamb?m de 50 casos, e a atribua a uma vari?vel
#de nome samp2. Como a m?dia de samp2 se compara ? m?dia de samp1? Vamos supor que
#retiremos mais duas amostras, uma de de 100 casos e outra de 1000 casos. Qual voc? acha que
#daria uma estimativa mais precisa da m?dia populacional?
samp2 <- sample(area, 50)
summary(samp2)
hist(samp2)

samp100 <- sample(area, 100)
summary(samp100)
hist(samp100)

samp1000 <- sample(area, 1000)
summary(samp1000)
hist(samp1000)

# R. Quanto maior a amostra, mais precisa ser? a estimativa da m?dia, 
# pois mais pr?ximo se estar? da popula??o.


#A cada vez que retiramos uma nova amostra, obtemos uma m?dia amostral diferente. 
#? ?til ter uma ideia de quanta variabilidade podemos esperar quando estimamos
#a m?dia populacional desta maneira. A distribui??o das m?dias amostrais, 
#denominada de distribui??o amostral, pode nos ajudar a compreeder essa variabilidade.

#geraremos 5000 amostras de n=50 e calcularemos a m?dia amostra de cada uma
sample_means50_5000 <- rep(0, 5000)
for(i in 1:5000){
  samp <- sample(area, 50)
  sample_means50_5000[i] <- mean(samp)
}
hist(sample_means50_5000)
hist(sample_means50_5000, breaks = 25)


#Exerc?cio 4: A vari?vel sample_means50 cont?m quantos elementos? Descreva a distribui??o
#amostral, e certifique-se de prestar aten??o especificamente em seu centro. Voc? acha que a
#distribuil??o mudaria se colet?ssemos 50.000 m?dias amostrais?
sample_means50_50000 <- rep(0, 50000)
for(i in 1:50000){
  samp <- sample(area, 50)
  sample_means50_50000[i] <- mean(samp)
}
hist(sample_means50_50000, breaks = 25)

# R. Sample_means50 tem 5000 m?dias das 5000 amostras
# R. A distribui??o das m?dias se assemelha a uma distribui??o normal - TCL
# R. Se mais amostras fossem coletadas, o gr?fico seria semelhante, 
# mas se o tamanho das amostras fosse maior, o gr?fico teria menor variabilidade.

sample_means500_50000 <- rep(0, 50000)
for(i in 1:50000){
  samp <- sample(area, 500)
  sample_means500_50000[i] <- mean(samp)
}
hist(sample_means500_50000, breaks = 25)



#entendendo o for - pedimos para imprimir o valor de i
sample_means50 <- rep(0, 5000)
for(i in 1:5000){
  samp <- sample(area, 50)
  sample_means50[i] <- mean(samp)
  print(i)
}

#Exerc?cio 5 Para certificar que voc? compreendeu o que voc? fez neste loop, experimente rodar
#uma vers?o menor. Inicialize um vetor com 100 zeros com o nome sample_means_small.
#Execute um loop que retira uma amostra de 50 elementos da vari?vel area e armazena a m?dia
#amostral no vetor sample_means_small, mas que repete a itera??o de 1 a 100. Imprima o
#resultado em sua tela (basta digitar sample_means_small no console e pressionar enter). H?
#quantos elementos no objeto sample_means_small? O que cada elemento representa?
sample_means_small <- rep(0, 100)
for(i in 1:100){
  samp <- sample(area, 50)
  sample_means_small[i] <- mean(samp)
}
sample_means_small
# R. S?o as m?dias das 100 amostras geradas.


#Tamanho da Amostra e Distribui??o Amostral
hist(sample_means50)

#a distribui??o amostral est? centrada na verdadeira m?dia da ?rea habit?vel da popula??o
#para verificar o efeito do tamanho da amostra na variabilidade da distribui??o amostral, 
#vamos construir mais duas distribui??es amostrais: 
#uma baseada numa amostra de 10 elementos e outra baseada numa amostra de 100
sample_means10 <- rep(0, 5000)
sample_means100 <- rep(0, 5000)
for(i in 1:5000){
  samp <- sample(area, 10)
  sample_means10[i] <- mean(samp)
  samp <- sample(area, 100)
  sample_means100[i] <- mean(samp)
}

#gr?ficos da popula??o e das tr?s distribui??es com tamanhos 10, 50 e 100
par(mfrow = c(2, 2))
xlimits = range(area)
hist(area, breaks = 20, xlim = xlimits)
hist(sample_means10, breaks = 20, xlim = xlimits)
hist(sample_means50, breaks = 20, xlim = xlimits)
hist(sample_means100, breaks = 20, xlim = xlimits)

#Para retornar para a configura??o padr?o de criar um gr?fico por vez
par(mfrow = c(1, 1))

# Exerc?cio 6 Quando o tamanho da amostra ? maior, o que acontece com o 
#centro da distribui??o? E com a dispers?o?
# R. O centro est? sempre em torno de 1500, e a dispers?o vai diminuindo 
#com o aumento do tamanho da amostra.


# Sua Vez - Laboratório 04A
# Até agora, nós nos ocupamos em estimar a média da área habitável nas casas do município de Ames.

# Agora você tentará estimar a média dos preços das casas.

# 1. Retire uma amostra aleatória de 50 elementos da variável price (preço). Com essa amostra, qual é
# sua melhor estimativa pontual para a média populacional?
sampPrice <- sample(price, 50)
sampPrice
hist(sampPrice, breaks = 25)
summary(sampPrice)
mean(price) # mÃ©dia real
mean(sampPrice) # mÃ©dia da amostra  
## R. A média real é 180796, enquanto a média da amostra de tamanho 50 é 178573.
  
# 2. Já que você tem acesso à população, simule a distribuição amostral de ¯ xprice retirando 5000 amostras
# de 50 elementos da população e calculando 5000 médias amostrais. Armazene essas médias em um
# vetor com o nome sample_means50. Crie um gráfico com os resultados, e então descreva a forma
# dessa distribuição amostral. Baseado nessa distribuição amostral, qual seria seu palpite para a média
# dos preços das casas na população? Por fim, calcule e informe a média populacional.
sample_means50 <- rep(0, 5000)
for(i in 1:5000){
  samp <- sample(price, 50)
  sample_means50[i] <- mean(samp)
}

hist(sample_means50)
mean(sample_means50)
mean(price)
## R. A distribuição amostral é muito parecida com uma distribuição normal.
## A média da amostra é 180687, muito próxima da média real que é 180796.

# 3. Mude o tamanho da sua amostra de 50 para 150, e então calcule a distribuição amostral utilizando o
# mesmo método descrito acima, e guarde as médias em um novo vetor com o nome sample_means150.
# Descreva a forma dessa distribuição amostral e compare-a com a distribuição amostral para a amostra
# de 50 elementos. Com base nessa distribuição amostral, qual seria seu palpite sobre a média dos
# preços de vendas de casas no município de Ames?

sample_means150 <- rep(0, 5000)
for(i in 1:5000){
  samp <- sample(price, 150)
  sample_means150[i] <- mean(samp)
}

par(mfrow=c(2,1)) # exibir grÃ¡ficos lado a lado
hist(sample_means50, breaks = 50, xlim=c(150000, 220000)) # limites eixo x para facilitar comparação visual
hist(sample_means150, breaks = 50, xlim=c(150000, 220000))
par(mfrow = c(1, 1)) # reset
mean(sample_means50)
mean(sample_means150)
mean(price)
## R. O ponto médio da distribuiçãode deslocou para direita, e a distribuição parece ser menos dispersa.
## A média indicada pela sample_means150 é 180777.


  
# 4. Das distribuições amostrais calculadas nos exercícios 2 e 3, qual tem menor dispersão? Se estamos
# interessados em estimativas que estão mais próximas do valor verdadeiro, preferiríamos uma
# distribuição com uma dispersão pequena ou grande?

par(mfrow=c(2,1))
hist(sample_means50, breaks = 50, xlim=c(150000, 220000))
hist(sample_means150, breaks = 50, xlim=c(150000, 220000))
par(mfrow = c(1, 1)) # reset
summary(sample_means50)
summary(sample_means150)
summary(price)

## R. A amostra sample_means150, parece ser menos dispersa e com a média mais próxima da média da distribuição real.

