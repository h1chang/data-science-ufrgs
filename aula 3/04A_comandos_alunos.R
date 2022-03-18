#dados do setor imobiliário da cidade de Ames, no estado de Iowa, Estados Unidos
#vendas de casas em Ames entre 2006 e 2010 => representa nossa população de interesse.

download.file("http://www.openintro.org/stat/data/ames.RData", destfile = "ames.RData")
load("ames.RData")

#restringiremos nossa atenção para somente duas variáveis: a área habitável
#da casa acima do nível do solo em pés quadrados (Gr.Liv.Area) e o preço da venda (SalePrice).
area <- ames$Gr.Liv.Area
price <- ames$SalePrice

#distribuição da área populacional
summary(area)
hist(area)

# Exercício 1 Descreva a distribuição da população.
# R. Assimétrica positiva


# Neste laboratório nós temos acesso à população inteira, 
#mas isso raramente acontece na vida real.

#retirando uma amostra n=50 de area
samp1 <- sample(area, 50)

# Exercício 2 Descreva a distribuição desta amostra. Como ela se compara à distribuição da população?
summary(samp1)
hist(samp1)
# R. O objetivo é que ela seja representativa da população

#estimar a área habitável média
mean(samp1)

#Exercício 3 Retire uma segunda amostra, também de 50 casos, e a atribua a uma variável
#de nome samp2. Como a média de samp2 se compara à média de samp1? Vamos supor que
#retiremos mais duas amostras, uma de de 100 casos e outra de 1000 casos. Qual você acha que
#daria uma estimativa mais precisa da média populacional?
samp2 <- sample(area, 50)
summary(samp2)
hist(samp2)

samp100 <- sample(area, 100)
summary(samp100)
hist(samp100)

samp1000 <- sample(area, 1000)
summary(samp1000)
hist(samp1000)

# R. Quanto maior a amostra, mais precisa será a estimativa da média, 
# pois mais próximo se estará da população.


#A cada vez que retiramos uma nova amostra, obtemos uma média amostral diferente. 
#É útil ter uma ideia de quanta variabilidade podemos esperar quando estimamos
#a média populacional desta maneira. A distribuição das médias amostrais, 
#denominada de distribuição amostral, pode nos ajudar a compreeder essa variabilidade.

#geraremos 5000 amostras de n=50 e calcularemos a média amostra de cada uma
sample_means50_5000 <- rep(0, 5000)
for(i in 1:5000){
  samp <- sample(area, 50)
  sample_means50_5000[i] <- mean(samp)
}
hist(sample_means50_5000)
hist(sample_means50_5000, breaks = 25)


#Exercício 4: A variável sample_means50 contém quantos elementos? Descreva a distribuição
#amostral, e certifique-se de prestar atenção especificamente em seu centro. Você acha que a
#distribuilção mudaria se coletássemos 50.000 médias amostrais?
sample_means50_50000 <- rep(0, 50000)
for(i in 1:50000){
  samp <- sample(area, 50)
  sample_means50_50000[i] <- mean(samp)
}
hist(sample_means50_50000, breaks = 25)

# R. Sample_means50 tem 5000 médias das 5000 amostras
# R. A distribuição das médias se assemelha a uma distribuição normal - TCL
# R. Se mais amostras fossem coletadas, o gráfico seria semelhante, 
# mas se o tamanho das amostras fosse maior, o gráfico teria menor variabilidade.

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

#Exercício 5 Para certificar que você compreendeu o que você fez neste loop, experimente rodar
#uma versão menor. Inicialize um vetor com 100 zeros com o nome sample_means_small.
#Execute um loop que retira uma amostra de 50 elementos da variável area e armazena a média
#amostral no vetor sample_means_small, mas que repete a iteração de 1 a 100. Imprima o
#resultado em sua tela (basta digitar sample_means_small no console e pressionar enter). Há
#quantos elementos no objeto sample_means_small? O que cada elemento representa?
sample_means_small <- rep(0, 100)
for(i in 1:100){
  samp <- sample(area, 50)
  sample_means_small[i] <- mean(samp)
}
sample_means_small
# R. São as médias das 100 amostras geradas.


#Tamanho da Amostra e Distribuição Amostral
hist(sample_means50)

#a distribuição amostral está centrada na verdadeira média da área habitável da população
#para verificar o efeito do tamanho da amostra na variabilidade da distribuição amostral, 
#vamos construir mais duas distribuições amostrais: 
#uma baseada numa amostra de 10 elementos e outra baseada numa amostra de 100
sample_means10 <- rep(0, 5000)
sample_means100 <- rep(0, 5000)
for(i in 1:5000){
  samp <- sample(area, 10)
  sample_means10[i] <- mean(samp)
  samp <- sample(area, 100)
  sample_means100[i] <- mean(samp)
}

#gráficos da população e das três distribuições com tamanhos 10, 50 e 100
par(mfrow = c(2, 2))
xlimits = range(area)
hist(area, breaks = 20, xlim = xlimits)
hist(sample_means10, breaks = 20, xlim = xlimits)
hist(sample_means50, breaks = 20, xlim = xlimits)
hist(sample_means100, breaks = 20, xlim = xlimits)

#Para retornar para a configuração padrão de criar um gráfico por vez
par(mfrow = c(1, 1))



# Exercício 6 Quando o tamanho da amostra é maior, o que acontece com o 
#centro da distribuição? E com a dispersão?
# R. O centro está sempre em torno de 1500, e a dispersão vai diminuindo 
#com o aumento do tamanho da amostra.







#SUA VEZ

