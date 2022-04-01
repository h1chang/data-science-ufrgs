# LABORATÓRIO 04A - SUA VEZ


#1. Retire uma amostra aleatória de 50 elementos da variável price (preço). Com essa amostra, qual é
#sua melhor estimativa pontual para a média populacional?
samp_price_50 <- sample(ames$SalePrice, 50)
summary(samp_price_50)
hist(samp_price_50)
# R. X_barra = 161373

#2. Já que você tem acesso à população, simule a distribuição amostral de ¯ xprice retirando 5000 amostras
#de 50 elementos da população e calculando 5000 médias amostrais. Armazene essas médias em um
#vetor com o nome sample_means50. Crie um gráfico com os resultados, e então descreva a forma
#dessa distribuição amostral. Baseado nessa distribuição amostral, qual seria seu palpite para a média
#dos preços das casas na população? Por fim, calcule e informe a média populacional.
sample_price_means50 <- rep(0, 5000)
for(i in 1:5000){
  samp <- sample(ames$SalePrice, 50)
  sample_price_means50[i] <- mean(samp)
}
hist(sample_price_means50)
mean(sample_price_means50)
mean(ames$SalePrice)
# R. X_barra.barra = 180854 e S_média = 11442
# R. Mi = 180796,1


#3. Mude o tamanho da sua amostra de 50 para 150, e então calcule a distribuição amostral utilizando o
#mesmo método descrito acima, e guarde as médias em um novo vetor com o nome sample_means150.
#Descreva a forma dessa distribuição amostral e compare-a com a distribuição amostral para a amostra
#de 50 elementos. Com base nessa distribuição amostral, qual seria seu palpite sobre a média dos
#preços de vendas de casas no município de Ames?
sample_price_means150 <- rep(0, 5000)
for(i in 1:5000){
  samp <- sample(ames$SalePrice, 150)
  sample_price_means150[i] <- mean(samp)
}
hist(sample_price_means150)
mean(sample_price_means150)
# R. X_barra.barra = 180734 e S_média = 6354


#4. Das distribuições amostrais calculadas nos exercícios 2 e 3, qual tem menor dispersão? Se estamos
#interessados em estimativas que estão mais próximas do valor verdadeiro, preferiríamos uma
#distribuição com uma dispersão pequena ou grande?
sd(sample_price_means50)
sd(sample_price_means150)
# R. A ditribuição das médias de tamanho 150 tem menor dispersão, dando estimativas mais próximas ao valor verdadeiro.

