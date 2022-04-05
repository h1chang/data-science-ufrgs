# LABORATÓRIO 04B - Sua Vez


#1. Utilizando a seguinte função (que foi carregada junto com o conjunto de dados), crie gráficos de todos
#os intervalos. Que proporção dos intervalos de confiança contém a verdadeira média populacional?
#Essa proporção é exatamente igual ao nível de confiança? Se não, explique por quê.???
plot_ci(lower_vector, upper_vector, mean(population))

#2. Escolha um intervalo de confiança de sua preferência, desde que não seja de 95%. Qual é o valor
#crítico apropriado?
qnorm(0.975) #dá o valor crítico para 95% de confiança
qnorm(0.95) #dá o valor crítico para 90% de confiança
qnorm(0.995) #dá o valor crítico para 99% de confiança

#3. Calcule 50 intervalos de confiança utilizando o nível de confiança que você escolheu na questão anterior.
#Você não precisa obter novas amostras: simplesmente calcule os novos intervalos baseado nas
#médias amostrais e desvios padrão que você já coletou. Utilizando a função plot_ci, crie gráficos de
#todos os intervalos e calcule a proporção de intervalos que contém a verdadeira média populacional.
#Compare essa proporção com o nível de confiança escolhido para os intervalos.

#com 90%
lower_vector <- samp_mean - 1.645 * samp_sd / sqrt(n)
upper_vector <- samp_mean + 1.645 * samp_sd / sqrt(n)
plot_ci(lower_vector, upper_vector, mean(population))

#com 99%
lower_vector <- samp_mean - 2.576 * samp_sd / sqrt(n)
upper_vector <- samp_mean + 2.576 * samp_sd / sqrt(n)
plot_ci(lower_vector, upper_vector, mean(population))
