# LABORAT�RIO 07

# O filme O Homem que Mudou o Jogo (Moneyball)
# Neste laborat�rio exploraremos os dados de todos os 30 times da Liga Principal 
# de Beisebol dos EUA e examinaremos a rela��o entre pontos marcados numa temporada
# e v�rias outras estat�sticas dos jogadores.

# Nosso objetivo ser� resumir essas rela��es de maneira visual e num�rica para
# identificar qual vari�vel, se houver alguma, melhor nos ajuda a predizer os 
# pontos marcados por um time numa temporada.

# Vamos carregar os dados da temporada de 2011.
download.file("http://www.openintro.org/stat/data/mlb11.RData", destfile = "mlb11.RData")
load("mlb11.RData")


# Exerc�cio 1 Que tipo de gr�fico voc� utilizaria para mostrar a rela��o entre
# runs (pontos) e alguma outra vari�vel num�rica? Crie um gr�fico dessa rela��o
# utilizando a vari�vel at_bats como preditora. A rela��o parece ser linear?
# Se voc� soubesse o valor de at_bats (vez ao taco) de um time, voc� se sentiria
# confiante para utilizar um modelo linear para predizer o n�mero de pontos (runs)?
plot(mlb11$at_bats, mlb11$runs)

# quantificar a for�a da rela��o utilizando o coeficiente de correla��o
cor(mlb11$runs, mlb11$at_bats)


# Exerc�cio 2 Examinando o gr�fico do exerc�cio anterior, descreva a rela��o
# entre essas duas vari�veis. Certifique-se de discutir a forma, a dire��o e
# a for�a da rela��o, bem como quaisquer caracter�sticas incomuns.
# (A) A rela��o � negativa, linear e moderadamente forte. Um dos potenciais outliers � um time com aproximadamente 5520 tacos.
# (B) A rela��o � positiva, linear e moderadamente forte. Um dos potenciais outliers � um time com aproximadamente 5520 tacos.
# (C) A rela��o � positiva, linear e muito fraca. N�o h� outliers.
# (D) A rela��o � positiva, linear e muito fraca. Um dos potenciais outliers � um time com aproximadamente 5520 tacos.


# podemos resumir a rela��o entre essas duas vari�veis por meio de uma linha
# que melhor descreve sua associa��o

# Utilize a seguinte fun��o interativa para selecionar a linha que voc� acha 
# que cruza a nuvem de pontos da melhor maneira.
plot_ss(x = mlb11$at_bats, y = mlb11$runs)

# Para visualizar o quadrado dos res�duos
plot_ss(x = mlb11$at_bats, y = mlb11$runs, showSquares = TRUE)

# Perceba que o resultado da fun��o plot_ss fornece a inclina��o (coef. angular)
# e o intercepto da sua linha, bem como a soma dos quadrados.


# Exerc�cio 3 Utilizando a fun��o plot_ss, escolha uma linha que consiga
# minimizar a soma dos quadrados. Rode a fun��o v�rias vezes. 
# Qual foi a menor soma dos quadrados que voc� obteve? 
# Compare-a com os resultados obtidos por outros alunos.


# O Modelo Linear (tamb�m conhecido como reta de regress�o)
# linha que minimiza a soma dos quadrados dos res�duos
m1 <- lm(runs ~ at_bats, data = mlb11)
summary(m1)
# runs = -2789.2429 + 0.6305 * at_bats
# 0.63 -> a cada aumento de uma unidade na vari�vel "at_bats", espera-se
# um aumento de 0.63 na vari�vel "runs"
# Sum of Squares: 66.47^2 * 28 = 123711.31
# R2 = 37,29%
# propor��o de variabilidade de Y-"runs" que � explicada por X-"at_bats"


# Exerc�cio 4 Ajuste um novo modelo que utilize a vari�vel homeruns para
# predizer runs (pontos). Utilizando as estimativas dos resultados do R, 
# escreva a equa��o da linha de regress�o. O que a inclina��o (coef. angular) 
# nos diz sobre a rela��o entre o sucesso de um time e seus home runs?
plot(mlb11$homeruns, mlb11$runs)
cor(mlb11$runs, mlb11$homeruns)
m2 <- lm(runs ~ homeruns, data = mlb11)
summary(m2)
# runs = 415.2389 + 1.8345 * homeruns
# R2 = 62.7% # Multiple R-squared:  0.6266

# (A) Para cada homerun adicional, o modelo prev� 1,83 pontos a mais, em m�dia.
# (B) Cada homerun adicional aumenta os pontos em 1,83.
# (C) Para cada homerun adicional, o modelo prev� 1,83 pontos a menos, em m�dia.
# (D) Para cada homerun adicional, o modelo prev� 415,24 pontos a mais, em m�dia.
# (E) Para cada homerun adicional, o modelo prev� 415,24 pontos a menos, em m�dia.

# 1.8345 -> a cada aumento de uma unidade em "homeruns", espera-se um aumento
# de 1.8345 em "runs".


# Predi��o e Erro de Predi��o
# gr�fico de dispers�o dos pontos com a reta ajustada
plot(mlb11$runs ~ mlb11$at_bats)
abline(m1)


# Exerc�cio 5 Se o gerente de um time visse a linha de regress�o dos m�nimos
# quadrados e n�o os dados reais, quantos ponto (runs) ele prediria para 
# um time com 5579 vezes ao taco (at-bats)? 
# Esse valor superestima ou subestima o valor real, e por quanto? 
# Em outras palavras, qual � o res�duo para essa predi��o?
# (A) -15.32
# (B) 15.32
# (C) 713
# (D) 5579

est  <- -2789.2429 + 0.6305 * 5579
est # estimado = 728.3166
library(dplyr)
obs<-mlb11 %>%  filter(at_bats == 5579) %>%  select(runs)
obs # observado = 713
obs-est
# Observado at_bats=5579 e runs=713
#logo: e=713-728.3=-15.3 (superestima)



# Diagn�sticos do Modelo

# (1) a linearidade
plot(m1$residuals ~ mlb11$at_bats)
abline(h = 0, lty = 3) # adiciona uma linha pontilhada horizontal em y = 0

# Exerc�cio 6 H� algum padr�o aparente do gr�fico de res�duos? O que isso indica
# sobre a linearidade da rela��o entre pontos (runs) e vezes ao taco (at-bats)?

# Qual e falsa?
# (A) Os res�duos parecem ser distribu�dos aleatoriamente em torno de 0.
# (B) Os res�duos mostram um padr�o curvo.
# (C) O gr�fico � indicativo de uma rela��o linear entre corridas e rebatidas.
# (D) A equipe com um res�duo muito alto em rela��o aos demais parece ser um outlier.

# R. (B)


# (2) res�duos normalmente distribu�dos
hist(m1$residuals)
qqnorm(m1$residuals)
qqline(m1$residuals) # adiciona uma linha diagonal ao gr�fico de probabilidade normal

# Exerc�cio 7 Com base no histograma e no gr�fico de probabilidade normal, 
# a condi��o de distribui��o normal dos res�duos parece ser atendida?
shapiro.test(m1$residuals)
# (A) Os res�duos s�o extremamente assim�tricos � direita, portanto, a condi��o de distribui��o normal dos res�duos n�o � atendida.
# (B) Os res�duos s�o extremamente assim�tricos � esquerda, portanto, a condi��o de distribui��o normal dos res�duos n�o � atendida.
# (C) Os res�duos s�o perfeitamente sim�tricos, portanto, a condi��o de distribui��o normal dos res�duos � atendida.
# (D) Os res�duos s�o bastante sim�tricos, com apenas uma cauda ligeiramente mais longa � direita, portanto, seria apropriado considerar a condi��o de distribui��o normal dos res�duos atendida.

# R. (D)

# (3) vari�ncia constante.

# Exerc�cio 8 Com base no gr�fico criado em (1), a condi��o de vari�ncia constante parece ser
# atendida?



# Sua Vez - Laborat�rio 07

# 1. Escolha outra vari�vel tradicional contida no banco de dados mlb11 que voc� acha que poderia ser
# um bom preditor da vari�vel runs (pontos). Crie um gr�fico de dispers�o das duas vari�veis e ajuste
# um modelo linear. Visualmente, parece haver uma rela��o linear?

## vari�vel escolhida: bat_avg
plot(mlb11$runs ~ mlb11$bat_avg)
cor(mlb11$runs, mlb11$bat_avg) # correla��o existe e � positiva
m_runs_batavg <- lm(runs ~ bat_avg, data = mlb11)
summary(m_runs_batavg)
abline(m_runs_batavg)

## R. Sim. Parece haver uma rela��o quanto maior runs, maior bat_avg

  
# 2. Compare essa rela��o com a rela��o entre runs (pontos) e at_bats (vez ao taco). Utilize os valores
# R2 do sum�rio dos dois modelos para compar�-los. A vari�vel que voc�s escolheu parece predizer
# runs (pontos) melhor do que at_bats (vez ao taco)? Como voc� justificaria sua resposta?
summary(m1) # Multiple R-squared:  0.3729 = 37%
summary(m_runs_batavg) # Multiple R-squared:  0.6561 = 66%

## R. R2 indica a propor��o da variabilidade de Y [runs]  que � explicada por X [at_bats, bat_avg]
## Sendo R2 de m1 menor que R2 de m_runs_batavg, a rela��o entre runs&bat_avg � mais forte que runs&at_bats


# 3. Agora que voc� pode resumir a rela��o linear entre duas vari�veis, investigue a rela��o entre runs
# (pontos) e cada uma das outras cinco vari�veis tradicionalmente utilizadas no beisebol. Qual vari�vel
# prediz melhor o valor de runs? Justifique sua conclus�o utilizando m�todos gr�ficos e num�ricos
# que j� discutimos (para ser conciso, inclua apenas os resultados da melhor vari�vel, n�o de todas as
# cinco).

## 7 vari�veis tradicionais at_bats, hits, homeruns, bat_avg, strikeouts, stolen_bases, wins

m_runs_at_bats <- lm(runs ~ at_bats, data = mlb11)
summary(m_runs_at_bats) # Multiple R-squared:  0.3729

m_runs_hits <- lm(runs ~ hits, data = mlb11)
summary(m_runs_hits) # Multiple R-squared:  0.6419

m_runs_homeruns <- lm(runs ~ homeruns, data = mlb11)
summary(m_runs_homeruns) #Multiple R-squared:  0.6266

m_runs_bat_avg <- lm(runs ~ bat_avg, data = mlb11)
summary(m_runs_bat_avg) # Multiple R-squared:  0.6561

m_runs_strikeouts <- lm(runs ~ strikeouts, data = mlb11)
summary(m_runs_strikeouts) # Multiple R-squared:  0.1694

m_runs_stolen_bases <- lm(runs ~ stolen_bases, data = mlb11)
summary(m_runs_stolen_bases) # Multiple R-squared:  0.002914

m_runs_wins <- lm(runs ~ wins, data = mlb11)
summary(m_runs_wins) # Multiple R-squared:  0.361

## R. A vari�vel que melhor melhor se relaciona com runs � bat_avg.


# 4. Agora examine as tr�s vari�veis mais recentes. Essas s�o as estat�sticas utilizadas pelo autor do
# filme O Homem que Mudou o Jogo para predizer o sucesso de um time. De modo geral, elas s�o
# mais ou menos eficazes para predizer os pontos do que as vari�veis mais tradicionais? Explique
# utilizando evid�ncias gr�ficas e num�ricas. De todas as dez vari�veis que n�s analisamos, qual
# parece ser o melhor preditor da vari�vel runs (pontos)? Utilizando as informa��es limitadas (ou n�o
# t�o limitadas) que voc� conhece sobre estas estat�sticas do beisebol, seu resultado faz sentido?

## novas vari�veis: new_onbase, new_slug, new_obs
m_runs_new_onbase <- lm(runs ~ new_onbase, data = mlb11)
summary(m_runs_new_onbase) # Multiple R-squared:  0.8491

m_runs_new_slug <- lm(runs ~ new_slug, data = mlb11)
summary(m_runs_new_slug) # Multiple R-squared:  0.8969


m_runs_new_obs <- lm(runs ~ new_obs, data = mlb11)
summary(m_runs_new_obs) # Multiple R-squared:  0.9349

## R. As novas vari�vesi s�o muito mais relacionadas com a vari�vel runs que as anteriores.
## Meu conhecimento sobre � beisebol � muito limitado para entender como essas vari�veis se relacionam.

  
# 5. Verifique os diagn�sticos do modelo para o modelo de regress�o com a vari�vel que voc� escolheu
# como o melhor preditor dos pontos (runs).

plot(mlb11$runs ~ mlb11$new_obs)
abline(m_runs_new_obs)

# (1) a linearidade
plot(m_runs_new_obs$residuals ~ mlb11$new_obs)
abline(h = 0, lty = 3) # adiciona uma linha pontilhada horizontal em y = 0

# (2) res�duos normalmente distribu�dos
hist(m_runs_new_obs$residuals)
qqnorm(m_runs_new_obs$residuals)
qqline(m_runs_new_obs$residuals) # adiciona uma linha diagonal ao gr�fico de probabilidade normal

# FINALIZANDO...
# Encontrar um modelo de Regress�o Linear M�ltipla para os dados do LAB07.


library(openintro)
library(ggplot2)
library(broom)
library(dplyr)
library(GGally) # devtools::install_github("ggobi/ggally")
library(car)

# plot das combina��es
mlb11 %>% 
  select(runs, at_bats, hits, homeruns, bat_avg, strikeouts, stolen_bases, wins, new_onbase, new_slug, new_obs) %>% 
  ggpairs()

# modelo de regress�o m�ltipla
mm_basebol<-lm(runs~at_bats+hits+homeruns+bat_avg+strikeouts+stolen_bases+wins+new_onbase+new_slug+new_obs
       , data=mlb11)
summary(mm_basebol)

# stepwise
?step
stepwise_basebol <-step(mm_basebol,direction="both")
summary(stepwise_basebol)

## vari�veis finais: hits, bat_avg, stolen_bases, new_slug, new_obs

mlb11 %>% 
  select(hits, bat_avg, stolen_bases, new_slug, new_obs) %>% 
  ggpairs()

# FIV (VIF) deve ser menor que 10
vif(stepwise_basebol)
## VIF < 10 somente para stolen_bases

# diagnóstico do modelo
plot(stepwise_basebol,2)
plot(stepwise_basebol,3)

