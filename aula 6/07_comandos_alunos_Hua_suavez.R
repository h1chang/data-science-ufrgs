# LABORATÓRIO 07

# O filme O Homem que Mudou o Jogo (Moneyball)
# Neste laboratório exploraremos os dados de todos os 30 times da Liga Principal 
# de Beisebol dos EUA e examinaremos a relação entre pontos marcados numa temporada
# e várias outras estatísticas dos jogadores.

# Nosso objetivo será resumir essas relações de maneira visual e numérica para
# identificar qual variável, se houver alguma, melhor nos ajuda a predizer os 
# pontos marcados por um time numa temporada.

# Vamos carregar os dados da temporada de 2011.
download.file("http://www.openintro.org/stat/data/mlb11.RData", destfile = "mlb11.RData")
load("mlb11.RData")


# Exercício 1 Que tipo de gráfico você utilizaria para mostrar a relação entre
# runs (pontos) e alguma outra variável numérica? Crie um gráfico dessa relação
# utilizando a variável at_bats como preditora. A relação parece ser linear?
# Se você soubesse o valor de at_bats (vez ao taco) de um time, você se sentiria
# confiante para utilizar um modelo linear para predizer o número de pontos (runs)?
plot(mlb11$at_bats, mlb11$runs)

# quantificar a força da relação utilizando o coeficiente de correlação
cor(mlb11$runs, mlb11$at_bats)


# Exercício 2 Examinando o gráfico do exercício anterior, descreva a relação
# entre essas duas variáveis. Certifique-se de discutir a forma, a direção e
# a força da relação, bem como quaisquer características incomuns.
# (A) A relação é negativa, linear e moderadamente forte. Um dos potenciais outliers é um time com aproximadamente 5520 tacos.
# (B) A relação é positiva, linear e moderadamente forte. Um dos potenciais outliers é um time com aproximadamente 5520 tacos.
# (C) A relação é positiva, linear e muito fraca. Não há outliers.
# (D) A relação é positiva, linear e muito fraca. Um dos potenciais outliers é um time com aproximadamente 5520 tacos.


# podemos resumir a relação entre essas duas variáveis por meio de uma linha
# que melhor descreve sua associação

# Utilize a seguinte função interativa para selecionar a linha que você acha 
# que cruza a nuvem de pontos da melhor maneira.
plot_ss(x = mlb11$at_bats, y = mlb11$runs)

# Para visualizar o quadrado dos resíduos
plot_ss(x = mlb11$at_bats, y = mlb11$runs, showSquares = TRUE)

# Perceba que o resultado da função plot_ss fornece a inclinação (coef. angular)
# e o intercepto da sua linha, bem como a soma dos quadrados.


# Exercício 3 Utilizando a função plot_ss, escolha uma linha que consiga
# minimizar a soma dos quadrados. Rode a função várias vezes. 
# Qual foi a menor soma dos quadrados que você obteve? 
# Compare-a com os resultados obtidos por outros alunos.


# O Modelo Linear (também conhecido como reta de regressão)
# linha que minimiza a soma dos quadrados dos resíduos
m1 <- lm(runs ~ at_bats, data = mlb11)
summary(m1)
# runs = -2789.2429 + 0.6305 * at_bats
# 0.63 -> a cada aumento de uma unidade na variável "at_bats", espera-se
# um aumento de 0.63 na variável "runs"
# Sum of Squares: 66.47^2 * 28 = 123711.31
# R2 = 37,29%
# proporção de variabilidade de Y-"runs" que é explicada por X-"at_bats"


# Exercício 4 Ajuste um novo modelo que utilize a variável homeruns para
# predizer runs (pontos). Utilizando as estimativas dos resultados do R, 
# escreva a equação da linha de regressão. O que a inclinação (coef. angular) 
# nos diz sobre a relação entre o sucesso de um time e seus home runs?
plot(mlb11$homeruns, mlb11$runs)
cor(mlb11$runs, mlb11$homeruns)
m2 <- lm(runs ~ homeruns, data = mlb11)
summary(m2)
# runs = 415.2389 + 1.8345 * homeruns
# R2 = 62.7% # Multiple R-squared:  0.6266

# (A) Para cada homerun adicional, o modelo prevê 1,83 pontos a mais, em média.
# (B) Cada homerun adicional aumenta os pontos em 1,83.
# (C) Para cada homerun adicional, o modelo prevê 1,83 pontos a menos, em média.
# (D) Para cada homerun adicional, o modelo prevê 415,24 pontos a mais, em média.
# (E) Para cada homerun adicional, o modelo prevê 415,24 pontos a menos, em média.

# 1.8345 -> a cada aumento de uma unidade em "homeruns", espera-se um aumento
# de 1.8345 em "runs".


# Predição e Erro de Predição
# gráfico de dispersão dos pontos com a reta ajustada
plot(mlb11$runs ~ mlb11$at_bats)
abline(m1)


# Exercício 5 Se o gerente de um time visse a linha de regressão dos mínimos
# quadrados e não os dados reais, quantos ponto (runs) ele prediria para 
# um time com 5579 vezes ao taco (at-bats)? 
# Esse valor superestima ou subestima o valor real, e por quanto? 
# Em outras palavras, qual é o resíduo para essa predição?
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



# Diagnósticos do Modelo

# (1) a linearidade
plot(m1$residuals ~ mlb11$at_bats)
abline(h = 0, lty = 3) # adiciona uma linha pontilhada horizontal em y = 0

# Exercício 6 Há algum padrão aparente do gráfico de resíduos? O que isso indica
# sobre a linearidade da relação entre pontos (runs) e vezes ao taco (at-bats)?

# Qual e falsa?
# (A) Os resíduos parecem ser distribuídos aleatoriamente em torno de 0.
# (B) Os resíduos mostram um padrão curvo.
# (C) O gráfico é indicativo de uma relação linear entre corridas e rebatidas.
# (D) A equipe com um resíduo muito alto em relação aos demais parece ser um outlier.

# R. (B)


# (2) resíduos normalmente distribuídos
hist(m1$residuals)
qqnorm(m1$residuals)
qqline(m1$residuals) # adiciona uma linha diagonal ao gráfico de probabilidade normal

# Exercício 7 Com base no histograma e no gráfico de probabilidade normal, 
# a condição de distribuição normal dos resíduos parece ser atendida?
shapiro.test(m1$residuals)
# (A) Os resíduos são extremamente assimétricos à direita, portanto, a condição de distribuição normal dos resíduos não é atendida.
# (B) Os resíduos são extremamente assimétricos à esquerda, portanto, a condição de distribuição normal dos resíduos não é atendida.
# (C) Os resíduos são perfeitamente simétricos, portanto, a condição de distribuição normal dos resíduos é atendida.
# (D) Os resíduos são bastante simétricos, com apenas uma cauda ligeiramente mais longa à direita, portanto, seria apropriado considerar a condição de distribuição normal dos resíduos atendida.

# R. (D)

# (3) variância constante.

# Exercício 8 Com base no gráfico criado em (1), a condição de variância constante parece ser
# atendida?



# Sua Vez - Laboratório 07

# 1. Escolha outra variável tradicional contida no banco de dados mlb11 que você acha que poderia ser
# um bom preditor da variável runs (pontos). Crie um gráfico de dispersão das duas variáveis e ajuste
# um modelo linear. Visualmente, parece haver uma relação linear?

## variável escolhida: bat_avg
plot(mlb11$runs ~ mlb11$bat_avg)
cor(mlb11$runs, mlb11$bat_avg) # correlação existe e é positiva
m_runs_batavg <- lm(runs ~ bat_avg, data = mlb11)
summary(m_runs_batavg)
abline(m_runs_batavg)

## R. Sim. Parece haver uma relação quanto maior runs, maior bat_avg

  
# 2. Compare essa relação com a relação entre runs (pontos) e at_bats (vez ao taco). Utilize os valores
# R2 do sumário dos dois modelos para compará-los. A variável que vocês escolheu parece predizer
# runs (pontos) melhor do que at_bats (vez ao taco)? Como você justificaria sua resposta?
summary(m1) # Multiple R-squared:  0.3729 = 37%
summary(m_runs_batavg) # Multiple R-squared:  0.6561 = 66%

## R. R2 indica a proporção da variabilidade de Y [runs]  que é explicada por X [at_bats, bat_avg]
## Sendo R2 de m1 menor que R2 de m_runs_batavg, a relação entre runs&bat_avg é mais forte que runs&at_bats


# 3. Agora que você pode resumir a relação linear entre duas variáveis, investigue a relação entre runs
# (pontos) e cada uma das outras cinco variáveis tradicionalmente utilizadas no beisebol. Qual variável
# prediz melhor o valor de runs? Justifique sua conclusão utilizando métodos gráficos e numéricos
# que já discutimos (para ser conciso, inclua apenas os resultados da melhor variável, não de todas as
# cinco).

## 7 variáveis tradicionais at_bats, hits, homeruns, bat_avg, strikeouts, stolen_bases, wins

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

## R. A variável que melhor melhor se relaciona com runs é bat_avg.


# 4. Agora examine as três variáveis mais recentes. Essas são as estatísticas utilizadas pelo autor do
# filme O Homem que Mudou o Jogo para predizer o sucesso de um time. De modo geral, elas são
# mais ou menos eficazes para predizer os pontos do que as variáveis mais tradicionais? Explique
# utilizando evidências gráficas e numéricas. De todas as dez variáveis que nós analisamos, qual
# parece ser o melhor preditor da variável runs (pontos)? Utilizando as informações limitadas (ou não
# tão limitadas) que você conhece sobre estas estatísticas do beisebol, seu resultado faz sentido?

## novas variáveis: new_onbase, new_slug, new_obs
m_runs_new_onbase <- lm(runs ~ new_onbase, data = mlb11)
summary(m_runs_new_onbase) # Multiple R-squared:  0.8491

m_runs_new_slug <- lm(runs ~ new_slug, data = mlb11)
summary(m_runs_new_slug) # Multiple R-squared:  0.8969


m_runs_new_obs <- lm(runs ~ new_obs, data = mlb11)
summary(m_runs_new_obs) # Multiple R-squared:  0.9349

## R. As novas variávesi são muito mais relacionadas com a variável runs que as anteriores.
## Meu conhecimento sobre é beisebol é muito limitado para entender como essas variáveis se relacionam.

  
# 5. Verifique os diagnósticos do modelo para o modelo de regressão com a variável que você escolheu
# como o melhor preditor dos pontos (runs).

plot(mlb11$runs ~ mlb11$new_obs)
abline(m_runs_new_obs)

# (1) a linearidade
plot(m_runs_new_obs$residuals ~ mlb11$new_obs)
abline(h = 0, lty = 3) # adiciona uma linha pontilhada horizontal em y = 0

# (2) resíduos normalmente distribuídos
hist(m_runs_new_obs$residuals)
qqnorm(m_runs_new_obs$residuals)
qqline(m_runs_new_obs$residuals) # adiciona uma linha diagonal ao gráfico de probabilidade normal

# FINALIZANDO...
# Encontrar um modelo de Regressão Linear Múltipla para os dados do LAB07.


library(openintro)
library(ggplot2)
library(broom)
library(dplyr)
library(GGally) # devtools::install_github("ggobi/ggally")
library(car)

# plot das combinações
mlb11 %>% 
  select(runs, at_bats, hits, homeruns, bat_avg, strikeouts, stolen_bases, wins, new_onbase, new_slug, new_obs) %>% 
  ggpairs()

# modelo de regressão múltipla
mm_basebol<-lm(runs~at_bats+hits+homeruns+bat_avg+strikeouts+stolen_bases+wins+new_onbase+new_slug+new_obs
       , data=mlb11)
summary(mm_basebol)

# stepwise
?step
stepwise_basebol <-step(mm_basebol,direction="both")
summary(stepwise_basebol)

## variáveis finais: hits, bat_avg, stolen_bases, new_slug, new_obs

mlb11 %>% 
  select(hits, bat_avg, stolen_bases, new_slug, new_obs) %>% 
  ggpairs()

# FIV (VIF) deve ser menor que 10
vif(stepwise_basebol)
## VIF < 10 somente para stolen_bases

# diagnÃ³stico do modelo
plot(stepwise_basebol,2)
plot(stepwise_basebol,3)

