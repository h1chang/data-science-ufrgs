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
## 30 observa��es - 2 = 28
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
# R2 = 62.7%

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
library(dplyr)
obs<-mlb11 %>%  filter(at_bats == 5579) %>%  select(runs)
obs-est
# Observado at_bats=5579 e runs=713, logo: e=713-728.3=-15.3 (superestima)



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


# (3) variância constante.

# Exercício 8 Com base no gráfico criado em (1), a condição de variância constante parece ser
# atendida?
