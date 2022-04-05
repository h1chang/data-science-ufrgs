#M?os Quentes
#artigo de 1985 escrito por Gilovich, Vallone e Tversky coletou evid?ncia que contradiz 
#essa cren?a e mostrou que lances sucessivos s?o eventos independentes.

#Os objetivos deste laborat?rio s?o (1) refletir sobre o efeito de eventos independentes e dependentes,
#(2) aprender como simular sequ?ncias de lances no R, e
#(3) comparar a simula??o com os dados efetivos para determinar se o fen?meno das m?os quentes parece ser real.

#performance de um jogador: Kobe Bryant do Los Angeles Lakers. Sua
#performance contra o Orlando Magic nas finais de 2009 da NBA lhe deram o t?tulo de "Jogador Mais
#Valioso" e v?rios espectadores comentaram como ele parecia demonstrar uma m?o quente.

download.file("http://www.openintro.org/stat/data/kobe.RData", destfile = "kobe.RData")
load("kobe.RData")

#analisar as primeiras linhas
head(kobe)

#Neste banco de dados, cada linha registra um lance feito por Kobe Bryant. Se ele acertou o lance 
#(fez uma cesta), um acerto, H (de Hit), ? registrado na coluna denominada basket (cesta); 
#caso contr?rio um erro, M (de Miss), ? registrado.

#sequ?ncia de acertos e erros de suas nove tentativas no primeiro quarto do primeiro jogo
kobe$basket[1:9]

# H M | M | H H M | M | M | M
# comprimentos s?o um, zero, dois, zero, zero, zero

# Exerc?cio 1 O que uma sequ?ncia de comprimento 1 significa, ou seja, quantos acertos e erros
# existem dentro de um sequ?ncia de 1? E de uma sequ?ncia de comprimento 0?
# R. Comprimeiro 1 significa que teve um acerto em sequencia. No comprimento zero n?o houve acerto.


#A fun??o personalizada calc_streak, que foi carregada com os dados, pode ser utilizada para calcular os
#comprimentos de todas as sequ?ncias de acertos e ent?o conferir sua distribui??o.
kobe_streak <- calc_streak(kobe$basket)
barplot(table(kobe_streak))

# Exerc?cio 2 Descreva a distribui??o do comprimento das sequ?ncias de Kobe nas finais de 2009 da NBA. 
# R. Distribui??o assim?trica positiva, de 0 a 4.
# Qual foi seu tamanho de sequ?ncia t?pico? 
# R. zero
# Qu?o longa foi sua maior sequ?ncia de cestas?
# R. quatro

# Mostramos que Kobe teve algumas sequ?ncias de arremesso longas, 
# mas elas s?o longas o suficiente para apoiar a cren?a de que ele 
# tinha m?os quentes? Com o que podemos compar?-las?
# ...

#simular um lance de uma moeda honesta
outcomes <- c("heads", "tails")
sample(outcomes, size = 1, replace = TRUE)

#simular o lan?amento de uma moeda honesta 100 vezes
sim_fair_coin <- sample(outcomes, size = 100, replace = TRUE)
sim_fair_coin
table(sim_fair_coin)

#simular uma moeda viciada que sabemos que d? cara somente 20% das vezes
sim_unfair_coin <- sample(outcomes, size = 100, replace = TRUE, prob = c(0.2, 0.8))
sim_unfair_coin
table(sim_unfair_coin)

# Exerc?cio 3 Em sua simula??o de lan?ar uma moeda viciada 100 vezes, quantos lances deram cara?
# R. 49 lances na moeda honesta e 24 lances na moeda viciada


#Se voc? quiser saber mais sobre a fun??o sample
?sample


#simular um jogador de basquete que arremessa de forma independente
outcomes <- c("H", "M")
sim_basket <- sample(outcomes, size = 1, replace = TRUE)
sim_basket #um ?nico arremesso

#Exerc?cio 4 
#Qual mudan?a precisa ser feita para que a fun??o sample reflita o percentual
#de arremessos de 45%? Fa?a esse ajuste, e ent?o rode a simula??o para uma amostra de 133
sim_basket_kobe <- sample(outcomes, size = 133, replace = TRUE, prob = c(0.45, 0.55))
sim_basket_kobe
table(sim_basket_kobe)/133 # percentual de acertos simulado
table(kobe$basket)/133 # percentual de acertos observado no dataset kobe




#SUA VEZ

# Utilizando a função calc_streak, calcule o comprimento das sequências do vetor sim_basket.
# 1. Descreva a distribuição das sequências de arremessos. Qual é o comprimento de sequência típico 
# para o arremessador independente simulado com um percentual de arremesso de 45%? Quão longa
# é a sequência mais longa de cestas em 133 arremessos?
sim_basket_kobe <- sample(outcomes, size = 133, replace = TRUE, prob = c(0.45, 0.55)) # rodando simulação
sim_basket_kobe
barplot(table(sim_basket_kobe)) # quantidade de H e M
sim_basket_kobe_streak <- calc_streak(sim_basket_kobe)
sim_basket_kobe_streak
plot(sim_basket_kobe_streak) # seq mais comum é "quebra de sequencia" = 0
tabela_sim_basket_kobe_streak <- table(sim_basket_kobe_streak)
barplot(tabela_sim_basket_kobe_streak) # agrupando os streaks, pelas quantidades de seq

## R. A sequência mais comum é a quebra de sequência, representada pelo 0 no vetor sim_basket_kobe_streak.
##  A sequencia mais longa na simulação foi 4 acertos (H) e só aconteceu uma única vez.

# 2. Se você rodasse a simulação do arremessador independente uma segunda vez, como você acha que
# seria a distribuição de sequências em relação à distribuição da questão acima? Exatamente a mesma?
# Mais ou menos parecida? Completamente diferente? Explique seu raciocínio.
sim_basket_kobe_novo <- sample(outcomes, size = 133, replace = TRUE, prob = c(0.45, 0.55)) # rodando simulação novamente
sim_basket_kobe_novo
table(sim_basket_kobe_novo) # quantidade de H e M
calc_streak(sim_basket_kobe_novo)
barplot(table(calc_streak(sim_basket_kobe_novo))) # quantidade de cada sequência

## R. Apesar da quantidade total de H e M serem parecidas, as distribuições das sequências foram diferentes.
## Segundo caso por exemplo teve sequencias de 7 e 8 acertos, enquanto que a primeira simulação a maior sequência foi de 4.

# 3. Como a distribuição dos comprimentos de sequência de Kobe Bryant, analisada na página 2, se
# comparam à distribuição de comprimentos de sequência do arremessador simulado? Utilizando
# essa comparação, você tem evidência de que o modelo das mãos quentes se ajusta aos padrões de
# arremessos de Kobe? Explique.
par(mfrow = c(1, 2)) # para exibir lado a lado
barplot(table(kobe_streak))
barplot(table(calc_streak(sim_basket_kobe_novo)))

## R. A distribuição das sequências de Kobe bryant e do jogador simulado são muito parecidas. 
## A distribuição simulada tem sequências maiores, possivelmente pela maior amostra.
## Não há evidência que o modelo das mãos quentes se aplica ao jogador, 
## a distribuição de sequências de acertos do jogador é aderente ao jogador simulado.

