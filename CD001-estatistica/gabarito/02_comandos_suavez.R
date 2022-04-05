# LABORATÓRIO 02 - SUA VEZ

#1. Descreva a distribuição das sequências de arremessos. Qual é o comprimento de sequência típico
#para o arremessador independente simulado com um percentual de arremesso de 45%? Quão longa
#é a sequência mais longa de cestas em 133 arremessos?
sim_streak <- calc_streak(sim_basket_kobe)
barplot(table(sim_streak))
# 5 arremessos com cesta em sequência


#2. Se você rodasse a simulação do arremessador independente uma segunda vez, como você acha que
#seria a distribuição de sequências em relação à distribuição da questão acima? Exatamente a mesma?
#Mais ou menos parecida? Completamente diferente? Explique seu raciocínio.
# R. Serão bastante parecidas


#3. Como a distribuição dos comprimentos de sequência de Kobe Bryant, analisada na página 2, se
#comparam à distribuição de comprimentos de sequência do arremessador simulado? Utilizando
#essa comparação, você tem evidência de que o modelo das mãos quentes se ajusta aos padrões de
#arremessos de Kobe? Explique.
# R. O modelo das mãos quentes não se ajusta, pois os arremessos de Kobe se assemelham muito
# aos arremessos independentes que foram simulados
