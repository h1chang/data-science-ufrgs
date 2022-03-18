#trabalharemos com medidas de dimens?es do corpo. Este conjunto de dados cont?m medidas
#de 247 homens e 260 mulheres, a maioria dos quais foram considerados adultos jovens saud?veis.
download.file("http://www.openintro.org/stat/data/bdims.RData", destfile = "bdims.RData")
load("bdims.RData")

#primeiras linhas dos dados
head(bdims)
#temos 25 medidas, muitas das quais s?o di?metros ou circunfer?ncias
#focaremos em: peso em kg (wgt), altura em cm (hgt), e sex (sexo, 1 masculino, 0 feminino)

#ser? ?til criar dois conjuntos de dados adicionais: um com os dados dos homens e outro com os dados das mulheres
mdims <- subset(bdims, bdims$sex == 1)
fdims <- subset(bdims, bdims$sex == 0)

#Exerc?cio 1 Elabore um histograma da altura dos homens e um histograma das alturas das
#mulheres. Como voc? descreveria os diferentes aspectos das duas distribui??es?
hist(mdims$hgt)
hist(fdims$hgt)
#R. Homens com distribui??o de altura centrada em 175-185 e mulheres entre 160-170
# Distribui??o aproximadamente sim?trica, em forma de sino (com uma moda central)

#m?dia e desvio padr?o das alturas das mulheres
fhgtmean <- mean(fdims$hgt) # média
fhgtsd <- sd(fdims$hgt) # desvio
summary(fdims$hgt)

#histograma de densidade (?reas das barras somadas resultam em 1) - s? muda o eixo y
hist(fdims$hgt, probability = TRUE)
x <- 140:190
y <- dnorm(x = x, mean = fhgtmean, sd = fhgtsd)
lines(x = x, y = y, col = "blue")
?dnorm

# Exerc?cio 2 Baseado neste gr?fico, parece que os dados seguem aproximadamente uma distribui??o normal?
# R. Sim, bastante parecido.


#gr?fico normal Q-Q, de "quantil-quantil" (quantil amostral-quantil teórico)
qqnorm(fdims$hgt)
qqline(fdims$hgt) #se for normal, espera-se todos os pontos perto da linha


#simulando dados a partir de uma distribui??o normal - para ver como fica o q-q plot
sim_norm <- rnorm(n = length(fdims$hgt), mean = fhgtmean, sd = fhgtsd)

#Exerc?cio 3 
#Fa?a um gr?fico de probabilidade normal do vetor sim_norm. Os pontos caem todos em 
#cima da linha? Como este gr?fico se compara ao gr?fico de probabilidade dos dados reais?
hist(sim_norm, probability = TRUE)
lines(x = x, y = y, col = "blue")
qqnorm(sim_norm)
qqline(sim_norm)
# R. Os gr?ficos se parecem muito, com alguns pontos errantes na dire??o das caudas. 


#comprarar v?rios outros gr?ficos utilizando os dados originais e amostras simuladas
qqnormsim(fdims$hgt)


# Exerc?cio 4 O gr?fico de probabilidade normal para fdims$hgt parece similar aos gr?ficos criados para os dados simulados? 
# R. S?o muito similares
# Quer dizer, os gr?ficos fornecem evid?ncia de que as alturas de mulheres s?o aproximadamente normais?
# R. As evid?ncias s?o de que as alturas das mulheres s?o aproximadamente normais.


#####################################

#Exerc?cio 5 
#Usando a mesma t?cnica, determine se os pesos de mulheres parecem ser provenientes de uma distribui??o normal.
hist(fdims$wgt)

fwgtmean <- mean(fdims$wgt)
fwgtsd <- sd(fdims$wgt)

hist(fdims$wgt, probability = TRUE)
x <- 20:120
y <- dnorm(x = x, mean = fwgtmean, sd = fwgtsd)
lines(x = x, y = y, col = "blue")

qqnorm(fdims$wgt)
qqline(fdims$wgt)

sim_norm <- rnorm(n = length(fdims$wgt), mean = fwgtmean, sd = fwgtsd)
hist(sim_norm, probability = TRUE)
lines(x = x, y = y, col = "blue")
qqnorm(sim_norm)
qqline(sim_norm)

qqnormsim(fdims$wgt)


########################################

#Qual ? a probabilidade de que uma mulher adulta jovem escolhida por acaso 
# ? maior do que 6 p?s (cerca de 182 cm - os dados est?o em cm)?
1 - pnorm(q = 182, mean = fhgtmean, sd = fhgtsd)
# R. 0.004434387

#calcular a mesma probabilidade anterior empiricamente
sum(fdims$hgt > 182) / length(fdims$hgt)
# R. 0.003846154

########################################

#Exerc?cio 6 
#Elabore duas quest?es de probabilidade que voc? gostaria de responder; uma
#com rela??o ? altura de mulheres e outra com rela??o ao peso de mulheres. Calcule essas
#probabilidades usando tanto o m?todo te?rico da distribui??o normal quanto a distribui??o
#emp?rica (quatro probabilidade no total). 


#Qual ? a probabilidade de uma mulher ser menor do que 168 cm?
pnorm(q = 168, mean = fhgtmean, sd = fhgtsd)
# R. 0.6836408
sum(fdims$hgt < 168) / length(fdims$hgt)
# R. 0.6846154

#Qual ? a probabilidade de uma mulher ter peso maior que 60 kg?
1 - pnorm(q = 60, mean = fwgtmean, sd = fwgtsd)
# R. 0.524893
sum(fdims$wgt > 60) / length(fdims$wgt)
# 0.4384615

# Qual vari?vel, altura ou peso, teve uma concord?ncia maior entre os dois m?todos?
# R. altura

########################################

# SUA VEZ
# Sua Vez - Laboratório 03

# 1. Agora vamos analisar outras variáveis no conjunto de dados das dimensões corporais. Utilizando
# as figuras na próxima página, combine os histogramas com seus gráficos de probabilidade normal.
# Todas as variáveis foram estandardizadas (primeiro subtraindo a média, e em seguida dividindo pelo
# desvio padrão), de tal forma que as unidades não serão de qualquer ajuda. Se você estiver incerto
# com base nessas figuras, gere um gráfico no R para verificar.

# (a) O histograma do diâmetro bi-ilíaco (pélvico) feminino (bii.di) pertence ao gráfico de probabilidade normal de letra .
hist(fdims$bia.di)

drawqq <- function(dados) {
  mean <- mean(dados) # média
  sd <- sd(dados) # desvio
  
  hist(dados, probability = TRUE)
  
  x <- min(dados):max(dados)
  y <- dnorm(x = x, mean = mean, sd = sd)
  lines(x = x, y = y, col = "blue")
  qqnorm(dados)
  qqline(dados)
}
drawqq(fdims$bii.di)
## R. gráfico de probabilidade normal é o B

# (b) O histograma do diâmetro do cotovelo feminino (elb.di) pertence ao gráfico de probabilidade normal de letra.
hist(fdims$elb.di)
drawqq(fdims$elb.di)
## R. gráfico de probabilidade normal é o C

# (c) O histograma de idade geral (age) pertence ao gráfico de probabilidade normal de letra .
hist(bdims$age)
drawqq(bdims$age)
## R. gráfico de probabilidade normal é o D

# (d) O histograma de profundidade do peito feminino (che.de) pertence ao gráfico de probabilidade normal de letra .
hist(fdims$che.de)
drawqq(fdims$che.de)
## R. gráfico de probabilidade normal é o A

# 2. Perceba que os gráficos de probabilidade normal C e D tem um pequeno padrão passo a passo. Por
# que você acha que eles são assim?
head(bdims$age)
head(bdims$che.de)

## R. Isso acontece um função da precisão da escala métrica utilizada. 
## Por exemplo a idade (age) é medida em números inteiros.
  
# 3. Como você pode ver, gráficos de probabilidade normal podem ser utilizados tanto para avaliar a
# normalidade quanto visualizar a assimetria. Crie um gráfico de probabilidade normal para o diâmetro
# do joelho feminino (kne.di). Baseado neste gráfico de probabilidade normal, você diria que
# essa variável é simétrica, assimétrica à direita ou assimétrica à esquerda? Utiliza um histograma para
# confirmar seu resultado.
hist(fdims$kne.di)
drawqq(fdims$kne.di)

media <- mean(fdims$kne.di) # média deslocada para a direita
desvio_padrao <- sd(fdims$kne.di) # desvio
summary(fdims$kne.di)

hist(fdims$kne.di, probability = TRUE)
x <- 15:25
y <- dnorm(x = x, mean = media, sd = desvio_padrao)
lines(x = x, y = y, col = "blue")

pnorm(q = media, mean = media, sd = desvio_padrao)
sum(fdims$kne.di < media) / length(fdims$kne.di)
## abaixo da média real = 52%

sum(fdims$kne.di > media) / length(fdims$kne.di)
## acima da média real = 48%

## R. A média está deslocada para a direita (valor 18.1). 
## Em relação a normal, distribuição real é assimétrica a direita.

