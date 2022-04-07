
############################
###### Questão 1 - Definições
############################

# Estatística
# Amostra, amostragem
# População
# Média, desvio padrão
# Parâmetro
# Censo

# Declarações corretas

# Temos o interesse na altura média dos jogadores de futebol profissional no Brasil. As alturas de 23% dos jogadores profissionais dos diversos clubes
# nacionais são medidas. Então, a média e o desvio padrão das alturas são calculadas. Dessa forma, se observou uma amostra dos jogadores profissionais

# Suponha que uma parte os alunos de uma escola foram investigados com o objetivo de saber o seu desempenho (bom, ruim e regular) nas aulas
# assíncronas de matemática. O resultado desta proporção foi de 37% para desempenho bom, o que representa um estatística

# A abordagem estatística envolvida ao generalizarmos resultados de uma amostra para toda uma população é chamada inferência estatística

# Estatística é a medida usada para descrever uma característica numérica da amostra.

# População é o conjunto formado por todos os elementos ou resultados sob investigação.

# Suponha que todos os alunos de uma escola foram investigados com o objetivo de saber o seu desempenho (bom, ruim e regular) nas aulas assíncronas de matemática. O resultado desta
# proporção foi de 40% para desempenho bom, o que representa um parâmetro




############################
###### Questão 2 - Média, Mediana e Desvio Padrão
############################

# criar array 


# descobrir estatísticas
vector1 <- c(21, 20, 18, 16, 28, 22, 16, 21, 20)
summary(vector1)
##  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 

# Desvio padrão de uma coleção 
sd(vector1)


############################
###### Questão 3 - Histograma
############################

vector2 <- c(20, 20.4, 20.4, 20.6, 21.5, 22.1, 22.3, 22.6, 23, 24.1
             , 24.1, 24.4, 25.1, 25.9, 25.9, 26.1, 26.2, 27, 27.3, 27.3
             , 27.5, 27.6, 27.6, 27.8, 28.4, 29.4, 29.8, 29.9, 31.3, 31.4)

x_inicial <- 20 # x inicial
x_final <- 35 # x final
amp_classe <- 1.875 # amplitude de classe

i_break <- x_inicial
custom_breaks <- c()
while (i_break <= x_final) {
  custom_breaks <- append(custom_breaks, i_break)  
  i_break <- i_break + amp_classe
}
custom_breaks

hist(vector2, 
     breaks = custom_breaks, 
     xlim = c(x_inicial, x_final), 
     labels = TRUE)


############################
###### Questão 4 - Interpretação boxplot
############################

boxplot(values)
boxplot(values, plot = F)$stats



############################
###### Questão 5 - Probabilidade
############################

# União = adição
# interseção = produto
# Espaço amostral = todos resultados possíveis
# Elementos disjuntos


############################
###### Questão 6 - Distribuição Normal
############################

# qnorm para aplicar a probabilidade(%) e encontrar valores
# Ex. qnorm(0.05, mean = media, sd = desvio_padrao, lower.tail = T)

# pnorm para aplicar valores e encontrar a probabilidade (%)

media <- 24
desvio_padrao <- 1.5

# 1 F
qnorm(0.05, mean = media, sd = desvio_padrao, lower.tail = T)
##  21.53272

# 2 V
x <- pnorm(22.6, mean = media, sd = desvio_padrao, lower.tail = T)
y <- pnorm(19.3, mean = media, sd = desvio_padrao, lower.tail = T)
x-y 
## [1] 0.1744598 V

# 3 F
2.5 * 1.5
24 + (2.5 * 1.5)
## 27.75

# 4 V
pnorm(23.4, mean = media, sd = desvio_padrao, lower.tail = T)
# 0.3445783

# 5 F
0.9/2
0.5 + 0.45
0.5 - 0.45

qnorm(0.95, mean = media, sd = desvio_padrao, lower.tail = T)
# 26.46728
qnorm(0.05, mean = media, sd = desvio_padrao, lower.tail = T)
# 21.53272

############################
###### Questão 7 - 
############################

# media <- ?
variancia <- 25
tamanho <- 400
z_calculado <- 0.2

desvio_padrao <- sqrt(variancia)
erro_padrao <- desvio_padrao / sqrt(tamanho)
valor_critico <- z_calculado / erro_padrao
prob <- 1 - 2 * pnorm(valor_critico, 0, 1, lower.tail = F)
prob
##  0.5762892


############################
###### Questão 8 - 
############################

tamanho <- 121
media <- 130.1
desvio_padrao <- 9
alpha <- 98/100 # confiança
# intervalo [128.2; 132]

erro_padrao <- desvio_padrao/sqrt(tamanho)
z <- qnorm (1 - (1 - alpha)/2, mean = 0, sd = 1, lower.tail = T)

media + z * erro_padrao
media - z * erro_padrao

# 1) encontrar intervelo
# 2) interpretação do intervalo
# 3) Mantendo-se tudo mais fixo, quando aumentamos (diminuímos) o tamanho da amostra, a amplitude do intervalo é diminuida (aumentada).
# 4) Mantendo-se tudo mais fixo, conforme a confiança do intervalo aumenta (diminui), a sua amplitude tende a aumentar (diminuir).
# 5) Mantendo-se tudo mais fixo, quanto maior (menor) for a variância, maior (menor) será a amplitude do intervalo de confiança para a média populacional.

############################
###### Questão 9 - 
############################

# a) limite superior para intervalo de confiança 99%
tamanho <- 15
media <- 345
variancia <- 100

desvio_padrao <- sqrt(variancia)

# a) qual limite superior?
confianca <- 99/100

x <- 1 - ((1-confianca)/2) # 0.995
t <- qt(x, tamanho - 1) # 2.976843
limite_superior <- media + t * desvio_padrao / sqrt(tamanho)
limite_superior # 352.6862

# b) amplitude do intervalo bilateral de confiança de 90%
# calcular z
confianca <- 90/100

x <- 1 - ((1-confianca)/2) # 0.95
t <- qt(x, tamanho - 1) #  1.76131

amplitude <- 2 * t * desvio_padrao / sqrt(tamanho)
amplitude # 9.095366

# c) tamamnho da amostra para intervalo de confiança de 90% apresentasse amplitude de no máximo 5 unidades
amplitude <- 5
confianca <- 90/100

x <- 1 - ((1-confianca)/2) # 0.95
t <- qt(x, tamanho - 1) #  1.76131
tamanho <- (2 * t * desvio_padrao / amplitude)^2
tamanho # 49.63541


############################
###### Questão 10 - 
############################

# comparar p-valor com alpha
# Se p-valor < alpha -> rejeita H0

# 1. Quais parâmetros de interesse?
# 2. Quais as hipóteses?
# 3. QUal o erro tipo II
# 4 e 5. comparar p-valor com alpha, se p-valor < alpha -> rejeita H0


############################
###### Questão 11 - 
############################

test_antes <- c(128, 128, 126, 118, 128, 120)
test_depois <- c(120,	120,	130,	114,	120,	116)

t.test(test_antes, test_depois, paired=T, alternative="greater") 
# t = 2.4445, df = 5, p-value = 0.02916


# a) unilateral a direita, unilateral esquerda ou bilateral?
# b) encontrar t
# c) encontrar valor-crítico
# d) p-valor em qual intervalao
# e) análise de hipótese


############################
###### Questão 12 - 
############################

# h0: ilegal = 50%
# h1: ilegal > 80%

total <- 1539
n_ilegal <- 1262
n_legal  <- total - n_ilegal
alpha <- 0.04

phat <- n_ilegal / total
p0 <- 0.8 # 80%

SE = sqrt(p0*(1-p0)/n)


phat + c(-1)*qnorm(0.96)*SE #IC apenas considerando a parte inferior do intervalo (hA é  "mais que 80%")

(phat-p0)/SE # z_calculado

qnorm(0.96) # z_critico unilateral

pnorm((phat-p0)/SE,lower.tail=F) # p-valor do teste unilateral (só multiplica por 2 quando são as duas caudas)



############################
###### Questão 13 - 
############################

confianca <- 99/100
desvio <- 0.045

# a) 

p <- 1/2

x <- 1 - ((1-confianca)/2) # 0.95
z <- qnorm (x, mean = 0, sd = 1, lower.tail = T)
n <- (z/desvio)^2 * p * (1 - p)
n # 815

#b) 

p <- 0.15
x <- 1 - ((1-confianca)/2) # 0.95
z <- qnorm (x, mean = 0, sd = 1, lower.tail = T)
n <- (z/desvio)^2 * p * (1 - p)
n # 415


############################
###### Questão 14 - 
############################

# y
teste_pro <- c(30, 40, 45, 26, 37, 33, 33, 28)

# x
teste_apt <- c(18, 19, 22, 16, 25, 22, 21, 20) 

modelo <- lm(teste_pro ~ teste_apt)
summary(modelo)

# y = b + a * x

b <- 8.283
a <- 1.262
# pro <- 8.283 + 1.262 * apt

# a. 1,262

# b. 8,283

# c. 
apt <- 20
pro <- b + a * apt
pro # 33.523

# d. 
# pro <- 8.283 + 1.262 * apt
pro <- 44
apt <- (pro - b) / a
apt
