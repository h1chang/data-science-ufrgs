
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
vector1 <- c(21, 15, 16, 18, 15, 20, 23, 28, 16, 28, 18)
summary(vector1)
##  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 15.00   16.00   18.00   19.82   22.00   28.00 

# Desvio padrão de uma coleção 
sd(vector1)


############################
###### Questão 3 - Histograma
############################

vector2 <- c(20.7, 20.8, 21.0, 21.4, 22.2, 22.4, 22.4, 22.4, 22.9, 23.0
             , 23.1, 23.3, 23.6, 23.7, 24.1, 24.1, 24.2, 24.6, 24.7, 25.0
             , 25.2, 25.8, 27.3, 27.4, 28.6, 28.8, 29.0, 29.6, 30.0, 31.4)

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

vector3 <- c(43, 44, 46, 46, 46, 
             46, 47, 47, 50, 53, 
             53, 54, 54, 58, 66)
boxplot(vector3)
boxplot(vector3, plot = F)$stats



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

media <- 21
desvio_padrao <- 2

# 1 V
1- pnorm(20.5, mean = media, sd = desvio_padrao, lower.tail = T)
##  0.5987063

# 2
pnorm(17.5, mean = media, sd = desvio_padrao, lower.tail = F)
## 0.9599408

# 3
0.99/2
0.5 + 0.495
0.5 - 0.495

qnorm(0.995, mean = media, sd = desvio_padrao, lower.tail = T)
# 26.15166
qnorm(0.005, mean = media, sd = desvio_padrao, lower.tail = T)
# 15.84834

# 4
pnorm(22.1, mean = media, sd = desvio_padrao, lower.tail = T)

# 5
qnorm(0.1, mean = media, sd = desvio_padrao, lower.tail = T)

x <- pnorm(22.6, mean = media, sd = desvio_padrao, lower.tail = T)
y <- pnorm(19.3, mean = media, sd = desvio_padrao, lower.tail = T)
x-y 
## [1] 0.1744598 V

# 3
2.5 * 1.5
24 + (2.5 * 1.5)
## 27.75

# 4
pnorm(23.4, mean = media, sd = desvio_padrao, lower.tail = T)
# 0.3445783

# 5
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
variancia <- 16
tamanho <- 576
z_calculado <- 0.1

desvio_padrao <- sqrt(variancia)
erro_padrao <- desvio_padrao / sqrt(tamanho)
valor_critico <- z_calculado / erro_padrao
prob <- 1 - 2 * pnorm(valor_critico, 0, 1, lower.tail = F)
prob
##  0.5762892


############################
###### Questão 8 - 
############################

tamanho <- 144
media <- 119.9
desvio_padrao <- 7
alpha <- 99/100 # confiança
# intervalo [118.4; 121.4]

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
tamanho <- 19
media <- 329
variancia <- 121

desvio_padrao <- sqrt(variancia)
desvio_padrao

# a) qual limite superior?
confianca <- 99/100

x <- 1 - ((1-confianca)/2) # 0.995
t <- qt(x, tamanho - 1) # 2.976843
limite_superior <- media + t * desvio_padrao / sqrt(tamanho)
limite_superior # 336.264

# b) amplitude do intervalo bilateral de confiança de 90%
# calcular z
confianca <- 98/100

x <- 1 - ((1-confianca)/2) # 0.95
t <- qt(x, tamanho - 1) #  1.76131

amplitude <- 2 * t * desvio_padrao / sqrt(tamanho)
amplitude # 12.88223

# c) tamamnho da amostra para intervalo de confiança de 90% apresentasse amplitude de no máximo 5 unidades
amplitude <- 9
confianca <- 98/100

x <- 1 - ((1-confianca)/2) # 0.95
t <- qt(x, tamanho - 1) #  1.76131
tamanho_novo <- (2 * t * desvio_padrao / amplitude)^2
tamanho_novo # 38.927


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

test_antes <- c(125, 126, 118, 124, 139, 124, 118, 110, 132)
test_depois <- c(118, 130, 114, 123, 130, 123, 114, 112, 124)

t.test(test_antes, test_depois, paired=T, alternative="greater") 
# t = 2.0812, df = 8, p-value = 0.03549

qt(0.99, 8)


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

n <- 1626
n_ilegal <- 1350
n_legal  <- n - n_ilegal
alpha <- 0.06

phat <- n_ilegal / n
p0 <- 0.8 # 80%

SE = sqrt(p0*(1-p0)/n)


phat + c(-1)*qnorm(0.94)*SE #IC apenas considerando a parte inferior do intervalo (hA é  "mais que 80%")

(phat-p0)/SE # z_calculado (Qual é o valor da estatística de teste?)

qnorm(0.94) # z_critico unilateral (valor crítico)

pnorm((phat-p0)/SE,lower.tail=F) # p-valor do teste unilateral (só multiplica por 2 quando são as duas caudas)



############################
###### Questão 13 - 
############################

confianca <- 90/100
desvio <- 0.055

# a) 

p <- 1/2

x <- 1 - ((1-confianca)/2) # 0.95
z <- qnorm (x, mean = 0, sd = 1, lower.tail = T)
n <- (z/desvio)^2 * p * (1 - p)
n # 223.5986

#b) 

p <- 0.29
x <- 1 - ((1-confianca)/2) # 0.95
z <- qnorm (x, mean = 0, sd = 1, lower.tail = T)
n <- (z/desvio)^2 * p * (1 - p)
n # 184.1558


############################
###### Questão 14 - 
############################

# y
teste_pro <- c(37, 30, 45, 33, 25, 38, 26, 28)

# x
teste_apt <- c(25, 18, 22, 22, 15, 23, 16, 20) 

modelo <- lm(teste_pro ~ teste_apt)
summary(modelo)

# y = b + a * x

b <- 1.8820
a <- 1.5338
# pro <- 8.283 + 1.262 * apt

# a. 1.5338

# b. 1.8820

# c. 
apt <- 15
pro <- b + a * apt
pro # 24.889

# d. 
# pro <- 8.283 + 1.262 * apt
pro <- 43
apt <- (pro - b) / a
apt # 26.80793
