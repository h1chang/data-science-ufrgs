############################
###### Questão 1 - Definições
############################

# Estatística
# Amostra, amostragem
# População
# Média, desvio padrão
# Parâmetro




############################
###### Questão 2 - Média, Mediana e Desvio Padrão
############################

# criar array 


# descobris estatísticas
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

tamanho <- 400
z_calculado <- 0.2
variancia <- 25
desvio_padrao <- sqrt(variancia)
erro_padrao <- desvio_padrao / sqrt(tamanho)
valor_critico <- z_calculado / erro_padrao
prob <- 1 - 2 * pnorm(valor_critico, 0, 1, lower.tail = F)
prob
##  0.5762892


############################
###### Questão 8 - 
############################

# Mantendo-se tudo mais fixo, quando aumentamos (diminuímos) o tamanho da amostra, a amplitude do intervalo é diminuida (aumentada).
# Mantendo-se tudo mais fixo, conforme a confiança do intervalo aumenta (diminui), a sua amplitude tende a aumentar (diminuir).
# Mantendo-se tudo mais fixo, quanto maior (menor) for a variância, maior (menor) será a amplitude do intervalo de confiança para a média populacional.

############################
###### Questão 9 - 
############################

# a) limite superior para intervalo de confiança 99%
tamanho <- 15
media <- 345
variancia <- 100
desvio_padrao <- sqrt(variancia)
1- 1/100/2 # 0.995
qt(0.995, tamanho - 1) # 2.976843
limite_superior <- media + 2.976843 * desvio_padrao / sqrt(tamanho)
limite_superior # 352.6862

# b) amplitude do intervalo bilateral de confiança de 90%
# calcular z
1- 10/100/2 # 0.95
qt(0.95, tamanho - 1) #  1.76131

amplitude <- 2 * 1.76131 * desvio_padrao / sqrt(tamanho)
amplitude # 9.095366

# c) tamamnho da amostra para intervalo de confiança de 90% apresentasse amplitude de no máximo 5 unidades
amplitude <- 5
tamanho <- (2 * 1.76131 * desvio_padrao / amplitude)^2
tamanho # 49.63541


############################
###### Questão 10 - 
############################



############################
###### Questão 11 - 
############################

test_antes <- c(128, 128, 126, 118, 128, 120)
test_depois <- c(120,	120,	130,	114,	120,	116)

t.test(test_antes, test_depois, paired=T, alternative="greater") 


# a) unilateral a direita unilateral esquerda?
# b) 2.44
# c) 
tamanho <- 6
qt(0.95, tamanho - 1) #  1.676793
# d) 0.02916 - 2
# e) 


############################
###### Questão 12 - 
############################




############################
###### Questão 13 - 
############################

# a) 
intervalo_confianca <- 99/100
p <- 1/2
desvio <- 0.045
z <- 2.57 # 99%
n <- (z/desvio)^2 * p * (1 - p)
n # 815

#b) 
intervalo_confianca <- 99/100
p <- 0.15
desvio <- 0.045
z <- 2.57 # 99%
n <- (z/desvio)^2 * p * (1 - p)
n # 415


############################
###### Questão 14 - 
############################

teste_pro <- c(30, 40, 45, 26, 37, 33, 33, 28)
teste_apt <- c(18, 19, 22, 16, 25, 22, 21, 20)

modelo <- lm(teste_pro ~ teste_apt)
summary(modelo)
# pro <- 8.283 + 1.262 * apt

# a. 1,262

# b. 8,283


# c. 
apt <- 20
pro <- 8.283 + 1.262 * apt
pro # 33.523

# d. 
# pro <- 8.283 + 1.262 * apt
pro <- 44
apt <- (pro - 8.283) / 1.262
apt
