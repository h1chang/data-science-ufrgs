
############################
###### Quest�o 1 - Defini��es
############################

# Estat�stica
# Amostra, amostragem
# Popula��o
# M�dia, desvio padr�o
# Par�metro
# Censo

# Declara��es corretas

# Temos o interesse na altura m�dia dos jogadores de futebol profissional no Brasil. As alturas de 23% dos jogadores profissionais dos diversos clubes
# nacionais s�o medidas. Ent�o, a m�dia e o desvio padr�o das alturas s�o calculadas. Dessa forma, se observou uma amostra dos jogadores profissionais

# Suponha que uma parte os alunos de uma escola foram investigados com o objetivo de saber o seu desempenho (bom, ruim e regular) nas aulas
# ass�ncronas de matem�tica. O resultado desta propor��o foi de 37% para desempenho bom, o que representa um estat�stica

# A abordagem estat�stica envolvida ao generalizarmos resultados de uma amostra para toda uma popula��o � chamada infer�ncia estat�stica

# Estat�stica � a medida usada para descrever uma caracter�stica num�rica da amostra.

# Popula��o � o conjunto formado por todos os elementos ou resultados sob investiga��o.

# Suponha que todos os alunos de uma escola foram investigados com o objetivo de saber o seu desempenho (bom, ruim e regular) nas aulas ass�ncronas de matem�tica. O resultado desta
# propor��o foi de 40% para desempenho bom, o que representa um par�metro




############################
###### Quest�o 2 - M�dia, Mediana e Desvio Padr�o
############################

# criar array 


# descobrir estat�sticas
vector1 <- c(21, 15, 16, 18, 15, 20, 23, 28, 16, 28, 18)
summary(vector1)
##  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 15.00   16.00   18.00   19.82   22.00   28.00 

# Desvio padr�o de uma cole��o 
sd(vector1)


############################
###### Quest�o 3 - Histograma
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
###### Quest�o 4 - Interpreta��o boxplot
############################

vector3 <- c(43, 44, 46, 46, 46, 
             46, 47, 47, 50, 53, 
             53, 54, 54, 58, 66)
boxplot(vector3)
boxplot(vector3, plot = F)$stats



############################
###### Quest�o 5 - Probabilidade
############################

# Uni�o = adi��o
# interse��o = produto
# Espa�o amostral = todos resultados poss�veis
# Elementos disjuntos


############################
###### Quest�o 6 - Distribui��o Normal
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
###### Quest�o 7 - 
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
###### Quest�o 8 - 
############################

tamanho <- 121
media <- 130.1
desvio_padrao <- 9
alpha <- 98/100 # confian�a
# intervalo [128.2; 132]

erro_padrao <- desvio_padrao/sqrt(tamanho)
z <- qnorm (1 - (1 - alpha)/2, mean = 0, sd = 1, lower.tail = T)

media + z * erro_padrao
media - z * erro_padrao

# 1) encontrar intervelo
# 2) interpreta��o do intervalo
# 3) Mantendo-se tudo mais fixo, quando aumentamos (diminu�mos) o tamanho da amostra, a amplitude do intervalo � diminuida (aumentada).
# 4) Mantendo-se tudo mais fixo, conforme a confian�a do intervalo aumenta (diminui), a sua amplitude tende a aumentar (diminuir).
# 5) Mantendo-se tudo mais fixo, quanto maior (menor) for a vari�ncia, maior (menor) ser� a amplitude do intervalo de confian�a para a m�dia populacional.

############################
###### Quest�o 9 - 
############################

# a) limite superior para intervalo de confian�a 99%
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

# b) amplitude do intervalo bilateral de confian�a de 90%
# calcular z
confianca <- 90/100

x <- 1 - ((1-confianca)/2) # 0.95
t <- qt(x, tamanho - 1) #  1.76131

amplitude <- 2 * t * desvio_padrao / sqrt(tamanho)
amplitude # 9.095366

# c) tamamnho da amostra para intervalo de confian�a de 90% apresentasse amplitude de no m�ximo 5 unidades
amplitude <- 5
confianca <- 90/100

x <- 1 - ((1-confianca)/2) # 0.95
t <- qt(x, tamanho - 1) #  1.76131
tamanho <- (2 * t * desvio_padrao / amplitude)^2
tamanho # 49.63541


############################
###### Quest�o 10 - 
############################

# comparar p-valor com alpha
# Se p-valor < alpha -> rejeita H0

# 1. Quais par�metros de interesse?
# 2. Quais as hip�teses?
# 3. QUal o erro tipo II
# 4 e 5. comparar p-valor com alpha, se p-valor < alpha -> rejeita H0


############################
###### Quest�o 11 - 
############################

test_antes <- c(128, 128, 126, 118, 128, 120)
test_depois <- c(120,	120,	130,	114,	120,	116)

t.test(test_antes, test_depois, paired=T, alternative="greater") 
# t = 2.4445, df = 5, p-value = 0.02916


# a) unilateral a direita, unilateral esquerda ou bilateral?
# b) encontrar t
# c) encontrar valor-cr�tico
# d) p-valor em qual intervalao
# e) an�lise de hip�tese


############################
###### Quest�o 12 - 
############################

# h0: ilegal = 50%
# h1: ilegal > 80%

n <- 1539
n_ilegal <- 1262
n_legal  <- n - n_ilegal
alpha <- 0.04

phat <- n_ilegal / n
p0 <- 0.8 # 80%

SE = sqrt(p0*(1-p0)/n)


phat + c(-1)*qnorm(0.96)*SE #IC apenas considerando a parte inferior do intervalo (hA �  "mais que 80%")

(phat-p0)/SE # z_calculado (Qual � o valor da estat�stica de teste?)

qnorm(0.96) # z_critico unilateral (valor cr�tico)

pnorm((phat-p0)/SE,lower.tail=F) # p-valor do teste unilateral (s� multiplica por 2 quando s�o as duas caudas)



############################
###### Quest�o 13 - 
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
###### Quest�o 14 - 
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
