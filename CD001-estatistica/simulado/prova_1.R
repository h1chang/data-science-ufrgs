
############################
###### Questão 1 - Definições
############################

# Qualitativa nominal /// sem ordem 
# Qualitativa ordinal // com ordem
# Qualitativa constante

# Quatitativa contínua // infinito
# Quantitativa discreta // finito
# Quantitativa ordinal

# Quantitativa constante


# qualitativa = não numéricas
# Quantitativa = numéricas



############################
###### Questão 2 - Média, mediana e moda
############################

# descobrir estatísticas
vector1 <- c(275, 373, 470, 373, 398, 403, 301, 396, 231, 396, 426, 285, 373, 396)
summary(vector1)
## Média
364.0

## Mediana

384.5
## Moda
hist(vector1)
373
396
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(vector1)


############################
###### Questão 3 - boxplot e histograma
############################

#unimodal: Falso (2 picos)

#outliers: Sim

#1Q > : 3,2
#1Q < : 4,7
# 1/2 : 4


############################
###### Questão 4 - conjuntos
############################

n <- 200

# I - F
15/n

# II - F
(110+40+10+5+20)/200

# III - F
(20+15+5+110)/200

############################
###### Questão 4 - normal
############################

# qnorm para aplicar a probabilidade(%) e encontrar valores
# Ex. qnorm(0.05, mean = media, sd = desvio_padrao, lower.tail = T)

# pnorm para aplicar valores e encontrar a probabilidade (%)
# Ex. pnorm(20.5, mean = media, sd = desvio_padrao, lower.tail = T)

media <- 6.5
desvio_padrao <- 1.2

# nota > 7.5 e < 9 = conceito B
alunos_b <- 6

# Quantos alunos fizeram a disciplina?
1 - pnorm(7.5, mean = media, sd = desvio_padrao, lower.tail = T)
pnorm(7.5, mean = media, sd = desvio_padrao, lower.tail = F)
perc_maior_75 <- pnorm(7.5, mean = media, sd = desvio_padrao, lower.tail = F)
perc_maior_75

perc_maior_90 <- pnorm(9, mean = media, sd = desvio_padrao, lower.tail = F)
perc_maior_90

perc_alunos_b <- perc_maior_75 - perc_maior_90
perc_alunos_b

alunos_total <- alunos_b / perc_alunos_b
alunos_total
# 32.65876

# 5% top podem concorrer a bolsa. QUal a nota?
nota_perc_5 <- qnorm(0.05, mean = media, sd = desvio_padrao, lower.tail = F)
nota_perc_5

############################
###### Questão 5 
############################


tamanho <- 1000
media <- 171.5
desvio_padrao <- 7.2

n_amostras <- 500
tamanho_amostra <- 19

30 * tamanho_amostra
# n_amostras < 30 * tamanho_amostra (população finita)

# poop infinita
erro_padrao <- desvio_padrao / sqrt(tamanho)
erro_padrao # 0.227684

# pop finita
a <- sqrt((tamanho - tamanho_amostra)/(tamanho - 1))
b <- desvio_padrao / sqrt(tamanho)
erro_padrao <- a* b
erro_padrao #0.2256235

###########

desvio_padrao <- 0.2256235
media <- 171.5

a<- pnorm(174.8, mean = media, sd = desvio_padrao, lower.tail = T)
b<- pnorm(169.3, mean = media, sd = desvio_padrao, lower.tail = T)
a-b
500 * (a-b) #  148.336

###############

c<- pnorm(169, mean = media, sd = desvio_padrao, lower.tail = T)
500 * c # 182.1061



############################
###### Questão 7 
############################




############################
###### Questão 8 
############################
tamanho <- 12
media <- 60
amostra <- c(65, 55, 63, 65, 58, 69, 57, 60, 60, 66, 68, 68)


t.test(amostra,alternative="two.sided",mu=media,conf.level=0.99)



############################
###### Questão 9 
############################

# rendimento pelo menos 6.5
desvio_padrao <- 1.5
tamanho <- 20

# teste hipoese. verificação mais detalhada se rendimento < 6.5
# H0 >= 6.5 rendimento <= 6.5 precisa 
# H1 < 6.5

############################
###### Questão 10 
############################

invasiva_a <- c(63, 63, 63, 62, 54, 65, 63) 
invasiva_b <- c(65, 80, 65, 80, 78, 70, 65)
summary(invasiva_a)
summary(invasiva_b)

t.test(invasiva_a, invasiva_b, paired=T, alternative="less") 
# t = -2.8257, df = 6, p-value = 0.01506

qt(0.05,df=6,lower.tail = T) #t crÃ­tico = -1.859548




############################
###### Questão 11 
############################

t_onibus <- c(10, 18, 15, 20, 12, 18, 10, 10, 11)
t_carro <- c(15, 25, 20, 20, 17, 25, 18, 18, 10)

t.test(t_onibus, t_carro, paired=T, alternative="greater") 
?t.test

# t = -4.4504, df = 8, p-value = 0.9989
qt(0.1,df=8,lower.tail = T) #t crÃ­tico = -1.859548



############################
###### Questão 12 
############################

n_microsoft <- 305
n_nintendo <- 167

alpha <- 1/100

n1 = 305
x1 = 65/100 * n_microsoft
n2 = 167
x2 = 45/100 * n_nintendo
phat1 = x1/n1
phat2 = x2/n2
SE = sqrt((phat1*(1-phat1)/n1)+(phat2*(1-phat2)/n2))
(phat1-phat2)- + c(-1, 1)*qnorm(.99)*SE #IC
#[1] 0.2925123 0.1074877
(phat1-phat2)/SE # z_calculado
# 4.237195
qnorm(.99) # z_critico
# 2.326348

# z_calculado > z_critico, rejeitamos h0





############################
###### Questão 13
############################



n1 = 220
x1 = 65
n2 = 65
x2 = 37
phat1 = x1/n1
phat2 = x2/n2
SE = sqrt((phat1*(1-phat1)/n1)+(phat2*(1-phat2)/n2))
(phat1-phat2)- + c(-1, 1)*qnorm(.9)*SE #IC
#[1] -0.001490590 0.001260488
(phat1-phat2)/SE # z_calculado
#[1] -0.1639328
qnorm(.9) # z_critico
#[1] 1.959964
2*pnorm((phat1-phat2)/SE,lower.tail=T) # p-valor
#[1] 0.869784














############################
###### Questão 14
############################


# y
amostra_nota <- c(65, 90, 40, 45, 35, 50, 65, 80)

# x
amostra_hora <- c(60, 105, 45, 55, 30, 65, 75, 80) 

modelo <- lm(amostra_nota ~ amostra_hora)
summary(modelo)

# y = b + a * x

b <- 7.1404
a <- 0.8017
# nota <- 7.1404 + 0.8017 * hora

# a. 1.5338

# b. 1.8820

# c. 
hora <- 40
nota <- b + a * hora
nota # 39.2084

# d. 
# nota <- b + a * hora
nota <- 60
hora <- (nota - b) / a
hora # 65.93439





