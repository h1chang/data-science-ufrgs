# Exemplo - slide 13 - Aula 4
mi0 <- 60 
amostra <- c(48,61,56,55,92,84,69,83,61,59,58,77,
             61,58,78,81,61,54,70,99,66,65,86,57,54) 
summary(amostra)

## H0: dadossãonormais / HA: dadosnãosãonormais
shapiro.test(amostra) #teste de normalidade
# normalidade aceita p = 0.0434 > alfa = 0.01
?shapiro.test
qqnorm(amostra)
qqline(amostra)

## H0: mi0 = 60 / mi0 != 60
#teste bilateral
# alpha = 0.05 -> conf.level = 0.95
t.test(amostra, alternative="two.sided", mu=mi0, conf.level=0.95) 
# resultado: t = 2.8472, p-value = 0.008895
?t.test
# alpha / 2 = 0.025
qt(0.025,df=24,lower.tail = T) #t crÃ­tico = 2.064
qt(0.025,df=24,lower.tail = F) #t crÃ­tico
#teste unilateral
t.test(amostra,alternative="greater",mu=mi0,conf.level=0.95) 
qt(0.05,df=24,lower.tail = F) #t crÃ­tico

# 1) O intervalo de 95% de confiança (62.12 ; 73.32) não inclui o ganho de peso da
# ração padrão (60kg), sendo esperado valor superior, concluindo-se que o novo
# tipo de alimentação deve dar maior ganho de peso que a ração padrão.
# 2)  Mesmo resultado pelo teste t, comparando o t-calculado = 2.847 com o
# t-crítico = 2.064 (valor crítico cai em uma das caudas ??? rejeita-se H0)
# 3) Ou ainda comprando o p-valor = 0.0089 com alpha = 0.05 (p < alpha -> rejeita-se H0).



# Exemplo - slide 16 - Aula 4
maq_X <- c(80,72,65,78,85) 
maq_Y <- c(75,70,60,72,78) 
summary(maq_X)
summary(maq_Y)
shapiro.test(maq_X) #teste de normalidade
shapiro.test(maq_Y) #teste de normalidade
t.test(maq_X, maq_Y, paired=T, alternative="two.sided") 
qt(0.025,df=4,lower.tail = T) #t crÃ­tico
qt(0.025,df=4,lower.tail = F) #t crÃ­tico
dif<-maq_X-maq_Y #calculando as diferenÃ§as entre os pares
t.test(dif) #mesmo resultado do teste pareado

# 1) O intervalo de 95% de confiança (2.677 ; 7.323) não inclui o valor 0 (não
# diferença entre X e Y), sendo esperado valor superior, concluindo-se que a
# máquina X deve ter tempo para realizar a tarefa maior que na máquina Y,
# sendo a máquina Y de maior facilidade de aprendizagem.
# 2) Mesmo resultado pelo teste t, comparando o t-calculado = 5.976 com o
# t-crítico = 2.776 (valor crítico cai em uma das caudas -> rejeita-se H0)
# 3) Ou ainda comprando o p-valor = 0.0039 com alpha = 0.05 (p < alpha -> rejeita-se H0).



# Exemplo - slide 19 - Aula 4
C <- c(18.2,32.9,10.0,14.3,16.2,27.6,15.7)
N <- c(12.7,19.3,20.5,10.5,14.0,10.8,16.6,14.0,17.2)
summary(C)
summary(N)
shapiro.test(C) #teste de normalidade
shapiro.test(N) #teste de normalidade

var.test(C,N,alternative = c("two.sided"),conf.level = 0.9) # F test
qf(0.05, df1=6, df2=8, lower.tail = TRUE) #f crÃ­tico # 0.2411496
qf(0.05, df1=6, df2=8, lower.tail = FALSE) #f crÃ­tico # 3.58058

# A) F = 5.123 > fcrítico = 3.58
#    p-valor = 0.038 < alpha = 0.1
#    Rejeita-se H0. Deve-se considerar a desigualdade de variâncias
#   var.equal = F

t.test(C,N,alternative = c("two.sided"), var.equal=F,paired=F, conf.level = 0.9) 
# t = 1.2872, df = 7.825, p-value = 0.2348
qt(0.05, df=7.825, lower.tail = T) #t crÃ­tico # -1.864954
qt(0.05, df=7.825, lower.tail = F) #t crÃ­tico # 1.864954

# B) tcrítico = 1.287 < t0.05 = 1.865 (fora da cauda)
# p-valor = 0.2348 > alpha = 0.10
# (-1.887471 : 10.296995) inclui o 0 - não diferença entre N e C
# Não se rejeita H0. Com signigficância de 5%, não foi possível verificar
# se existe efeito significativo do nitrato sobre o ganho de peso.


###############################################################################
# Exercícios Aula 4

# Fazer exercícios 1 e 2 dos slides 15, 18 e 22.
# Utilize os três exemplos feitos em aula como referência (para resolver e interpretar).
#########################
# slide 15 - exercício 1
mi0_ex1 <- 60 
amostra_ex1 <- c(27.2, 29.3, 31.5, 28.7, 30.2, 29.6) 
summary(amostra_ex1)
shapiro.test(amostra_ex1) #teste de normalidade
## normalidade aceita p = 0.988 > alfa = 0.01

# alpha = 0.05 -> conf.level = 0.95
## H0: mi0 = 30 / mi0 != 30
t.test(amostra_ex1, alternative="two.sided", mu=30, conf.level=0.95) 
# resultado: t = -0.9894, p-value = 0.3679
# alpha / 2 = 0.025
qt(0.025,df=5,lower.tail = T) #t crÃ­tico = -2.570582

# 1) O intervalo de 95% de confiança (27.90 ; 30.93) inclui o valor 30.
# 2)  Comparando o t-calculado = -0.9894 com o t-crítico = +-2.570582
# (valor crítico não cai em uma das caudas -> aceita H0)
# 3) Comprando o p-valor = 0.3679 com alpha = 0.05 (p > alpha -> aceita H0).



#########################
# slide 15 - exercício 2
amostra_ex2 <- c(204, 108, 140, 152, 158, 129, 175, 146, 157, 174, 192, 194, 144, 152, 135)
summary(amostra_ex2)
shapiro.test(amostra_ex2) #teste de normalidade
## normalidade aceita p = 0.8541 > alfa = 0.01

# alpha = 0.05 -> conf.level = 0.95
## H0: mi0 < 190 / mi0 != 190
t.test(amostra_ex2,alternative="less",mu=190,conf.level=0.95) 
# resultado: t = -4.8032, p-value = 0.0001404
qt(0.05,df=14,lower.tail = T) #t crÃ­tico = -1.76131

# 1) O intervalo de 95% de confiança ( < 169.3121) não inclui a média normal 190.
# Falso para "menor que 190"
# 2)  Comparando o t-calculado = -4.8032 com o t-crítico = -1.76131
# (valor crítico cai em uma das caudas -> rejeita H0)
# 3) Comprando o p-valor =0.0001404 com alpha = 0.05 (p < alpha -> não aceita H0).


#########################
# slide 18 - exercício 1
olho_dir <- c(44, 39, 33, 56, 43, 56, 47, 58)
olho_esq <- c(40, 37, 28, 53, 48, 51, 45, 60)
summary(olho_dir)
summary(olho_esq)
shapiro.test(olho_dir) #teste de normalidade
shapiro.test(olho_esq) #teste de normalidade
t.test(olho_dir, olho_esq, paired=T, alternative="two.sided") 
# t = 1.4, p-value = 0.2042, intervalo confiança  -1.20578  4.70578
qt(0.025,df=7,lower.tail = T) #t crÃ­tico = 2.365
qt(0.025,df=7,lower.tail = F) #t crÃ­tico

# 1) O intervalo de 95% de confiança (-1.20578  4.70578) inclui o valor 0 (diferença entre X e Y).
# 2) Comparando o t-calculado = 1.4 com o t-crítico = 2.365
# (valor crítico não cai em uma das caudas -> aceita  H0 -> diferença = 0)
# 3) Ou ainda comprando o p-valor = 0.2042 com alpha = 0.05 (p > alpha -> aceita H0).

#########################
# slide 18 - exercício 2
dieta_antes <- c(54, 61, 50, 74, 79, 58, 55, 49, 63)
dieta_depois <- c(57, 66, 53, 73, 82, 58, 56, 53, 63)
summary(dieta_antes)
summary(dieta_depois)
shapiro.test(dieta_antes) #teste de normalidade
shapiro.test(dieta_depois) #teste de normalidade
t.test(dieta_antes, dieta_depois, paired=T, alternative="less") 

# t = -2.9104, p-value = 0.009788, intervalo confiança  -Inf -0.7221478
qt(0.05,df=8,lower.tail = T) #t crÃ­tico = -1.859548

dif<-maq_X-maq_Y #calculando as diferenÃ§as entre os pares
t.test(dif) #mesmo resultado do teste pareado

# 1) O intervalo de 95% de confiança (-Inf -0.7221478) não inlcui o valor 0 
# (existe diferença entre X e Y ).
# 2) Comparando o t-calculado = -2.9104 com o t-crítico = -1.859548
# (valor crítico cai em uma das caudas -> rejeita  H0 -> diferença != 0)
# 3) Ou ainda comprando o p-valor = 0.009788 com alpha = 0.05 (p < alpha -> rejeita H0).


#########################
# slide 22 - exercício 1

metodoA <- c(16, 14, 19, 18, 19, 20, 15, 18, 17, 18)
metodoB <- c(13, 19, 14, 17, 21, 24, 10, 14, 13, 15)
summary(metodoA)
summary(metodoB)
shapiro.test(metodoA) #teste de normalidade
shapiro.test(metodoB) #teste de normalidade

var.test(metodoA,metodoB,alternative = c("two.sided"),conf.level = 0.9) # F test
# F = 0.2, p-value = 0.02507
qf(0.05, df1=9, df2=9, lower.tail = TRUE) # 0.3145749
qf(0.05, df1=9, df2=9, lower.tail = FALSE) # 3.178893

# A) F = 0.2 < fcrítico = 0.3145749 (dentro da cauda)
#    p-value = 0.02507 < alpha = 0.1
#    Rejeita-se H0. Deve-se considerar a desigualdade de variâncias
#   var.equal = F

t.test(metodoA,metodoB,alternative = c("two.sided"), var.equal=F,paired=F, conf.level = 0.9)
# t = 0.95258, df = 12.462, p-value = 0.3589
qt(0.025, df=12.462, lower.tail = T) #-2.169888
qt(0.025, df=12.462, lower.tail = F) #+2.169888
# B) t = 0.95258 > t0.025 = 2.169888 (fora da cauda)
# p-valor = 0.3589 > alpha = 0.10
# (-1.211357  4.011357) inclui o 0 - não diferença entre N e C
# Não se rejeita H0. Com signigficância de 5%, não foi possível verificar
# se existe efeito significativo entre os métodos.


#########################
# slide 22 - exercício 2
operarioA <- c(35, 32, 40, 36, 35, 32, 33)
operarioB <- c(29, 35, 36, 34, 30, 33, 31)
summary(operarioA)
summary(operarioB)
shapiro.test(operarioA) #teste de normalidade
shapiro.test(operarioB) #teste de normalidade

var.test(operarioA,operarioB,alternative = c("two.sided"),conf.level = 0.9) # F test
# F = 1.137, p-value = 0.8801
qf(0.05, df1=6, df2=6, lower.tail = TRUE) #  0.233434
qf(0.05, df1=6, df2=6, lower.tail = FALSE) # 4.283866

# A) F = 1.137 < fcrítico = 4.283866 (fora da cauda)
#    p-value = 0.8801 > alpha = 0.1
#    Aceita H0. Não se deve considerar a desigualdade de variâncias
#   var.equal = T
?t.test
t.test(operarioA,operarioB,alternative = c("two.sided"), var.equal=T,paired=F, conf.level = 0.9)
# t = 1.4709, df = 12, p-value = 0.1671
qt(0.025, df=12, lower.tail = T) #-2.178813
qt(0.025, df=12, lower.tail = F) #2.178813
# B) t = 1.4709 < t2.178813 (fora da cauda)
# p-valor = 0.1671 > alpha = 0.10
# (-0.4536912  4.7394054) inclui o 0 - não diferença entre N e C
# Não se rejeita H0. Com signigficância de 5%, não foi possível verificar
# se existe diferença de eficácia entre os operários.

