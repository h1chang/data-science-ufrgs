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
t.test(amostra, alternative="two.sided", mu=mi0, conf.level=0.95) 
?t.test
qt(0.025,df=24,lower.tail = T) #t crÃ­tico
qt(0.025,df=24,lower.tail = F) #t crÃ­tico
#teste unilateral
t.test(amostra,alternative="greater",mu=mi0,conf.level=0.95) 
qt(0.05,df=24,lower.tail = F) #t crÃ­tico

# 1) O intervalo de 95% de confiança (62.12 ; 73.32) não inclui o ganho de peso da
# ração padrão (60kg), sendo esperado valor superior, concluindo-se que o novo
# tipo de alimentação deve dar maior ganho de peso que a ração padrão.
# 2)  Mesmo resultado pelo teste t, comparando o t-calculado = 2.847 com o
# t-crítico = 2.064 (valor crítico cai em uma das caudas ??? rejeita-se H0)
# 3) Ou ainda comprando o p-valor = 0.0089 com ??? = 0.05 (p < ??? ??? rejeita-se H0).



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
qt(0.05, df=7.825, lower.tail = T) #t crÃ­tico # -1.864954
qt(0.05, df=7.825, lower.tail = F) #t crÃ­tico # 1.864954

# B) tcrítico = 1.287 < t0.05 = 1.865
# p-valor = 0.2348 > alpha = 0.10
# (-1.887471 : 10.296995) inclui p 0 - não diferença entre N e C
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


#########################
# slide 15 - exercício 2
amostra_ex2 <- c(204, 108, 140, 152, 158, 129, 175, 146, 157, 174, 192, 194, 144, 152, 135)
summary(amostra_ex2)
shapiro.test(amostra_ex2) #teste de normalidade


#########################
# slide 18 - exercício 1
olho_dir <- c(44, 39, 33, 56, 43, 56, 47, 58)
olho_esq <- c(40, 37, 28, 53, 48, 51, 45, 60)
summary(olho_dir)
summary(olho_esq)
shapiro.test(olho_dir) #teste de normalidade
shapiro.test(olho_esq) #teste de normalidade
t.test(olho_dir, olho_esq, paired=T, alternative="two.sided") 

#########################
# slide 18 - exercício 2
dieta_antes <- c(54, 61, 50, 74, 79, 58, 55, 49, 63)
dieta_depois <- c(57, 66, 53, 73, 82, 58, 56, 53, 63)
summary(dieta_antes)
summary(dieta_depois)
shapiro.test(dieta_antes) #teste de normalidade
shapiro.test(dieta_depois) #teste de normalidade
t.test(dieta_antes, dieta_depois, paired=T, alternative="less") 

#########################
# slide 22 - exercício 1

metodoA <- c(16, 14, 19, 18, 19, 20, 15, 18, 17, 18)
metodoB <- c(13, 19, 14, 17, 21, 24, 10, 14, 13, 15)
summary(metodoA)
summary(metodoB)
shapiro.test(metodoA) #teste de normalidade
shapiro.test(metodoB) #teste de normalidade
var.test(metodoA,metodoB,alternative = c("two.sided"),conf.level = 0.9) # F test
t.test(metodoA,metodoB,alternative = c("two.sided"), var.equal=F,paired=F, conf.level = 0.9) 

#########################
# slide 22 - exercício 2
operarioA <- c(35, 32, 40, 36, 35, 32, 33)
operarioB <- c(29, 35, 36, 34, 30, 33, 31)
summary(operarioA)
summary(operarioB)
shapiro.test(operarioA) #teste de normalidade
shapiro.test(operarioB) #teste de normalidade
var.test(operarioA,operarioB,alternative = c("two.sided"),conf.level = 0.9) # F test
t.test(operarioA,operarioB,alternative = c("two.sided"), var.equal=F,paired=F, conf.level = 0.9) 


