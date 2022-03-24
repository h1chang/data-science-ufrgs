# Exemplo - slide 13 - Aula 4
mi0 <- 60 
amostra <- c(48,61,56,55,92,84,69,83,61,59,58,77,
             61,58,78,81,61,54,70,99,66,65,86,57,54) 
summary(amostra)
shapiro.test(amostra) #teste de normalidade
?shapiro.test
qqnorm(amostra)
qqline(amostra)
#teste bilateral
t.test(amostra,alternative="two.sided",mu=mi0,conf.level=0.95) 
?t.test
qt(0.025,df=24,lower.tail = T) #t crítico
qt(0.025,df=24,lower.tail = F) #t crítico
#teste unilateral
t.test(amostra,alternative="greater",mu=mi0,conf.level=0.95) 
qt(0.05,df=24,lower.tail = F) #t crítico



# Exemplo - slide 16 - Aula 4
maq_X <- c(80,72,65,78,85) 
maq_Y <- c(75,70,60,72,78) 
summary(maq_X)
summary(maq_Y)
shapiro.test(maq_X) #teste de normalidade
shapiro.test(maq_Y) #teste de normalidade
t.test(maq_X, maq_Y, paired=T, alternative="two.sided") 
qt(0.025,df=4,lower.tail = T) #t crítico
qt(0.025,df=4,lower.tail = F) #t crítico
dif<-maq_X-maq_Y #calculando as diferenças entre os pares
t.test(dif) #mesmo resultado do teste pareado



# Exemplo - slide 19 - Aula 4
C <- c(18.2,32.9,10.0,14.3,16.2,27.6,15.7)
N <- c(12.7,19.3,20.5,10.5,14.0,10.8,16.6,14.0,17.2)
summary(C)
summary(N)
shapiro.test(C) #teste de normalidade
shapiro.test(N) #teste de normalidade
var.test(C,N,alternative = c("two.sided"),conf.level = 0.9)
qf(0.05, df1=6, df2=8, lower.tail = TRUE) #f crítico
qf(0.05, df1=6, df2=8, lower.tail = FALSE) #f crítico
t.test(C,N,alternative = c("two.sided"), var.equal=F,paired=F, conf.level = 0.9) 
qt(0.05, df=7.825, lower.tail = T) #t crítico
qt(0.05, df=7.825, lower.tail = F) #t crítico


###############################################################################
# Exerc�cios Aula 4

# Fazer exerc�cios 1 e 2 dos slides 15, 18 e 22.
# Utilize os tr�s exemplos feitos em aula como refer�ncia (para resolver e interpretar).

# slide 15
