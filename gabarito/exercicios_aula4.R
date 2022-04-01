
# Exercício 1 - slide 15 - Aula 4
mi0 <- 30 
amostra <- c(27.2,29.3,31.5,28.7,30.2,29.6) 
summary(amostra)
shapiro.test(amostra)
t.test(amostra,mu=mi0,alternative="two.sided") 
qt(0.025,df=5,lower.tail=T)
qt(0.025,df=5,lower.tail=F)


# Exercício 2 - slide 15 - Aula 4
mi0 <- 190 
amostra <- c(204,108,140,152,158,129,175,146,157,174,192,194,144,152,135) 
summary(amostra)
shapiro.test(amostra)
t.test(amostra,mu=mi0,alternative="less") 
qt(0.05,df=5,lower.tail=T) #t crítico somente na cauda da esquerda


# Exercício 1 - slide 18 - Aula 4
direito <- c(44,39,33,56,43,56,47,58) 
esquerdo <- c(40,37,28,53,48,51,45,60) 
summary(direito)
summary(esquerdo)
shapiro.test(direito) 
shapiro.test(esquerdo) 
t.test(direito, esquerdo,paired=T) 
qt(0.025,df=7,lower.tail=T)
qt(0.025,df=7,lower.tail=F)


# Exercício 2 - slide 18 - Aula 4
antes <- c(54,61,50,74,79,58,55,49,63) 
depois <- c(57,66,53,73,82,58,56,53,63) 
summary(antes)
summary(depois)
shapiro.test(antes) 
shapiro.test(depois) 
t.test(antes,depois,paired=T,alternative="less") 
qt(0.05,df=8,lower.tai =TRUE) #t crítico somente na cauda da esquerda


# Exercício 1 - slide 22 - Aula 4
A <- c(16,14,19,18,19,20,15,18,17,18 )
B <- c(13,19,14,17,21,24,10,14,13,15)
summary(A)
summary(B)
shapiro.test(A) 
shapiro.test(B)
var.test(B,A,alternative=c("two.sided"),conf.level = 0.95)
qf(0.025,df1=9,df2=9,lower.tail=T)
qf(0.025,df1=9,df2=9,lower.tail=F)
t.test(A,B,alternative=c("two.sided"),var.equal=F,conf.level=0.95) 
qt(0.025,df=12.46,lower.tail=T)
qt(0.025,df=12.46,lower.tail=F)


# Exercício 2 - slide 22 - Aula 4
A <- c(35,32,40,36,35,32,33)
B <- c(29,35,36,34,30,33,31)
summary(A)
summary(B)
shapiro.test(A) 
shapiro.test(B)
var.test(A,B,alternative=c("two.sided"),conf.level=0.95)
qf(0.025,df1=6,df2=6,lower.tail = T)
qf(0.025,df1=6,df2=6,lower.tail = F)
t.test(A,B,alternative=c("two.sided"),var.equal=T,conf.level=0.95) 
qt(0.025,df=12,lower.tail =T)
qt(0.025,df=12,lower.tail =F)


# erro da função inference do openintro
X<-c(rep("A",7),rep("B",7))
Y<-c(A,B)
inference(Y,X,est="mean",type="ht",alternative="twosided",method="theoretical") #erro dos gl 
