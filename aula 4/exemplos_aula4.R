# Exemplo - slide 13 - Aula 4
mi0 <- 60 
amostra <- c(48,61,56,55,92,84,69,83,61,59,
             58,77,61,58,78,81,61,54,70,99,66,65,86,57,54) 
summary(amostra)
shapiro.test(amostra)
?shapiro.test
# teste bilateral
t.test(amostra, alternative = "two.sided", mu = mi0, conf.level = 0.95) 
?t.test
qt(0.025,df=24,lower.tail = T)
qt(0.025,df=24,lower.tail = F) #t crítico


inference(amostra, est = "mean", type = "ht", null = mi0,
          alternative = "twosided", method = "theoretical")

# teste unilateral
t.test(amostra, alternative = "greater", mu = mi0, conf.level = 0.95) 
?t.test
qt(0.05,df=24,lower.tail = F)




# Exemplo - slide 16 - Aula 4
maq_X <- c(80,72,65,78,85) 
maq_Y <- c(75,70,60,72,78) 
summary(maq_X)
summary(maq_Y)
shapiro.test(maq_X) 
shapiro.test(maq_Y) 
t.test(maq_X, maq_Y, paired=T, alternative="two.sided") # duas amostras
qt(0.025,df=4,lower.tail = T)
qt(0.025,df=4,lower.tail = F)
dif<-maq_X-maq_Y #calculando as diferenÃ§as entre os pares
t.test(dif) #mesmo resultado do teste pareado (média das diferenças)
# teste pareado = diferenças das amostras




# Exemplo - slide 19 - Aula 4 (2 médias independentes)
C <- c(18.2,32.9,10.0,14.3,16.2,27.6,15.7)
N <- c(12.7,19.3,20.5,10.5,14.0,10.8,16.6,14.0,17.2)

# amsotras pequenas, tamanhos diferentes -> testar normal
summary(C)
summary(N)
shapiro.test(C) # normal OK > 0.01
shapiro.test(N) # normal mais OK > 0.01

# médias idependentes = 2 formas de calcular testeT (A e B)
# 1) fórmula A: variâncias iguais  2) fórmula B: variâncias diferentes
# 1) var.equal=T 2) var.equal=F

# descobrir variância
var.test(C,N,alternative = c("two.sided"),conf.level = 0.9)
# p-value = 0.0381 < 10% (rejeitar h0) = variâncias diferentes


qf(0.05, df1=6, df2=8, lower.tail = TRUE)
qf(0.05, df1=6, df2=8, lower.tail = FALSE)

t.test(C,N,alternative = c("two.sided"), var.equal=F, conf.level = 0.9) 
qt(0.05, df=7.825, lower.tail = T)
qt(0.05, df=7.825, lower.tail = F)

