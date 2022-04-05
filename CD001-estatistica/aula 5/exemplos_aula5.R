#  IC e TH para uma proporção - slide 6 - aula 5
n = 1028
x = 576
phat = x/n
p0 = 0.50
SE = sqrt(p0*(1-p0)/n)
phat + c(-1, 1)*qnorm(0.025,lower.tail = F)*SE #IC
(phat-p0)/SE # z_calculado
qnorm(.975) # z_critico
2*pnorm((phat-p0)/SE,lower.tail=F) # p-valor

prop.test(576,1028,p=0.5,alternative="two.sided",correct = F,conf.level = 0.95) #aproximado

binom.test(576,1028,p=0.5,alternative="two.sided",conf.level = 0.95) #exato



# TH e IC para duas proporções - slide 11
n1 = 44925
x1 = 500
n2 = 44910
x2 = 505
phat1 = x1/n1
phat2 = x2/n2
SE = sqrt((phat1*(1-phat1)/n1)+(phat2*(1-phat2)/n2))
(phat1-phat2) + c(-1, +1)*qnorm(.975)*SE #IC
(phat1-phat2)/SE # z_calculado
qnorm(.975) # z_critico
2*pnorm((phat1-phat2)/SE,lower.tail=T) # p-valor

prop.test(x=c(500,505),n=c(44925,44910),alternative="two.sided",correct = F,conf.level = 0.95) #aproximado

fisher.test(matrix(c(500,44425,505,44405),nrow=2),alternative="two.sided",conf.level = 0.95) #exato



# teste qui-quadrado de independência - slide 16
dados=matrix(c(3511,1489,1749,751,1818,682),nrow=2)
chisq.test(dados)

# valor crítico
qchisq(0.05,df=2,lower.tail=FALSE)

# análise de resíduos
chisq.test(dados)$stdres




