#  IC e TH para uma proporÃ§Ã£o - slide 6 - aula 5
n = 1028 # adultos norte americanos
56/100 * 1028 # 56 % apoiam a redução de armas nucleares
x = 576
phat = x/n
p0 = 0.50
SE = sqrt(p0*(1-p0)/n)
phat + c(-1, 1)*qnorm(0.025,lower.tail = F)*SE #IC
(phat-p0)/SE # z_calculado
qnorm(.975) # z_critico
2*pnorm((phat-p0)/SE,lower.tail=F) # p-valor < 0.0001, rejeitamos H0
# z calculado = 3.867 > z crítico = 1.96, rejeitamos H0
# p-valor é a soma das duas caudas extremas ao valor calulado, pois o teste é bilateral

prop.test(576,1028,p=0.5,alternative="two.sided",correct = F,conf.level = 0.95) #aproximado

binom.test(576,1028,p=0.5,alternative="two.sided",conf.level = 0.95) #exato



# TH e IC para duas proporÃ§Ãµes - slide 11
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



# teste qui-quadrado de independÃªncia - slide 16
# nenhuma nova procura 3511, 1749, 1818
# nova procura 1489, 751, 682
dados=matrix(c(3511,1489,1749,751,1818,682),nrow=2)
chisq.test(dados) # X-squared = 6.1203, df = 2, p-value = 0.04688

## p-valor < 0.04688 < 5%, rejeitamos H0. Algoritimos não funcionam igualmente bem

# valor crÃ­tico
qchisq(0.05,df=2,lower.tail=FALSE) # 5.991465
## x-squared > valor crítico, rejeitamos H0


# anÃ¡lise de resÃ­duos
chisq.test(dados)$stdres
## resíduo ajustado 2.462894 > 1.96, teste 2 tem melhor performance


# Exercícios Aula 5
# Fazer exercícios do slide 17.
# Utilize os três exemplos feitos em aula como referência (para resolver e interpretar).

###########################################
## Exercício 1
n = 200 # 200 peças
# 25 peças defeituosas 
x = 25
phat = x/n
p0 = 0.9
SE = sqrt(p0*(1-p0)/n)
phat + c(-1, 1)*qnorm(0.975,lower.tail = T)*SE #IC
(phat-p0)/SE # z_calculado
qnorm(.975) # z_critico
2*pnorm((phat-p0)/SE,lower.tail=F) # p-valor < 0.0001, rejeitamos H0
# z calculado = 3.867 > z crítico = 1.96, rejeitamos H0
# p-valor é a soma das duas caudas extremas ao valor calulado, pois o teste é bilateral

prop.test(576,1028,p=0.5,alternative="two.sided",correct = F,conf.level = 0.95) #aproximado

binom.test(576,1028,p=0.5,alternative="two.sided",conf.level = 0.95) #exato

###########################################
## Exercício 2

n1 = 80 # total homem
x1 = 32 # apreciam a revista
n2 = 50 # total mulheres
x2 = 26 # apreciam a revista
phat1 = x1/n1
phat2 = x2/n2
SE = sqrt((phat1*(1-phat1)/n1)+(phat2*(1-phat2)/n2))
(phat1-phat2) + c(-1, +1)*qnorm(.975)*SE #IC [-0.29521683  0.05521683]. Inclui o 0, não rejeitamos H0
(phat1-phat2)/SE # z_calculado -1.342312
qnorm(.975) # z_critico 1.959964
# z-calculado < z-crítico, não rejeitamos H0
2*pnorm((phat1-phat2)/SE,lower.tail=T) # p-valor 0.1794948
# p-valor > 0.05, não rejeitamos H0

###########################################
## Exercício 3

# aprovam (obama, democratas, republicanos): 842, 736, 541
# desaprovam (obama, democratas, republicanos): 616, 646, 842
dados=matrix(c(842, 616, 736,646, 541, 842),nrow=2)
chisq.test(dados) # X-squared = 106.35, df = 2, p-value < 2.2e-16
## p-valor muito próximo de 0 < 5%, rejeitamos H0. Não há diferença na classificação dos três grupos.

# valor crÃ­tico
qchisq(0.05,df=2,lower.tail=FALSE) # 5.991465
## x-squared > valor crítico, rejeitamos H0

# anÃ¡lise de resÃ­duos
chisq.test(dados)$stdres
## Aprovação do Obama e Democratas e rejeição dos republicanos tem resíduos ajustados > 1.96



