
# exemplo - slide 9
X<-c(rep(5,40),rep(10,60),rep(15,40),rep(25,40),rep(50,20))
Y<-c(rep(0,28),rep(1,12),rep(0,31),rep(1,29),rep(0,18),rep(1,22),rep(0,15),rep(1,25),rep(0,5),rep(1,15))
dados<-data.frame(X,Y)
m1 <- glm(Y~X,family=binomial,data=dados)
summary(m1)
formatC(exp(cbind(OR = coef(m1))),digits = 4) #Calcula a razao de chances
dados$predito<-predict(m1,type='response',dados)
# Qual o valor do cupom de desconto que induziria 50% do público alvo a adquirir o produto?
# R. 15%
anova(m1, test = "Chisq")
library(modEvA)
RsqGLM(m1)


# exemplo - slide 11
RA<-c(rep("B",53),rep("B",414),rep("B",16),rep("N",11),rep("N",37),rep("N",4),rep("N",139))
RV<-c(rep("B",53),rep("B",414),rep("N",16),rep("B",11),rep("B",37),rep("N",4),rep("N",139))
Y<-c(rep(1,53),rep(0,414),rep(0,16),rep(1,11),rep(0,37),rep(1,4),rep(0,139))
dados<-data.frame(RA,RV,Y)
m1 <- glm(Y~RA+RV,family=binomial,data=dados)
summary(m1)
formatC(exp(cbind(OR = coef(m1))),digits = 4) #Calcula a razao de chances
dados$predito<-predict(m1,type='response',dados)
anova(m1, test = "Chisq")
require(modEvA)
RsqGLM(m1)

# criando as dummies
dados$RAN <- ifelse(dados$RA == 'N', 1, 0)
dados$RVB <- ifelse(dados$RV == 'B', 1, 0)
m1 <- glm(Y~RAN+RVB,family=binomial,data=dados)
summary(m1)
formatC(exp(cbind(OR = coef(m1))),digits = 4) #Calcula a razao de chances
dados$predito<-predict(m1,type='response',dados)




# exemplo - slide 13
# Dados e-mail https://www.openintro.org/data/index.php?data=email
library(openintro)
data("email")
names(email)

#DESCRITIVAS
library(Hmisc)
Hmisc::describe(email)

library(dlookr)
dlookr::describe(email)

library(GGally)
ggpairs(email[, c(1:11)])
ggpairs(email[, c(1,12:21)])

library(DataExplorer)
introduce(email)
plot_intro(email)
plot_histogram(email)
plot_boxplot(email,by="spam")
plot_bar(email,by="spam")
plot_correlation(email,type = "c")
plot_correlation(email,type = "d")

library(skimr)
skim(email)



#Regressao Logistica - modelo completo
modelo.completo <- glm(spam ~.,family=binomial,data=email)
summary(modelo.completo)

#Stepwise para selecao de variaveis
stepwise <- step(modelo.completo,direction="both")
stepwise$formula

#Modelo com as variaveis indicadas pelo Stepwise
stepwise <- glm(stepwise$formula, family=binomial,data=email)

#Resume os resultados do modelo
summary(stepwise)

#Calcula a razao de chances
formatC(exp(cbind(OR = coef(stepwise), confint(stepwise))),digits = 4)

# FIV (VIF) deve ser menor que 10
library(scorecard)
scorecard::vif(stepwise)


# retirando a variável num_char
md_f <- glm(spam ~ to_multiple + from + sent_email + time + image + attach + 
              dollar + winner + inherit + viagra + password + line_breaks + 
              format + re_subj + urgent_subj + exclaim_mess + number, 
              family = binomial,data = email)
summary(md_f)
anova(md_f, test = "Chisq")
require(modEvA)
RsqGLM(md_f)

# FIV (VIF) deve ser menor que 10
library(scorecard)
scorecard::vif(md_f)

#área sob a curva ROC
library(ROCR)
library(pROC)

email$predito<-predict(md_f,type='response',email)

#GRAFICO DA ROC com valor
rocplot <- function(pred, truth, ...) {
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf, ...)
  area <- auc(truth, pred)
  area <- format(round(area, 4), nsmall = 4)
  text(x=0.8, y=0.1, labels = paste("AUC =", area))
  # linha de referencia
  segments(x0=0, y0=0, x1=1, y1=1, col="gray", lty=2)
}
rocplot(email$predito, email$spam, col="blue",main="ROC")

#Teste KS
library(creditR)
Kolmogorov.Smirnov(email,"spam","predito")

#Prediz quem vai ser "1" (SPAM) e quem vai ser "0" (Ñ SPAM)
email$predito2<-ifelse(email$predito>=0.4,1,0)

#Taxa de acerto 
tab_email<-table(email$spam,email$predito2)
tab_email
taxaacerto_email<-sum(diag(tab_email))/sum(tab_email)
taxaacerto_email #acerto total
precisao<-diag(tab_email)/colSums(tab_email)
precisao  #acerto por grupo
