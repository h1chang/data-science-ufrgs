# exercício - slide 7
X<-c(340,230,405,325,280,195,265,300,350,410)
Y<-c(71,65,83,74,67,56,57,78,84,65)
plot(X,Y)
cor(X,Y)


# exercício - slide 13
X<-c(1,2,3,4,5,6,7,8,9,10,11,12)
Y<-c(10.7,10.9,10.8,9.3,9.5,10.4,9.0,9.3,7.6,7.6,7.9,7.7)
plot(X,Y)
cor(X,Y) # negativo - linha decresente
m1 <- lm(Y ~ X)
summary(m1)
# Intercepto = b0, x = b1 (coeficiente angular)
#cor ^2 = Multiple R-squared, nesse caso onde �nico X
abline(m1)
plot(m1,1)
plot(m1,2)
shapiro.test(residuals.lm(m1))


# exemplo academia - slide 17
library(readxl)
dados_academia <- read_excel("dados_academia.xlsx")

mean(dados_academia$Y)

# regressão só com a média
m0 <- lm(dados_academia$Y ~ 1)
summary(m0)
sum(m0$residuals^2)

# matriz de correlação para escolher a primeira variável
library(DataExplorer)
plot_correlation(dados_academia)

# regressão simples com X2
m1 <- lm(dados_academia$Y ~ dados_academia$X2)
summary(m1)
sum(m1$residuals^2)
plot(dados_academia$X2,dados_academia$Y)
abline(m1)

# regressão simples com X2 e X1
m2 <- lm(dados_academia$Y ~ dados_academia$X1+dados_academia$X2)
summary(m2)
sum(m2$residuals^2)

# regressão simples com X2 e X1 e X3
m3 <- lm(dados_academia$Y ~ dados_academia$X1+dados_academia$X2+dados_academia$X3)
summary(m3)
sum(m3$residuals^2)

library(lm.beta)
summary(lm.beta(m3))

plot(m3,1)
plot(m3,2)


# EXEMPLO Mário Kart
library(openintro)
data(mariokart)

#{id}{Auction ID assigned by Ebay.}
#{duration}{Auction length, in days.}
#{n_bids}{Number of bids.}
#{cond}{Game condition, either \code{new} or \code{used}.}
#{start_pr}{Start price of the auction.}
#{ship_pr}{Shipping price.}
#{total_pr}{Total price, which equals the auction price plus the shipping price.}
#{ship_sp}{Shipping speed or method.}
#{seller_rate}{The seller's rating on Ebay. This is the number of positive ratings minus the number of negative ratings for the seller.}
#{stock_photo}{Whether the auction feature photo was a stock photo or not. If the picture was used in many auctions, then it was called a stock photo.}
#{wheels}{Number of Wii wheels included in the auction. These are steering wheel attachments to make it seem as though you are actually driving in the game. When used with the controller, turning the wheel actually causes the character on screen to turn.}
#{title}{The title of the auctions.}

library(ggplot2)
library(broom)
library(dplyr)
library(GGally) # devtools::install_github("ggobi/ggally")
library(car)

# estrutura do dataset
str(mariokart)
# ver primeiras linhas e variáveis
head(mariokart)
names(mariokart)

# identificar outliers
boxplot(mariokart$total_pr)
ggplot(mariokart, aes(x=total_pr,y=cond)) +
  geom_boxplot()

# tirando outliers
mariokart %>%
  filter(total_pr < 100) %>%
  ggplot(aes(x=total_pr,y=cond)) +
  geom_boxplot()

mariokart_no <- mariokart %>% filter(total_pr < 80)


# entendendo as variáveis
mariokart_no %>% 
  select(total_pr,duration,n_bids,cond,start_pr,ship_pr,ship_sp,seller_rate,stock_photo,wheels) %>% 
  ggpairs()

# gráfico, correlação e regressão entre preço de venda e número de rodas
plot(mariokart_no$wheels,mariokart_no$total_pr)
cor(mariokart_no$wheels,mariokart_no$total_pr)
m1<-lm(total_pr~wheels,data=mariokart_no)
summary(m1)
abline(m1)

# gráfico, correlação e regressão entre preço de venda e dias no eBay
plot(mariokart_no$duration,mariokart_no$total_pr)
cor(mariokart_no$duration,mariokart_no$total_pr)
m2<-lm(total_pr~duration,data=mariokart_no)
summary(m2)
abline(m2)

# gráfico, correlação e regressão entre preço de venda e preço inicial
plot(mariokart_no$start_pr,mariokart_no$total_pr)
cor(mariokart_no$start_pr,mariokart_no$total_pr)
m3<-lm(total_pr~start_pr,data=mariokart_no)
summary(m3)
abline(m3)


# ajustando regressão múltipla
mm<-lm(total_pr~wheels+duration+start_pr,data=mariokart_no)
summary(mm)


# incluindo variáveis categórica
# mariokart$cond <- ifelse(mariokart$cond == 'used', 0, 1)
# mariokart$stock_photo <- ifelse(mariokart$stock_photo == 'yes', 1, 0)
mmc<-lm(total_pr~cond+wheels,data =mariokart_no)
summary(mmc)

model_plot = mariokart_no %>% 
  data_grid(cond, wheels) %>% 
  add_predictions(mmc)
model_plot %>% 
  ggplot(aes(x  = wheels, y = pred, colour = cond)) + 
  geom_line() + 
  geom_point(data = mariokart_no, aes(y = total_pr)) + 
  facet_grid(. ~ cond)


#Modelo completo e Stepwise
modelo.completo<-lm(total_pr~duration+n_bids+cond+start_pr+ship_pr+ship_sp+seller_rate+stock_photo+wheels,data=mariokart_no)
stepwise <-step(modelo.completo,direction="both")
summary(stepwise)

# variáveis que ficaram no modelo
mariokart_no %>% 
  select(total_pr,n_bids,cond,start_pr,seller_rate,wheels) %>% 
  ggpairs()

# FIV (VIF) deve ser menor que 10
vif(stepwise)

# diagnóstico do modelo
plot(stepwise,2)
plot(stepwise,3)

aug_m3 <- augment(stepwise)
ggplot(aug_m3, aes(x = 1:nrow(aug_m3), y = .resid)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Ordem de coleta dos dados", y = "Resíduos")
ggplot(aug_m3, aes(x = cond, y = .resid)) +
    geom_boxplot() +
    labs(x = "Condition", y = "Resíduos")
ggplot(aug_m3, aes(x = n_bids, y = .resid)) +
  geom_point() +
  labs(x = "n_bids", y = "Resíduos")