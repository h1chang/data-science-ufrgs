# LABORATÓRIO 06

#"Independente de você frequentar algum culto religioso ou não, você diria que você 
#é uma pessoa religiosa, não é uma pessoa religiosa ou é um ateu convicto?"

# 2012 - Índice Global de Religiosidade e Ateísmo


# comunicado à imprensa da pesquisa de opinião, realizada pela WIN-Gallup International
# https://github.com/OpenIntroStat/oilabs/blob/master/data-raw/atheism/Global_INDEX_of_Religiosity_and_Atheism_PR__6.pdf


# Exercício 1 No primeiro parágrafo, vários resultados importantes são relatados. Essas porcentagens
# parecem ser estatísticas amostrais (derivadas dos dados da amostra) ou parâmetros populacionais?
# R. dados de pesquisa com 50 mil pessoal - são estatísticas amostrais


# Exercício 2 O título do relatório é "Índice Global de Religiosidade e Ateísmo" ("Global Index
# of Religiosity and Atheism"). Para generalizar os resultados do relatório para a população humana
# global, o que devemos assumir a respeito do método amostral? Parece ser uma suposição razoável?
# R. que a quantidade e  forma de seleção dessas pessoas tenha sido representatativa da população
# global. E parece que sim, pois engloba os 5 continentes, com um total de 50 mil respondentes.


# Os Dados
download.file("http://www.openintro.org/stat/data/atheism.RData", destfile = "atheism.RData")
load("atheism.RData")


# Exercício 3 A que corresponde cada linha da Tabela 6? 
# R. um país
# A que corresponde cada linha do banco de dados atheism (ateísmo)?
# R. uma pessoa em 2005 ou 2012.


#Exercício 4 Utilizando o comando abaixo, crie um novo banco de dados denominado us12 que
#contém apenas as linhas do banco de dados atheism associadas aos respondentes da pesquisa
#realizada em 2012 nos Estados Unidos. Em seguida, calcule a proporção de respostas dos que
#se afirmam ateus. Ela é semelhante à porcentagem da Tabela 6? Se não, por quê?
us12 <- subset(atheism, atheism$nationality == "United States" & atheism$year == "2012")
table(us12$response)
table(us12$response)/1002
# R. 4,99% (50/1002)


# Tabela 6 apresenta estatísticas, ou seja, cálculos feitos a partir da amostra de 51.927 pessoas.
# "Qual a proporção de pessoas na amostra que informaram serem ateus?"
# "Qual a proporção de pessoas na Terra que informariam serem ateus?"


# Exercício 5 Descreva as condições para inferência necessárias para construir um 
# intervalo de confiança de 95% para a proporção de ateus nos Estados Unido em 2012. 
# Você está confiante de que todas as condições são atendidas?
# R. Depende de autor, mas np > 5 ou 10 e n(1-p) > 5 ou 10
# R. e amostra aleatória de observações independente
# R. E está ok, pois np = 50 e n(1-p) = 952


#construir o intervalo de confiança para a proporção de ateus nos EUA utilizando a função inference
inference(y = us12$response, est = "proportion", type = "ci", method = "theoretical",
          success = "atheist")
# R. 95 % Confidence interval = ( 0.0364 , 0.0634 )
# R. EP = raiz (p(1-p)/n) = raiz (0,499(1-0,499)/1002) = 0.0069
# R. IC = 0.499 -+ 1.96 * 0.0069

# Exercício 6 Com base nos resultados do R, qual é a margem de erro para a 
# estimativa da proporção de ateus nos EUA em 2012?
# R. ME = Z * EP = 1.96 * 0.0069 = 0.0135


#Exercício 7 Utilizando a função inference, calcule os intervalos de confiança para a proporção
#de ateus em 2012 para dois outros países de sua escolha, e informe as margens de erro associadas
#a eles. Certifique-se de observar se as condições para inferência são atendidas. Pode ser
#útil primeiro criar novos conjuntos de dados para cada um dos dois países, e então usar essas
#conjuntos de dados junto com a função inference para construir os intervalos de confiança.
Brazil12 <- subset(atheism, atheism$nationality == "Brazil" & atheism$year == "2012")
table(Brazil12$response)
table(Brazil12$response)/2002
inference(y = Brazil12$response, est = "proportion", type = "ci", method = "theoretical",
          success = "atheist")
# R. Brasil: 95 % Confidence interval = ( 0.0056 , 0.0143 )
1.96*0.0022
0.0143-00999001
# ME = 0.00431


Italy12 <- subset(atheism, atheism$nationality == "Italy" & atheism$year == "2012")
table(Italy12$response)
table(Italy12$response)/987
inference(y = Italy12$response, est = "proportion", type = "ci", method = "theoretical",
          success = "atheist")
# R. 95 % Confidence interval = ( 0.0631 , 0.097 )
1.96*0.0086
0.08004053-0.0631
# ME = 0.0169


# ME depende da confinaça, quanto maior a confiança, maior será a ME
# ME dependendo tamanho da amostra, quanto maior o n, menor será ME
# mas também depende do valor de p... como??? p(1-p) no numerador
# gráfico relacionando ME com p
n <- 1000
p <- seq(0, 1, 0.01)
me <- 2*sqrt(p*(1 - p)/n)
plot(me ~ p)


# Exercício 8 Descreva a relação entre p e me.
# R. com p=0,5 temos a maior ME, que diminui com p se aproximando de 0 ou 1.




# np > 10 e n(1 - p) > 10
# simulação para investigar a relação entre n e p e a forma da distribuição amostral
p <- 0.1
n <- 1040
p_hats <- rep(0, 5000)
for(i in 1:5000){
  samp <- sample(c("atheist", "non_atheist"), n, replace = TRUE, prob = c(p, 1-p))
  p_hats[i] <- sum(samp == "atheist")/n
}
hist(p_hats, main = "p = 0.1, n = 1040", xlim = c(0, 0.18))


#Exercício 9 Descreva a distribuição amostral da proporção com n = 1040 e p = 0.1. Certifiquese
#de identificar seu centro, dispersão e forma.
#Dica: Lembre-se que o R tem funções como mean para calcular estatísticas descritivas.
mean(p_hats)
# R. 0.09969
sd(p_hats)
# R. 0.009287382
sqrt(p*(1 - p)/n) #fórumula do EP de p_chapéu
# R. 0.009302605


#Exercício 10 Repita a simulação acima mais três vezes mas com diferentes tamanhos de amostra
#e proporções: com n = 400 e p = 0.1, n = 1040 e p = 0.02, e n = 400 e p = 0.02.
#Crie histogramas para as quatro distribuições e exiba-os em conjunto utilizando o comando
#par(mfrow = c(2,2)). Talvez você precise expandir a janela do gráfico para acomodar o gráfico
#maior. Descreva as três distribuições amostrais novas. Com base nesses gráficos limitados,
#como que n parece afetar a distribuição de p^? Como que p afeta a distribuição amostral?

#n = 400 e p = 0.1
p2 <- 0.1
n2 <- 400
p_hats2 <- rep(0, 5000)
for(i in 1:5000){
  samp2 <- sample(c("atheist", "non_atheist"), n2, replace = TRUE, prob = c(p2, 1-p2))
  p_hats2[i] <- sum(samp2 == "atheist")/n2
}
hist(p_hats2, main = "p = 0.1, n = 400", xlim = c(0, 0.18))
mean(p_hats2)
sd(p_hats2)
sqrt(p2*(1 - p2)/n2)



#n = 1040 e p = 0.02
p3 <- 0.02
n3 <- 1040
p_hats3 <- rep(0, 5000)
for(i in 1:5000){
  samp3 <- sample(c("atheist", "non_atheist"), n3, replace = TRUE, prob = c(p3, 1-p3))
  p_hats3[i] <- sum(samp3 == "atheist")/n3
}
hist(p_hats3, main = "p = 0.02, n = 1040", xlim = c(0, 0.18))
mean(p_hats3)
sd(p_hats3)
sqrt(p3*(1 - p3)/n3)

#n = 400 e p = 0.02
p4 <- 0.02
n4 <- 400
p_hats4 <- rep(0, 5000)
for(i in 1:5000){
  samp4 <- sample(c("atheist", "non_atheist"), n4, replace = TRUE, prob = c(p4, 1-p4))
  p_hats4[i] <- sum(samp4 == "atheist")/n4
}
hist(p_hats4, main = "p = 0.02, n = 400", xlim = c(0, 0.18))
mean(p_hats4)
sd(p_hats4)
sqrt(p4*(1 - p4)/n4)

#gráficos juntos
par(mfrow = c(2,2))
hist(p_hats, main = "p = 0.1, n = 1040", xlim = c(0, 0.18))
hist(p_hats2, main = "p = 0.1, n = 400", xlim = c(0, 0.18))
hist(p_hats3, main = "p = 0.02, n = 1040", xlim = c(0, 0.18))
hist(p_hats4, main = "p = 0.02, n = 400", xlim = c(0, 0.18))
par(mfrow = c(1,1))

# R. n maior, menor EP e maior aproximação à normal
# R. n maior com p pequeno (np = 8), menor aproximação à normal


#Exercício 11 Se você retomar a Tabela 6, verá que a Austrália tem uma proporção amostral
#de 0,1 numa amostra de 1040, e que o Equador tem uma proporção amostral de 0,02 com
#400 sujeitos. Vamos supor, para esse exercício, que essas estimativas pontuais são verdadeiras.
#Dada a forma de suas respectivas distribuições amostrais, você acha razoável efetuar inferência
#e informar a margem de erros, como o relatório faz?

# R. Como temos np = 8 há algum risco na inferência para o Equador.
# mas a aproximação normal ainda parece satisfatória, por isso alguns autores sugerem np>5
par(mfrow = c(2,2))
qqnorm(p_hats)
qqline(p_hats)
qqnorm(p_hats2)
qqline(p_hats2)
qqnorm(p_hats3)
qqline(p_hats3)
qqnorm(p_hats4)
qqline(p_hats4)
par(mfrow = c(1,1))



#Sua Vez
