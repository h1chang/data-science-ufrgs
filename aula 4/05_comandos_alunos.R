#Nascimentos na Carolina do Norte em 2004

download.file("http://www.openintro.org/stat/data/nc.RData", destfile = "nc.RData")
load("nc.RData")

#Exercício 1 Quais são os casos neste conjunto de dados? 
#Há quantos casos em nossa amostra?
dim(nc)
# R. os casos são nascimentos no estado da Carolina do Norte, em 2004
# R. Há 1000 casos com 13 variáveis

# fage - idade do pai em anos.
# mage - idade da mãe em anos.
# mature - maioridade da mãe.
# weeks - duração da gestação em semanas.
# premie - se o nascimento é classificado como prematuro ou a termo.
# visits - número de visitas hospitalares durante a gravidez.
# marital - se a mãe estava casada (married) ou solteira (not married) no momento do nascimento.
# gained - peso ganho pela mãe durante a gravidez, em libras.
# weight - peso do bebê no nascimento, em libras.
# lowbirthweight - se o bebê foi classificado como tendo baixo peso ao nascer (low) ou não (not low).
# gender - sexo do bebê, feminino (female) ou masculino (male).
# habit - se a mãe é não-fumante (nonsmoker) ou fumante (smoker).
# whitemom - se a mãe é branca (white) ou não-branca (not white).

#sumário dos dados de todas as variáveis do banco de dados
summary(nc)

#Variáveis são categóricas ou Numérica? Presença de outlier?

#gráfico para verificar presença de outlier - semanas de gestação
boxplot(nc$weeks)


#Exercício 2 Crie um gráfico de caixas lado-a-lado das variáveis habit (hábito) e weight (peso).
#O que o gráfico revela sobre a relação entre essas duas variáveis?
boxplot(nc$weight ~ nc$habit)
# R. Parece que mães fumantes têm bebês com menor peso.

#dividir a variável weight nos grupos da variável habit, e calcular a média
by(nc$weight, nc$habit, mean)
# Há uma diferença evidente, mas essa diferença é estatisticamente significante? 
# Para responder a essa questão, vamos realizar um teste de hipótese.


#Exercício 3 Verifique se as condições necessárias para realizar a inferência são atendidas.
#Perceba que você precisará obter o tamanho das amostras para verificar as condições. Você
#pode calcular o tamanho dos grupos utilizando o mesmo comando by utilizado acima, mas
#substituindo a função mean pela função length.
by(nc$weight, nc$habit, length)
# R. Sim, pois trata-se de amostras grandes, com 873 não fumantes e 126 fumantes


# Exercício 4 Escreva as hipóteses para testar se a média dos pesos dos bebês que 
# nasceram de mães fumantes é diferente daqueles que nasceram de mães não fumantes.
# R. H0: Mi_NF = Mi_F
#    H1: Mi_NF <> Mi_F

# utilizaremos uma nova função, inference, que será utilizada para realizar 
# os testes de hipótese e construir os intervalos de confiança.
View(inference)

inference(y = nc$weight, x = nc$habit, est = "mean", type = "ht", null = 0,
          alternative = "twosided", method = "theoretical")
# R. p-valor = 0,0184


#Exercício 5 Mude o argumento type (tipo) para "ci" para construir e registrar um intervalo
#de confiança para a diferença entre os pesos dos bebê que nasceram de mães fumantes e não fumantes.
inference(y = nc$weight, x = nc$habit, est = "mean", type = "ci", null = 0,
          alternative = "twosided", method = "theoretical")
# R. (0,0534 ; 0,5777) que nos dá o IC para Mi_NF - Mi_F


#mudar a ordem da diferença no intervalo de confiança
inference(y = nc$weight, x = nc$habit, est = "mean", type = "ci", null = 0,
          alternative = "twosided", method = "theoretical",
          order = c("smoker","nonsmoker"))
# R. (-0,5777 ; -0,0534) que nos dá o IC para Mi_F - Mi_NF



#Sua Vez


