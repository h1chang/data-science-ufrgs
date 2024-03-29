# LABORATÓRIO 05

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

inference(y = nc$weight, x = nc$habit, est = "mean", type = "ht", null = 0, siglevel = 0.05,
          alternative = "twosided", method = "theoretical")
# R. p-valor = 0,0184


#Exercício 5 Mude o argumento type (tipo) para "ci" para construir e registrar um intervalo
#de confiança para a diferença entre os pesos dos bebê que nasceram de mães fumantes e não fumantes.
inference(y = nc$weight, x = nc$habit, est = "mean", type = "ci",  conflevel = 0.95,
          alternative = "twosided", method = "theoretical")
# R. (0,0534 ; 0,5777) que nos dá o IC para Mi_NF - Mi_F


#mudar a ordem da diferença no intervalo de confiança
inference(y = nc$weight, x = nc$habit, est = "mean", type = "ci", null = 0,
          alternative = "twosided", method = "theoretical",
          order = c("smoker","nonsmoker"))
# R. (-0,5777 ; -0,0534) que nos dá o IC para Mi_F - Mi_NF


#####################################################################
# Sua Vez
# Sua Vez - Laborat�rio 05


# 1. Calcule o intervalo de confian�a de 95% para a dura��o m�dia das gravidezes (weeks) e o interprete
# no contexto do conjunto de dados. Perceba que, uma vez que voc� est� realizando uma infer�ncia
# sobre um �nico par�metro populacional, n�o h� nenhuma vari�veis explanat�ria, e portanto voc�
# pode omitir a vari�vel x da fun��o.
inference(y = nc$weeks, est = "mean", type = "ci", null = 0,
          alternative = "twosided", method = "theoretical")
## R. 95 % Confidence interval = ( 38.1528 , 38.5165 )

# 2. Calcule um novo intervalo de confian�a para o mesmo par�metro com n�vel de confian�a de 90%.
# Voc� pode mudar o n�vel de confian�a adicionando um novo argumento � fun��o: conflevel =0.90.
inference(y = nc$weeks, est = "mean", type = "ci", null = 0,
          alternative = "twosided", method = "theoretical", conflevel = 0.9)

## R. 90 % Confidence interval = ( 38.182 , 38.4873 )

# 3. Realize um teste de hip�tese para avaliar se o a m�dia do peso ganho pelas m�es mais jovens �
# diferente da m�dia de peso ganho pelas m�es mais velhas.
nc$mature # indica: "younger mom" ou "mature mom"

inference(y= nc$gained, x = nc$mature, est = "mean", type = "ht", null = 0,
          alternative = "twosided", method = "theoretical")

## R. observada diferen�a entre as m�dias de "mature" e "younger" = -1.7697
## H0: mu_mature mom - mu_younger mom = 0 
## HA: mu_mature mom - mu_younger mom != 0 
## Standard error = 1.286 
## Test statistic: Z =  -1.376 
## p-value =  0.1686, aceitamos HA


# 4. Agora, um tarefa n�o-inferencial: determine o ponto de corte da idade das m�es jovens e maduras.
# Utilize um m�todo da sua escolha, e explique como seu m�todo funciona.

## m�es jovens
nc_younger <- subset(nc$mage, nc$mature == "younger mom")
summary(nc_younger) # Max. 34
## m�e jovem mais velha tem 34 anos

## m�es maduras
nc_mature <- subset(nc$mage, nc$mature == "mature mom")
summary(nc_mature) # Min. 35
## m�e adulta mais jovem tem 35 anos

## R. 35 anos � a idade de corte. Idade < 35 = jovem, Idade >= 35 = madura.

# 5. Escolha um par de vari�veis, sendo uma num�rica e outra categorial, e desenvolva um pergunta de
# pesquisa para avaliar a rela��o entre essas vari�veis. Formule a quest�o de maneira que ela possa ser
# respondida utilizando um teste de hip�tese e/ou um intervalo de confian�a. Responda a sua quest�o
# utilizando a fun��o inference, informe os resultados estat�sticos, e tamb�m elabora uma explica��o
# sem linguagem simples.

##R.
## num�rica: weight
## categ�rica: marital ("married" ou "not married") 
inference(y= nc$weight, x = nc$marital, est = "mean", type = "ht", null = 0,
          alternative = "twosided", method = "theoretical")

## R. Existe rela��o entre peso do beb� e estado civil dos pais?
## A m�dia de peso do beb� dos pais casados � 6.8
## A m�dia de peso do beb� dos pais n�o casados � 7.3
## Quando p-value = 0, aceitamos HA
## N�o existe rela��o entre estado civil e peso do beb�.

