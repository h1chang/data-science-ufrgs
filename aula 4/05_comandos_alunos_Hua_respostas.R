# LABORATÃ“RIO 05

#Nascimentos na Carolina do Norte em 2004

download.file("http://www.openintro.org/stat/data/nc.RData", destfile = "nc.RData")
load("nc.RData")

#ExercÃ­cio 1 Quais sÃ£o os casos neste conjunto de dados? 
#HÃ¡ quantos casos em nossa amostra?
dim(nc)
# R. os casos sÃ£o nascimentos no estado da Carolina do Norte, em 2004
# R. HÃ¡ 1000 casos com 13 variÃ¡veis

# fage - idade do pai em anos.
# mage - idade da mÃ£e em anos.
# mature - maioridade da mÃ£e.
# weeks - duraÃ§Ã£o da gestaÃ§Ã£o em semanas.
# premie - se o nascimento Ã© classificado como prematuro ou a termo.
# visits - nÃºmero de visitas hospitalares durante a gravidez.
# marital - se a mÃ£e estava casada (married) ou solteira (not married) no momento do nascimento.
# gained - peso ganho pela mÃ£e durante a gravidez, em libras.
# weight - peso do bebÃª no nascimento, em libras.
# lowbirthweight - se o bebÃª foi classificado como tendo baixo peso ao nascer (low) ou nÃ£o (not low).
# gender - sexo do bebÃª, feminino (female) ou masculino (male).
# habit - se a mÃ£e Ã© nÃ£o-fumante (nonsmoker) ou fumante (smoker).
# whitemom - se a mÃ£e Ã© branca (white) ou nÃ£o-branca (not white).

#sumÃ¡rio dos dados de todas as variÃ¡veis do banco de dados
summary(nc)

#VariÃ¡veis sÃ£o categÃ³ricas ou NumÃ©rica? PresenÃ§a de outlier?

#grÃ¡fico para verificar presenÃ§a de outlier - semanas de gestaÃ§Ã£o
boxplot(nc$weeks)


#ExercÃ­cio 2 Crie um grÃ¡fico de caixas lado-a-lado das variÃ¡veis habit (hÃ¡bito) e weight (peso).
#O que o grÃ¡fico revela sobre a relaÃ§Ã£o entre essas duas variÃ¡veis?
boxplot(nc$weight ~ nc$habit)
# R. Parece que mÃ£es fumantes tÃªm bebÃªs com menor peso.

#dividir a variÃ¡vel weight nos grupos da variÃ¡vel habit, e calcular a mÃ©dia
by(nc$weight, nc$habit, mean)
# HÃ¡ uma diferenÃ§a evidente, mas essa diferenÃ§a Ã© estatisticamente significante? 
# Para responder a essa questÃ£o, vamos realizar um teste de hipÃ³tese.


#ExercÃ­cio 3 Verifique se as condiÃ§Ãµes necessÃ¡rias para realizar a inferÃªncia sÃ£o atendidas.
#Perceba que vocÃª precisarÃ¡ obter o tamanho das amostras para verificar as condiÃ§Ãµes. VocÃª
#pode calcular o tamanho dos grupos utilizando o mesmo comando by utilizado acima, mas
#substituindo a funÃ§Ã£o mean pela funÃ§Ã£o length.
by(nc$weight, nc$habit, length)
# R. Sim, pois trata-se de amostras grandes, com 873 nÃ£o fumantes e 126 fumantes


# ExercÃ­cio 4 Escreva as hipÃ³teses para testar se a mÃ©dia dos pesos dos bebÃªs que 
# nasceram de mÃ£es fumantes Ã© diferente daqueles que nasceram de mÃ£es nÃ£o fumantes.
# R. H0: Mi_NF = Mi_F
#    H1: Mi_NF <> Mi_F

# utilizaremos uma nova funÃ§Ã£o, inference, que serÃ¡ utilizada para realizar 
# os testes de hipÃ³tese e construir os intervalos de confianÃ§a.
View(inference)

inference(y = nc$weight, x = nc$habit, est = "mean", type = "ht", null = 0, siglevel = 0.05,
          alternative = "twosided", method = "theoretical")
# R. p-valor = 0,0184


#ExercÃ­cio 5 Mude o argumento type (tipo) para "ci" para construir e registrar um intervalo
#de confianÃ§a para a diferenÃ§a entre os pesos dos bebÃª que nasceram de mÃ£es fumantes e nÃ£o fumantes.
inference(y = nc$weight, x = nc$habit, est = "mean", type = "ci",  conflevel = 0.95,
          alternative = "twosided", method = "theoretical")
# R. (0,0534 ; 0,5777) que nos dÃ¡ o IC para Mi_NF - Mi_F


#mudar a ordem da diferenÃ§a no intervalo de confianÃ§a
inference(y = nc$weight, x = nc$habit, est = "mean", type = "ci", null = 0,
          alternative = "twosided", method = "theoretical",
          order = c("smoker","nonsmoker"))
# R. (-0,5777 ; -0,0534) que nos dÃ¡ o IC para Mi_F - Mi_NF


#####################################################################
# Sua Vez
# Sua Vez - Laboratório 05


# 1. Calcule o intervalo de confiança de 95% para a duração média das gravidezes (weeks) e o interprete
# no contexto do conjunto de dados. Perceba que, uma vez que você está realizando uma inferência
# sobre um único parâmetro populacional, não há nenhuma variáveis explanatória, e portanto você
# pode omitir a variável x da função.



# 2. Calcule um novo intervalo de confiança para o mesmo parâmetro com nível de confiança de 90%.
# Você pode mudar o nível de confiança adicionando um novo argumento à função: conflevel =0.90.



# 3. Realize um teste de hipótese para avaliar se o a média do peso ganho pelas mães mais jovens é
# diferente da média de peso ganho pelas mães mais velhas.

# 4. Agora, um tarefa não-inferencial: determine o ponto de corte da idade das mães jovens e maduras.
# Utilize um método da sua escolha, e explique como seu método funciona.

# 5. Escolha um par de variáveis, sendo uma numérica e outra categorial, e desenvolva um pergunta de
# pesquisa para avaliar a relação entre essas variáveis. Formule a questão de maneira que ela possa ser
# respondida utilizando um teste de hipótese e/ou um intervalo de confiança. Responda a sua questão
# utilizando a função inference, informe os resultados estatísticos, e também elabora uma explicação
# sem linguagem simples.


