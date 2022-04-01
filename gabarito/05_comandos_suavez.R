# LABORATÓRIO 05 - Sua Vez


#1. Calcule o intervalo de confiança de 95% para a duração média das gravidezes (weeks) e o interprete
#no contexto do conjunto de dados. Perceba que, uma vez que você está realizando uma inferência
#sobre um único parâmetro populacional, não há nenhuma variáveis explanatória, e portanto você
#pode omitir a variável x da função.
inference(y = nc$weeks, est = "mean", type = "ci", null = 0,
          alternative = "twosided", method = "theoretical")
# R. 95 % Confidence interval = ( 38.1528 , 38.5165 )


#2. Calcule um novo intervalo de confiança para o mesmo parâmetro com nível de confiança de 90%.
#Você pode mudar o nível de confiança adicionando um novo argumento à função: conflevel =0.90.
inference(y = nc$weeks, est = "mean", type = "ci", null = 0,
          alternative = "twosided", method = "theoretical", conflevel =0.90)
# R. 90 % Confidence interval = ( 38.182 , 38.4873 )


#3. Realize um teste de hipótese para avaliar se a média do peso ganho pelas mães mais jovens é
#diferente da média de peso ganho pelas mães mais velhas.
inference(y = nc$gained, x = nc$mature, est = "mean", type = "ht", null = 0,
          alternative = "twosided", method = "theoretical")
# R. p-value =  0.1686


#4. Agora, um tarefa não-inferencial: determine o ponto de corte da idade das mães jovens e maduras.
#Utilize um método da sua escolha, e explique como seu método funciona.
by(nc$mage, nc$mature, min)
by(nc$mage, nc$mature, max)
# R. jovens: 13-34 e maduras: 35-50


#5. Escolha um par de variáveis, sendo uma numérica e outra categorial, e desenvolva uma pergunta de
#pesquisa para avaliar a relação entre essas variáveis. Formule a questão de maneira que ela possa ser
#respondida utilizando um teste de hipótese e/ou um intervalo de confiança. Responda a sua questão
#utilizando a função inference, informe os resultados estatísticos, e também elabora uma explicação
#em linguagem simples.

# a idade da mãe tem relação com o bebê ser prematuro?
inference(y = nc$mage, x = nc$premie, est = "mean", type = "ht", null = 0,
          alternative = "twosided", method = "theoretical")

# a idade da mãe tem relação com o bebê ter baixo peso?
inference(y = nc$mage, x = nc$lowbirthweight, est = "mean", type = "ht", null = 0,
          alternative = "twosided", method = "theoretical")

# o peso do bebê tem relação com a mãe ser mais madura?
inference(y = nc$weight, x = nc$mature, est = "mean", type = "ht", null = 0,
          alternative = "twosided", method = "theoretical")

# a duração da gestação tem relação com a mãe ser mais madura?
inference(y = nc$weeks, x = nc$mature, est = "mean", type = "ht", null = 0,
          alternative = "twosided", method = "theoretical")
