# LABORATÓRIO 00 - SUA VEZ

source("http://www.openintro.org/stat/data/present.R")

#1. Quais anos estão incluídos neste conjunto de dados? 
#Quais são as dimensões da base de dados e quais são os nomes das colunas ou variáveis?
present
dim(present)
names(present)
# R. 1940 a 2002
# R. 63 casos e 3 variáveis
# R. "year"  "boys"  "girls"

#2. Como estas contagens se comparam aos dados de Arbuthnot? Eles estão numa escala similar?
# R. Os dados de Arbuthnot estão na ordem de milhares, e os do arquivo Present na ordem dos milhões

#3. A observação de Arbuthnot de que os meninos nascem numa proporção maior que as meninas se mantém nos EUA?
present$boys > present$girls
# R. Sim

#4. Crie um gráfico que mostre a razão de meninos para meninas para cada ano do conjunto de dados. 
#O que você pode verificar?
plot(x = present$year, y = present$boys / (present$boys + present$girls), type = "l")
# R. Proporção de menino maior, com ligeira queda ao longo dos anos.

#5. Em qual ano se verifica o maior número de nascimentos nos EUA? 
max(present$boys + present$girls)
present$year
present$boys + present$girls
plot(x = present$year, y = present$boys + present$girls, type = "l")
present$year[which.max(present$boys + present$girls)]
# R. 1961 com 4.269.326 nascimentos
