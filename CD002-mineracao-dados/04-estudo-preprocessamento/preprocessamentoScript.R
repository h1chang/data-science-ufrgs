############################################################################
#
# Profa: Karin Becker
#
# Atividade Pratica: Laboratorio 02 - Preprocessando
#
# Conjunto de dados: Bike 
#
############################################################################

## Suponha que uma empresa disponibilize dados diários de aluguel de bicicleta, junto com informacoes sobre o clima, o horario, a estação, e a principal forma de atração (marketing) daqueles usuarios.
## Vamos usar regras de associação para gerar regras de associação que relacionem a utilizaçao das biciletas de acordo com a média de uso do horario.
## Neste estudo dorigido, primeiro aplicaremos as regras de associação a um dataset limpo. 
## Depois, mostraremos diferentes análises e acoes de preparação de dados para chegar neste dataset.

## Usando o dataset pre-processado e aplicando regras de associação.
## Leia o arquivo pre-processado, visualize seu conteudo.
## Veja a estrutura do dataframe, as estatisticas dos dados. 


library(readr)
bikeClean <- read.csv("bikeCleaned.csv",stringsAsFactors=TRUE)
View(bikeClean)
str(bikeClean)
summary(bikeClean)

#Q1: Informe o número de variáveis, seu tipo, e o número de observações.
#Q2: O que este conjunto de observações lhe permitiu de conhecimento sobre estes dados?

## Gere regras de associação a partir deste dataset.
## Ja praticamos tudo isto, então não vamos dar muita atençao aos resultados. 
## Geraremos regras que busquem explicar o uso acima/abaixo da média de acordo com as demais propriedades expressas no dataset.
## Eliminamos regras redundantes do resultado.

library(arules)
rules.all <- apriori(bikeClean, control = list(verbose=F),
                     parameter = list(minlen=2, maxlen=4, supp=0.05, conf=0.6),
                     appearance = list(rhs=c("naMedia=Yes","naMedia=No"), default="lhs"))
quality(rules.all) <- round(quality(rules.all), digits=3)
rules.sorted <- sort(rules.all, by="lift")
inspect(rules.sorted)

subset.matrix <- is.subset(rules.sorted, rules.sorted, sparse = FALSE)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

#Q3: Informe quantas regras encontrou, e apresente alguns achados, comentando 3 a 4 regras.

# Mas e se os dados brutos entregues fossem bem diferentes?
# Neste estudo dirigido, vamos exercitar todas as transformacoes que tivemos que fazer para chegar neste resultado.

# Ler dados brutos, entenda seu conteudo e estrutura do dataframe, e tire estatísticas
bike <- read.csv("raw_bikeshare_data.csv")
View(bike)
summary(bike)
str(bike)

#Q4: Você consegue responder quais são as estações do ano, quais condições climáticas são consideradas, quantos dias úteis ou feriados existem na amostra, e quais são as formas de propaganda existentes?  
#Q5: Compare a estrutura do dataframe dos dados brutos (bike), com a estrutura  do dataframe dos dados preprocessados (Q1). Quais são as principais diferenças? Explique por que você tem dificuldade de ter uma visão geral da distribuição dos dados.


# Encontrando e consertando problemas
# Valores nulos
# numero de valores nulos na tabela - nao interessa o atributo/registro
# em numero absoluto ou proporcoes
table(is.na(bike))
prop.table(table(is.na(bike))) * 100

#total de registros com valores nulos por atributo
colSums(is.na(bike)) 

#Q6: Qual ou quais atributos têm valores nulos? Quantos registros são afetados?

#substituindo os valores nulos (NA) por um valor padrao (desconhecido)
na_loc <- is.na(bike$sources)
bike$sources[na_loc] <- "desconhecido"
colSums(is.na(bike)) 

## padronizando os valores do atributo sources
## verificar os valores unicos 
unique(bike$sources)

#Q7: Quais problemas você nota nestes valores? Eles são realmente valores únicos?

#padronizando os valores: conversão para caixa baixa, remocao de espacos em branco
library(dplyr)
library(stringr)
bike$sources <- tolower(bike$sources)
bike$sources <- str_trim(bike$sources)
unique(bike$sources)

#abstraindo os diferentes sites para source "web"
library(DataCombine)
web_sites <- "(www.[a-z]*.[a-z]*)"
current <- unique(str_subset(bike$sources, web_sites))
replace <- rep("web", length(current))
replacements <- data.frame(from = current, to = replace)
bike <- FindReplace(data = bike, Var = "sources", replacements,
                    from = "from", to = "to", exact = FALSE)
unique(bike$sources)

str(bike)
summary(bike)

#Q8: Cite as duas formas de campanha mais eficazes em termos de prevalência diária. 
#Você conseguiu responder esta pergunta com o summary? Justifique sua resposta.

# transformando em dados categóricos
bike$sources <- as.factor(bike$sources)
str(bike)
summary(bike)



#transformando o tipo das demais variaveis (continuas, char)
# para valores categóricos, colocando nomes mais representativos
bike$humidity <- as.numeric(bike$humidity)

bike$holiday <- factor(bike$holiday, levels = c(0, 1),
                       labels = c("nao", "sim"))
bike$workingday <- factor(bike$workingday, levels = c(0, 1),
                          labels = c("nao", "sim"))

bike$season <- factor(bike$season, levels = c(1, 2, 3, 4),
                      labels = c("primavera", "verao",
                                 "outono", "inverno"),
                      ordered = TRUE )

bike$weather <- factor(bike$weather, levels = c(1, 2, 3, 4),
                       labels = c("claro",
                                  "nublado",
                                  "garoando",
                                  "chuvoso"),
                       ordered = TRUE )

str(bike)
summary(bike)

# plotando valores para escolher algumas variaveis relevantes
plot(bike$weather, main="Histograma - Distribuicao dos usuarios por tempo", 
     xlab="Tempo", ylab="Total usuarios",
     ylim=c(0,13000), las=0)
plot(bike$workingday, main="Histograma - Distribuicao dos usuarios por Dia Útil", 
     xlab="Dia Util", ylab="Total usuarios",
     ylim=c(0,13000), las=0)
plot(bike$holiday, main="Histograma - Distribuicao dos usuarios por Feriado", 
     xlab="Feriado", ylab="Total usuarios",
     ylim=c(0,17000), las=0)
plot(bike$season, main="Histograma - Distribuicao dos usuarios por Estação do Ano", 
     xlab="Feriado", ylab="Total usuarios",
     ylim=c(0,5000), las=0)

#Q9: Você acha que um ou mais dos 4 atributos plotados podiam ser descartados para geração de regras? 
#Cite qual/quais, e justifique sua resposta

#agregando os usos por horario
#tratando o atributo que representa um timestamp 

# Date and time conversions
library(lubridate)
library(ggplot2)
library(tidyr)


#separando datetime em dois atributos (data e hora)

bike$datetime <- mdy_hm(bike$datetime)
bike <- bike %>% 
  tidyr::separate(datetime, 
                  into = c("data", "hora"), 
                  sep = " ",
                  remove = FALSE)

bike$data <- as.Date(bike$data)
str(bike)

#agregando e calculando a mediana por data (independente do horario)
contagemd <- bike %>% 
  dplyr::select(data, count) %>% 
  group_by(data) %>% 
  summarise(med.count = median(count, na.rm = TRUE)) # mediana dos dados
contagemd

#plotando o resultado por data
ggplot(contagemd, aes(data, med.count)) +
  geom_line(col = "steelblue") +
  labs(x = "Data", 
       y = "Mediana de contagens", 
       title = "Rental Bike - San Francisco") +
  scale_x_date(limits = as.Date(c("2011-01-01","2012-12-31")))


#agregando e calculando a mediana por horario (independente do data)
contagemh <- bike %>% 
  dplyr::select(hora, count) %>% 
  group_by(hora) %>% 
  summarise(med.count = median(count, na.rm = TRUE)) # mediana dos dados
contagemh

#plotando o resultado dor hora
plot(contagemh$med.count, main="Histograma - Mediana usuarios por horario", 
     xlab="horario", ylab="Mediana usuarios")


#criando um atributo derivado TURNO
#selecionando o turno de acordo com o horario
bike1 <- mutate(bike, turno = ifelse(hora %in% c("20:00:00", "21:00:00", "22:00:00", "23:00:00", "24:00:00",
                                                 "00:00:00","01:00:00","02:00:00","03:00:00","04:00:00","05:00:00"), "noite", 
                                     ifelse(hora %in% c("06:00:00", "07:00:00", "08:00:00", "09:00:00",
                                                        "10:00:00", "11:00:00", "12:00:00"), "manha",
                                            "tarde")))
#convertendo em fator
bike1$turno <- as.factor(bike3$turno)
summary(bike1)

#criando um atributo derivado NAMEDIA
#para criar um atributo derivado naMedia, é necessário calcular as medianas por turno
#calculando a mediana por turno
contagemt <- bike1 %>% 
  dplyr::select(turno, count) %>% 
  group_by(turno) %>% 
  summarise(med.count = median(count, na.rm = TRUE)) # mediana dos dados

noite <- subset(bike1, turno=="noite")
medianaNoite <- median(noite$count)
manha <- subset(bike1, turno=="manha")
medianaManha <- median(manha$count)
tarde <- subset(bike1, turno=="tarde")
medianaTarde <- median(tarde$count)
medianaNoite
medianaManha
medianaTarde


#criando o atributo derivado naMedia, e comparando com a respectiva mediana
bike2 <- mutate(bike1, naMedia = NA)
bike2$naMedia[bike2$turno=="noite"] <- ifelse(bike2$count[bike2$turno=="noite"]<medianaNoite,"Yes","No")
bike2$naMedia[bike2$turno=="manha"] <- ifelse(bike2$count[bike2$turno=="manha"]<medianaManha,"Yes","No")
bike2$naMedia[bike2$turno=="tarde"] <- ifelse(bike2$count[bike2$turno=="tarde"]<medianaTarde,"Yes","No")
bike2$naMedia <- as.factor(bike2$naMedia)
summary(bike2)

#selecionando as variaveis finais e trocando o nome das variaveis
bikeClean <- select(bike2, turno, sources, weather, season, workingday,naMedia)

bikeClean <- bikeClean %>% rename(estacao = season,
                                  diaUtil = workingday,
                                  clima = weather, 
                                  fontes = sources)
str(bikeClean)

#salvando o arquivo limpo

write.csv(bikeClean, "bikeCleaned.csv",
          row.names = FALSE)

#Q10. Proponha uma variação qualquer para gerar uma variação do dataset limpo. 
