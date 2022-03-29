# LABORATÓRIO 08

# Os dados foram coletados a partir das avaliações discentes de final de semestre de uma grande amostra
# de professores da Universidade do Texas em Austin. Além disso, seis estudantes avaliaram a aparência
# física dos professores.‡ O resultado é um banco de dados no qual cada linha contém diferentes disciplinas
# e cada coluna representa as variáveis sobre as disciplinas e os professores.

download.file("http://www.openintro.org/stat/data/evals.RData", destfile = "evals.RData")
load("evals.RData")

# Explorando os Dados

# Exercício 1 Esse estudo é observacional ou experimental? A pergunta de pesquisa original
# proposta no artigo é se a beleza influencia diretamente as avaliações das disciplinas. Levando
# em consideração o desenho da pesquisa, é possível responder a essa pergunta tal como ela está
# formulada? Se não, reformule a pergunta.

# Fazer conclusões causais baseadas em experimentos é geralmente razoável. 
# No entanto, fazer as mesmas conclusões causais com base em dados observacionais pode ser traiçoeiro e não é recomendado. 
# Assim, estudos observacionais são geralmente suficientes apenas para mostrar associações.



# Exercício 2 Descreva a distribuição da variável score. A distribuição é assimétrica? O que
# sua forma permite dizer sobre a maneira como os alunos avaliam as disciplinas? A forma
# corresponde ao que você esperava ver? Por quê, ou por que não?
hist(evals$score)


# Exercício 3 Com exceção da variável score, escolha duas outras variáveis e descreva sua
# relação utilizando as técnicas apropriadas (gráfico de dispersão, gráfico de caixas lado-a-lado,
# ou gráfico de mosaico).
plot(evals$bty_avg~evals$cls_perc_eval)
plot(evals$age~evals$rank)
mosaicplot(evals$rank~evals$gender)


# Regressão Linear Simples

# O fenômeno proposto pelo estudo é que professores com melhor aparência 
# são avaliados de maneira mais favorável.

plot(evals$score ~ evals$bty_avg)

# Exercício 4 Refaça o gráfico de dispersão, mas agora utilize a função jitter() no eixo y ou x.
# (Utilize o comando ?jitter para aprender mais a respeito.) O que estava errado no gráfico de
# dispersão inicial?

?jitter
plot(jitter(evals$score) ~ evals$bty_avg)
plot(evals$score ~ jitter(evals$bty_avg))


# Exercício 5 Vamos verificar se a tendência aparente no gráfico é algo além de variação natural.
# Ajuste um modelo linear denominado m_bty para predizer a avaliação média de um professor
# a partir da média da avaliação da beleza e adicione a linha ao gráfico utilizando o comando
# abline(m_bty). Escreva a equação do modelo linear e interprete a inclinação da reta. A média
# da avaliação da beleza é um preditor estatisticamente significante? Essa variável parecer ser
# um preditor com significância prática?

m_bty <- lm(score ~ bty_avg, data = evals)
summary(m_bty)
plot(evals$score ~ evals$bty_avg)
abline(m_bty)


# Exercício 6 Utilize gráficos de resíduos para avaliar se as condições para uma regressão utilizando
# mínimos quadrados são plausíveis. Utilize gráficos e comente cada uma deles (retome
# o Laboratório 7 para relembrar como criá-los).
plot(m_bty$residuals ~ evals$bty_avg) #variância constante?
abline(h = 0, lty = 3) # linearidade?
qqnorm(m_bty$residuals);qqline(m_bty$residuals) # resíduos normalmente distribuídos?



# Regressão Linear Múltipla

# relação entre uma dessas avaliações (dos 6 alunos) e a média da avaliação da beleza.

plot(evals$bty_avg ~ evals$bty_f1lower)
cor(evals$bty_avg, evals$bty_f1lower)

# relações entre todas as variáveis relativas à beleza

plot(evals[,13:19])

# colinearidade: é melhor utilizar a média das avaliações como o único representante


# Para verificar se a beleza ainda é um preditor significante da avaliação docente depois que consideramos
# o sexo do professor, podemos adicionar um termo para o sexo no modelo.

m_bty_gen <- lm(score ~ bty_avg + gender, data = evals)
summary(m_bty_gen)

# Exercício 7 Valores p e estimativas dos parâmetros só são confiáveis se as condições para a
# regressão são plausíveis. Verifique se as condições para esse modelo são plausíveis utilizando
# gráficos de diagnóstico.

par(mfrow=c(2,2));plot(m_bty_gen);layout(1) #Quadro de Graficos de Residuos

# Exercício 8 A variável bty_avg continua sendo um preditor significante de score? A adição
# da variável gender ao modelo alterou a estimativa do parâmetro de bty_avg?


# Podemos traçar as linhas correspondentes aos homens e às mulheres
multiLines(m_bty_gen)

# Exercício 9 Qual é a equação da linha correspondente aos homens? (Dica: Para os homens, a
# estimativa do parâmetro é multiplicada por 1.) Para dois professores que receberam a mesma
# avaliação de beleza, qual gênero tende a ter as avaliações mais altas?


# Exercício 10 Crie um novo modelo denominado m_bty_rank removendo a variável gender e
# adicionando a variável rank. Como o R maneja variáveis categoriais que tem mais de dois
# níveis? Perceba que a variável rank tem três níveis: horista (teaching), assistente (tenure track)
# e titular (tenured).

m_bty_rank <- lm(score ~ bty_avg + rank, data = evals)
summary(m_bty_rank)


# A Busca pelo Melhor Modelo

# Exercício 11 Qual variável você acha que teria o maior valor p neste modelo? Por quê? Dica:
# Pense em qual variável você esperaria não estar associada à avaliação docente.

m_full <- lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval
             + cls_students + cls_level + cls_profs + cls_credits + bty_avg
             + pic_outfit + pic_color, data = evals)
summary(m_full)


# ver exercícios 12-19

m_full2 <- lm(score ~ ethnicity + gender + language + age + cls_perc_eval
              + cls_credits + bty_avg + pic_color, data = evals)
summary(m_full2)
par(mfrow=c(2,2));plot(m_full2,which=c(1:4));layout(1) #Quadro de Graficos de Residuos

# Seleção automatica de variáveis
m_0<- lm(score ~ 1, data = evals)

m_f<-step(m_0,direction="forward",scope=list(lower=m_0, upper=m_full))
summary(m_f)

m_b<-step(m_full,direction="backward",scope=list(lower=m_0, upper=m_full))
summary(m_b)

m_s<-step(m_full,direction="both",scope=list(lower=m_0, upper=m_full))
summary(m_s)

m_f2<-olsrr::ols_step_forward_p(m_full,pent=0.05,prem=0.1,progress=T)
m_f2

m_b2<-olsrr::ols_step_backward_p(m_full,pent=0.05,prem=0.1,progress=T)
m_b2

m_s2<-olsrr::ols_step_both_p(m_full,pent=0.05,prem=0.1,progress=T)
m_s2

