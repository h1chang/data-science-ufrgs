# LABORATÓRIO 03 - SUA VEZ

#1. Agora vamos analisar outras variáveis no conjunto de dados das dimensões corporais. Utilizando
#as figuras na próxima página, combine os histogramas com seus gráficos de probabilidade normal.
#Todas as variáveis foram estandardizadas (primeiro subtraindo a média, e em seguida dividindo pelo
#desvio padrão), de tal forma que as unidades não serão de qualquer ajuda. Se você estiver incerto
#com base nessas figuras, gere um gráfico no R para verificar.

#(a) O histograma do diâmetro bi-ilíaco (pélvico) feminino (bii.di) pertence ao gráfico de probabilidade normal de letra ___
hist(fdims$bii.di)
qqnorm(fdims$bii.di)
qqline(fdims$bii.di)
# R. B

#(b) O histograma do diâmetro do cotovelo feminino (elb.di) pertence ao gráfico de probabilidade normal de letra ___
hist(fdims$elb.di)
qqnorm(fdims$elb.di)
qqline(fdims$elb.di)
# R. C

#(c) O histograma de idade geral (age) pertence ao gráfico de probabilidade normal de letra ___
hist(fdims$age)
qqnorm(fdims$age)
qqline(fdims$age)
# R. D

#(d) O histograma de profundidade do peito feminino (che.de) pertence ao gráfico de probabilidade normal de letra ___
hist(fdims$che.de)
qqnorm(fdims$che.de)
qqline(fdims$che.de)
# R. A

#2. Perceba que os gráficos de probabilidade normal C e D tem um pequeno padrão passo a passo. Por que você acha que eles são assim?
# R. Age e Che.de são assimétricas positivas (assimétrica à direita)

#3. Como você pode ver, gráficos de probabilidade normal podem ser utilizados tanto para avaliar a
#normalidade quanto visualizar a assimetria. Crie um gráfico de probabilidade normal para o diâmetro
#do joelho feminino (kne.di). Baseado neste gráfico de probabilidade normal, você diria que
#essa variável é simétrica, assimétrica à direita ou assimétrica à esquerda? Utiliza um histograma para
#confirmar seu resultado.
qqnorm(fdims$kne.di)
qqline(fdims$kne.di)
hist(fdims$kne.di)
# R. assimétrica positiva (assimétrica à direita)
