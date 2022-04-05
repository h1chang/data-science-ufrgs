# Exemplos Distribuição Normal - Aula 2

# Sabrina
pnorm(1630, mean = 1500, sd = 300, lower.tail = F)
(1630-1500)/300
pnorm(0.43, mean = 0, sd = 1, lower.tail = F)


# Edward
pnorm(1400, mean = 1500, sd = 300, lower.tail = T)
(1400-1500)/300
pnorm(-0.33, mean = 0, sd = 1, lower.tail = T)



# Stuart
pnorm(2100, mean = 1500, sd = 300, lower.tail = T)
(2100-1500)/300
pnorm(2, mean = 0, sd = 1, lower.tail = T)

install.packages("ggplot2")
library(ggplot2)

set.seed(1)
m <- 1500
s <- 300
X <- m + s * seq(-4, 4, 0.01)
Y <- dnorm(X, m, s)
gg   <- data.frame(X,Y)

ggplot(data = gg, mapping = aes(x = X, y = Y)) + 
  geom_linerange(data = gg[gg$X < 2100,], aes(X, ymin = 0, ymax = Y), colour="blue") + 
  geom_path(size = 1) +
  labs(x = NULL, y = NULL) + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.line.y = element_blank()) + 
  geom_vline(xintercept = m - 100, linetype = "dashed") + 
  geom_hline(yintercept = 0) + 
  scale_x_continuous(breaks = seq(m - 3*s, m + 3*s, s)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))



# Mike
pnorm(170, mean = 178, sd = 8.38, lower.tail = T)
(170-178)/8.38
pnorm(-0.9546, mean = 0, sd = 1, lower.tail = T)

set.seed(1)
m <- 178
s <- 8.38
X <- m + s * seq(-4, 4, 0.01)
Y <- dnorm(X, m, s)
gg   <- data.frame(X,Y)

ggplot(data = gg, mapping = aes(x = X, y = Y)) + 
  geom_linerange(data = gg[gg$X < 170,], aes(X, ymin = 0, ymax = Y), colour="blue") + 
  geom_path(size = 1) +
  labs(x = NULL, y = NULL) + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.line.y = element_blank()) + 
  geom_vline(xintercept = 170, linetype = "dashed") + 
  geom_hline(yintercept = 0) + 
  scale_x_continuous(breaks = seq(m - 3*s, m + 3*s, s*1.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))


# Jim
pnorm(193, mean = 178, sd = 8.38, lower.tail = T)
(193-178)/8.38
pnorm(1.79, mean = 0, sd = 1, lower.tail = T)


ggplot(data = gg, mapping = aes(x = X, y = Y)) + 
  geom_linerange(data = gg[gg$X < 193,], aes(X, ymin = 0, ymax = Y), colour="blue") + 
  geom_path(size = 1) +
  labs(x = NULL, y = NULL) + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.line.y = element_blank()) + 
  geom_vline(xintercept = 193, linetype = "dashed") + 
  geom_hline(yintercept = 0) + 
  scale_x_continuous(breaks = seq(m - 3*s, m + 3*s, s*1.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))



# Erik
qnorm(0.40, mean = 178, sd = 8.38, lower.tail = T)
(175.88-178)/8.38
pnorm(-0.253, mean = 0, sd = 1, lower.tail = T)

ggplot(data = gg, mapping = aes(x = X, y = Y)) + 
  geom_linerange(data = gg[gg$X < qnorm(p = 0.40, mean = m, sd = s),], 
                 aes(X, ymin = 0, ymax = Y), colour="#E6205F") + 
  geom_path(size = 1) +
  labs(x = NULL, y = NULL) + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.line.y = element_blank()) + 
  geom_vline(xintercept = qnorm(p = 0.40, mean = m, sd = s), 
             linetype = "dashed") + 
  geom_hline(yintercept = 0) + 
  annotate("text", x = m - 2*s, 
           y = Y[which(grepl(m - s, X))], label = "0.40 (40%)") + 
  scale_x_continuous(breaks = seq(m - 3*s, m + 3*s, s*1.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

# Qual é a probabilidade de um homem adulto aleatório estar entre 175cm e 188cm
pnorm(188, mean = 178, sd = 8.38, lower.tail = T)-pnorm(175, mean = 178, sd = 8.38, lower.tail = T)

ggplot(data = gg, mapping = aes(x = X, y = Y)) + 
  geom_linerange(data = gg[gg$X > 175 & gg$X < 188, ], 
                 aes(X, ymin = 0, ymax = Y), colour="#E97C31") + 
  labs(x = NULL, y = NULL) + 
  geom_path(size = 1) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.line.y = element_blank()) + 
  geom_vline(xintercept = c(175, 188), 
             linetype = "dashed") + 
  geom_hline(yintercept = 0) + 
  scale_x_continuous(breaks = seq(m - 3*s, m + 3*s, s*1.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
