###############################################################################
# Capitulo 2 - Ejercicio 8c
###############################################################################

# Calculo de la probabilidad

set.seed(1)
sim <- NULL
for (i in 1:1000)
  sim[i] <- mean(rpois(30, 7))
mean(sim > 7.25)

# Grafico

library(ggplot2)

sims <- data.frame(i = 1:1000, sim = sim, res = as.numeric(sim > 7.25))
sims$prob <- cumsum(sims$res) / sims$i

ggplot(data = sims, aes(x = i, y = prob)) +
  geom_abline(intercept = 0.2995, slope = 0, size = 0.4, linetype = 2, colour = "black", alpha = 0.8) +
  geom_line(colour = "red", size = 1.2) +
  coord_cartesian(ylim = c(0, 1), xlim = c(1, 1000)) +
  scale_y_continuous(breaks = c(0, 0.2995, 1)) +
  labs(x = "Numero de muestras", y = "Probabilidad", title = "Probabilidad estimada por simulacion") +
  theme(text = element_text(size = 18))
