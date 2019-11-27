win.graph()
set.seed(123)
ejemplo <- rnorm(n = 10000, mean = 0, sd = 1)
hist(ejemplo, col='orange', breaks=40, ylab = "Frecuencia", main = "Histograma ejemplo")
win.graph()
tiempo <- c(1:10)
respuesta <- c(1:4, seq(10, 20, 2))  

plot(tiempo,respuesta,
     type='b',
     pch=19,
     col="black",
     main = "Respuesta vs tiempo",
     ylab = "Respuesta",
     xlab = "Tiempo")
