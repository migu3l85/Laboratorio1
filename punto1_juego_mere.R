##########################################################
# Laboratorio 1 - Métodos Cuantitativos para el Análisis
# Punto 1: El Juego del Caballero de Méré
# Universidad del Valle
# Integrantes: Miguel Andrés Andrade Muñoz
# Fecha: Lunes 7 de abril 2025
##########################################################

# -------------------------
# Función 1: juego.simple.1
# Simula un juego de 1 dado lanzado n veces.
# El jugador gana si al menos un 6 aparece.
# -------------------------
juego.simple.1 <- function(n) {
  lanzamientos <- sample(1:6, n, replace = TRUE)
  any(lanzamientos == 6)
}

# -------------------------
# Función 2: juego.simple.2
# Simula un juego de 2 dados lanzados n veces.
# El jugador gana si al menos una vez sale (6,6)
# -------------------------
juego.simple.2 <- function(n) {
  dados <- replicate(n, sample(1:6, 2, replace = TRUE))
  any(dados[1, ] == 6 & dados[2, ] == 6)
}

# -------------------------
# Función 3: prob.ganar.1
# Estima la probabilidad de ganar en el Modo 1
# mediante I simulaciones.
# -------------------------
prob.ganar.1 <- function(I, n) {
  mean(replicate(I, juego.simple.1(n)))
}

# -------------------------
# Función 4: prob.ganar.2
# Estima la probabilidad de ganar en el Modo 2
# mediante I simulaciones.
# -------------------------
prob.ganar.2 <- function(I, n) {
  mean(replicate(I, juego.simple.2(n)))
}

# -------------------------
# Función 5: graf.mere
# Genera el gráfico de la probabilidad de ganar
# estimada por simulación vs fórmula de Pascal.
# -------------------------
graf.mere <- function(I, seq.n, modo = 1) {
  if (modo == 1) {
    p <- 1/6
    simuladas <- sapply(seq.n, function(n) prob.ganar.1(I, n))
  } else {
    p <- 1/36
    simuladas <- sapply(seq.n, function(n) prob.ganar.2(I, n))
  }
  
  pascal <- 1 - (1 - p)^seq.n
  
  plot(seq.n, simuladas, type = "b", pch = 19, col = "blue",
       ylab = "Probabilidad de ganar",
       xlab = "Número de lanzamientos (n)",
       main = paste("Juego del Caballero de Méré - Modo", modo))
  lines(seq.n, pascal, type = "b", col = "red", pch = 17)
  legend("bottomright", legend = c("Simulación", "Fórmula de Pascal"),
         col = c("blue", "red"), pch = c(19, 17))
}

# -------------------------
# Ejemplo de uso:
# -------------------------

# Modo 1: dado lanzado 1 vez hasta 30 veces
graf.mere(I = 5000, seq.n = 1:30, modo = 1)

# Modo 2: dos dados lanzados 1 a 50 veces
graf.mere(I = 5000, seq.n = 1:50, modo = 2)