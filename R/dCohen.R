#'Função d de Cohen
#'
#'Calcula o d de Cohen, medida usada para ponderar a magnitude do efeito
#'na comparação entre duas médias
#'
#'@author Petronio Fagundes de Oliveira Filho
#'
#'@param n1 é tamanho do grupo 1
#'@param n2 é tamanho do grupo 2
#'@param media1 é a média do grupo 1
#'@param media2 é a média do grupo 2
#'@param s1 é desvio padrão do grupo 1
#'@param s2 é desvio padrão do grupo 2
#'
#'@return O valor do d de Cohen e uma interpretação da sua magnitude de acordo
#'com Cohen (1988)
#'
#'@examples
#'
#'altura1 <- c(1.50, 1.56, 1.63, 1.66, 1.60, 1.65, 1.49, 1.60, 1.56, 1.58,
#'             1.55, 1.66, 1.60, 1.56, 1.72, 1.65, 1.65, 1.57, 1.54, 1.55,
#'             1.65, 1.73, 1.46, 1.59, 1.62, 1.65, 1.62, 1.60, 1.62, 1.65)
#'altura2 <- c(1.44, 1.39, 1.54, 1.36, 1.32, 1.39, 1.37, 1.40, 1.44, 1.44,
#'             1.42, 1.35, 1.39, 1.34, 1.43, 1.34, 1.32, 1.61, 1.34, 1.30,
#'             1.37, 1.24, 1.36, 1.44, 1.38, 1.49, 1.41, 1.29, 1.38, 1.42)
#'
#'n1 <- length(altura1)
#'n2 <- length (altura2)
#'media1 <- mean(altura1)
#'media2 <- mean(altura2)
#'s1 <- sd(altura1)
#'s2 <- sd(altura2)
#'
#'dCohen(n1, n2, media1, media2, s1, s2)
#'
#'@export
dCohen <- function (n1, n2, media1, media2, s1, s2) {
sc <- sqrt((((n1 - 1) * s1^2) + ((n2 - 1) * s2^2))/(n1 + n2 - 2))
d <- round(((media1 - media2)/sc), 1)
print(d)
if (d < 0.2) categ <- 'Insignificante'
if ((d >= 0.2) & (d < 0.5)) categ <-  'Pequeno'
if ((d >= 0.5) & (d < 0.8)) categ <- 'Médio'
if (d >= 0.8) categ <-  'Grande'
return (categ)
}

