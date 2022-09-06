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
#'library (dplyr)
#'library (readxl)
#'dados <- read_excel("C:/Users/petro/Dropbox/Estatística/Bioestatística usando o R/dadosPop.xlsx")
#'dados$pop <- factor(dados$pop)
#'
#'media <- aggregate(dados$altura, list(dados$pop), FUN=mean)
#'s <- aggregate(dados$altura, list(dados$pop), FUN=sd)
#'n <- dados %>% count(pop, sort = TRUE)
#'
#'s1 <- s$x [1]
#'s2 <- s$x [2]
#'n1 <- n$n [1]
#'n2 <- n$n [2]
#'
#'media1 <- media$x [1]
#'media2 <- media$x [2]
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

