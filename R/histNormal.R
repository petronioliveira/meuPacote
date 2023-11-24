#' Função curva normal sobreposta ao histograma
#'
#' Esta função constroi um histograma e coloca uma curva normal sobreposta
#'
#' @author Petronio Fagundes de Oliveira Filho
#'
#' @param dados é um dataframe com os dados
#' @param x é uma variável numérica base para o histograma
#' @param xlab é o rotulo do eixo x
#' @param ylab é o rótulo do eixo y
#'
#' @examples
#' pesoRN <- c(3340, 3345, 3750, 3650, 3220, 4070, 3380, 3970,
#'            3060, 3180, 2865, 2815, 3245, 2051, 2630)
#' sexo <- c(2, 2, 2, 1, 1, 1, 2, 1, 1, 1, 2, 2, 1, 1, 2)
#'
#' neonato <- data.frame(pesoRN, sexo)
#'
#' neonato$sexo <- as.factor(neonato$sexo)
#'
#' histNormal(dados = neonato,
#'            x = neonato$pesoRN,
#'            ylab = "Densidade de Probabilidade",
#'            xlab = "Peso ao nascer (g)")
#'
#'@export
histNormal <- function(dados=NULL, x=NULL, xlab = NULL, ylab = NULL){
  library(ggplot2)

  ggplot(dados) +
    geom_histogram(aes(x = x,
                       y =..density..),
                   bins = 15,
                   colour = "black",
                   fill = "salmon",
                   alpha = 0.2) +
    stat_function(fun = dnorm,
                  args = list(mean = mean(x, na.rm = T), sd = sd(x, na.rm = T)),
                  colour = "brown",
                  size = 1) +
    xlab (xlab) +
    ylab (ylab) +
    theme_classic()
}
