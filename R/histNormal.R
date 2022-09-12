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
#' library (readxl)
#' library (dplyr)
#'
#' mater15 <- read_excel("C:/Users/petro/Dropbox/Estatística/Bioestatística usando o R/dadosMater15.xlsx") %>%
#'          select(pesoRN)
#'
#' histNormal(dados = mater15, x = mater15$pesoRN, ylab = "Peso ao nascer (g)", xlab = "Recém-nascidos")
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
