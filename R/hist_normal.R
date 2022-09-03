#' Função de curva normal sobreposta ao histograma
#'
#' Esta função constroi um histograma e coloca uma curva normal sobreposta
#'
#' @param dados é um dataframe com os dados
#' @param x é uma variável numérica base para o histograma
#' @param xlab é o rotulo do eixo x
#' @param ylab é o rótulo do eixo y
#'
#'@export
hist_normal <- function(dados=NULL, x=NULL, xlab = NULL, ylab = NULL){
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
