#' Histograma com curva normal sobreposta
#'
#' Constrói um histograma e sobrepõe a curva da distribuição normal
#' parametrizada pela média e desvio padrão dos dados.
#'
#' @author Petronio Fagundes de Oliveira Filho
#'
#' @param dados Dataframe com os dados.
#' @param x Vetor numérico base para o histograma.
#' @param xlab Rótulo do eixo x.
#' @param ylab Rótulo do eixo y.
#'
#' @return Um objeto ggplot.
#'
#' @examples
#' pesoRN <- c(3340, 3345, 3750, 3650, 3220, 4070, 3380, 3970,
#'             3060, 3180, 2865, 2815, 3245, 2051, 2630)
#' sexo <- c(2, 2, 2, 1, 1, 1, 2, 1, 1, 1, 2, 2, 1, 1, 2)
#' neonato <- data.frame(pesoRN, sexo)
#' histNormal(dados = neonato,
#'            x = neonato$pesoRN,
#'            ylab = "Densidade de Probabilidade",
#'            xlab = "Peso ao nascer (g)")
#'
#' @importFrom ggplot2 ggplot aes geom_histogram stat_function after_stat
#'   xlab ylab theme_classic
#' @importFrom stats dnorm sd
#' @export
histNormal <- function(dados = NULL, x = NULL, xlab = NULL, ylab = NULL) {
  ggplot2::ggplot(dados) +
    ggplot2::geom_histogram(
      ggplot2::aes(x = x, y = ggplot2::after_stat(density)),
      bins    = 15,
      colour  = "black",
      fill    = "salmon",
      alpha   = 0.2
    ) +
    ggplot2::stat_function(
      fun  = stats::dnorm,
      args = list(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)),
      colour    = "brown",
      linewidth = 1
    ) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::theme_classic()
}
