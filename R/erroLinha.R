#' Gráfico de linha com barra de erro
#'
#' Desenha um gráfico de linha com barras de erro.
#'
#' @author Petronio Fagundes de Oliveira Filho
#'
#' @param d Dataframe com os dados.
#' @param x Nome da variável categórica (fator), como string.
#' @param y Nome da variável numérica (média ou similar), como string.
#' @param erro Nome da coluna com a margem de erro, como string.
#'
#' @return Um objeto ggplot.
#'
#' @examples
#' library(dplyr)
#' pesoRN <- c(3786, 3891, 2761, 2891, 3880, 2780, 3188, 3387,
#'             3394, 3458, 3992, 2897, 3284, 3412, 3527, 2732,
#'             3373, 3045, 2448, 3410, 3066, 2721, 2938, 3698,
#'             3821, 3348, 3522, 2892, 3260, 3671, 2698, 3594,
#'             1981, 3170, 2882, 3713, 3543, 4313, 2763, 2739,
#'             3563, 3209, 2752, 3645, 2549, 3258, 2816, 3040,
#'             2747, 2859, 2433, 3000, 3192, 2686, 1611, 3076,
#'             3710, 2796, 3017, 2753)
#' fumo <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
#'           2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
#'           3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
#'           4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4)
#' tabagismo <- data.frame(pesoRN, fumo)
#' tabagismo$fumo <- factor(tabagismo$fumo,
#'                          levels = c(1, 2, 3, 4),
#'                          labels = c("Nao", "Leve", "Moderado", "Pesado"))
#' smoke <- tabagismo |>
#'   group_by(fumo) |>
#'   summarise(n = n(),
#'             media = mean(pesoRN),
#'             dp = sd(pesoRN),
#'             me = dp / sqrt(n) * qt(1 - (0.05 / 2), n - 1))
#' erroLinha(d = smoke, y = "media", x = "fumo", erro = "me") +
#'   ggplot2::labs(x = "Tabagismo", y = "Peso ao nascer (g)")
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_errorbar theme_classic labs
#' @export
erroLinha <- function(d = NULL, x = NULL, y = NULL, erro = NULL) {
  if (is.null(d))    stop("Informar dados")
  if (is.null(x))    stop("Informar os dados para o eixo x")
  if (is.null(y))    stop("Informar os dados para o eixo y")
  if (is.null(erro)) stop("Informar da margem de erro")

  d$li <- d[[y]] - d[[erro]]
  d$ls <- d[[y]] + d[[erro]]

  ggplot2::ggplot(d, ggplot2::aes(x = .data[[x]], y = .data[[y]], group = 1)) +
    ggplot2::geom_point() +
    ggplot2::geom_line(linetype = "dashed") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$li, ymax = .data$ls),
                           col = "red", width = 0.2) +
    ggplot2::theme_classic() +
    ggplot2::labs(x = NULL, y = NULL)
}
