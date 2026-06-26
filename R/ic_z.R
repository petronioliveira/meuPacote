#' Intervalo de Confiança com Desvio Padrão Populacional Conhecido (Z)
#'
#' Calcula o intervalo de confiança para a média quando o desvio padrão
#' populacional (\eqn{\sigma}) é conhecido, utilizando a distribuição normal (Z).
#'
#' @author Petronio Fagundes de Oliveira Filho
#'
#' @param x Vetor numérico com os dados amostrais.
#' @param sigma Desvio padrão populacional (número positivo).
#' @param conf_level Nível de confiança, entre 0 e 1 (padrão: 0,95).
#'
#' @return Um tibble invisível com as colunas: \code{n}, \code{media_amostral},
#'   \code{margem_erro}, \code{limite_inferior} e \code{limite_superior}.
#'   Imprime também uma mensagem formatada com o intervalo.
#'
#' @examples
#' x <- c(50, 48, 52, 48, 50, 51, 50, 51, 47, 47, 47, 49, 51, 50, 44)
#' ic_z(x, sigma = 2)
#' ic_z(x, sigma = 2, conf_level = 0.99)
#'
#' @importFrom glue glue
#' @importFrom stats qnorm
#' @importFrom tibble tibble
#' @export
ic_z <- function(x, sigma, conf_level = 0.95) {
  if (!is.numeric(x) || length(x) < 1)
    stop("`x` deve ser um vetor numerico com pelo menos 1 elemento.")
  if (!is.numeric(sigma) || length(sigma) != 1 || sigma <= 0)
    stop("`sigma` deve ser um numero positivo.")
  if (!is.numeric(conf_level) || conf_level <= 0 || conf_level >= 1)
    stop("`conf_level` deve estar entre 0 e 1 (exclusive).")

  n     <- length(x)
  x_bar <- mean(x)
  z     <- qnorm((1 + conf_level) / 2)
  me    <- z * sigma / sqrt(n)

  resultado <- tibble::tibble(
    n               = n,
    media_amostral  = x_bar,
    margem_erro     = me,
    limite_inferior = x_bar - me,
    limite_superior = x_bar + me
  )

  message(glue::glue(
    "IC {conf_level * 100}%: ",
    "[{sprintf('%.1f', resultado$limite_inferior)} ; ",
    "{sprintf('%.1f', resultado$limite_superior)}]"
  ))

  invisible(resultado)
}
