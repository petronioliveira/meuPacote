#' Intervalo de Confiança com Desvio Padrão Amostral (t de Student)
#'
#' Calcula o intervalo de confiança para a média quando o desvio padrão
#' populacional é desconhecido, utilizando a distribuição t de Student.
#'
#' @author Petronio Fagundes de Oliveira Filho
#'
#' @param x Vetor numérico com os dados amostrais.
#' @param conf_level Nível de confiança, entre 0 e 1 (padrão: 0,95).
#'
#' @return Um tibble invisível com as colunas: \code{n}, \code{media_amostral},
#'   \code{dp_amostral}, \code{gl}, \code{t_critico}, \code{margem_erro},
#'   \code{limite_inferior} e \code{limite_superior}.
#'   Imprime também uma mensagem formatada com o intervalo.
#'
#' @examples
#' x <- c(50, 48, 52, 48, 50, 51, 50, 51, 47, 47, 47, 49, 51, 50, 44)
#' ic_t(x)
#' ic_t(x, conf_level = 0.99)
#'
#' @importFrom glue glue
#' @importFrom stats qt
#' @importFrom tibble tibble
#' @export
ic_t <- function(x, conf_level = 0.95) {
  if (!is.numeric(x) || length(x) < 2)
    stop("`x` deve ser um vetor numerico com pelo menos 2 elementos.")
  if (!is.numeric(conf_level) || conf_level <= 0 || conf_level >= 1)
    stop("`conf_level` deve estar entre 0 e 1 (exclusive).")

  n     <- length(x)
  x_bar <- mean(x)
  s     <- sd(x)
  t     <- qt((1 + conf_level) / 2, df = n - 1)
  me    <- t * s / sqrt(n)

  resultado <- tibble::tibble(
    n               = n,
    media_amostral  = x_bar,
    dp_amostral     = s,
    gl              = n - 1,
    t_critico       = t,
    margem_erro     = me,
    limite_inferior = x_bar - me,
    limite_superior = x_bar + me
  )

  cat(glue::glue(
    "IC {conf_level * 100}%: ",
    "[{sprintf('%.1f', resultado$limite_inferior)} ; ",
    "{sprintf('%.1f', resultado$limite_superior)}]"
  ), "\n")

  invisible(resultado)
}
