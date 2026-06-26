#' Erro Padrão da Média
#'
#' Calcula o erro padrão da média amostral. Mede a variação esperada da
#' média amostral em torno da média populacional, sendo a base para a
#' construção de intervalos de confiança.
#'
#' @author Petronio Fagundes de Oliveira Filho
#'
#' @param x Vetor numérico.
#'
#' @return Valor numérico: \eqn{s / \sqrt{n}}.
#'
#' @examples
#' pesoRN <- c(3786, 3891, 2761, 2891, 3880, 2780, 3188, 3387,
#'             3394, 3458, 3992, 2897, 3284, 3412, 3527, 2732,
#'             3373)
#' erroPad(pesoRN)
#'
#' @importFrom stats sd
#' @export
erroPad <- function(x) {
  stats::sd(x) / sqrt(length(x))
}
