#' Função Odds Ratio
#'
#' Calcula a Odds Ratio a partir de uma tabela 2 x 2.
#'
#' @author Petronio Fagundes de Oliveira Filho
#'
#' @param tabela Tabela de tabulação cruzada 2 x 2.
#' @param alpha Nível de significância (padrão: 0.05).
#' @param referencerow Linha de referência da tabela (padrão: 2).
#' @param quiet Se TRUE, suprime a saída detalhada (padrão: FALSE).
#'
#' @details
#' Crie a tabela 2 x 2 antes de usar a função:
#'
#' tabela <- matrix(c(valor1, valor2, valor3, valor4), nrow = 2, byrow = TRUE)
#'
#' colnames(tabela) <- c("Doenca", "Sem doenca")
#'
#' rownames(tabela) <- c("Exposto", "Nao exposto")
#'
#' @importFrom stats qnorm
#' @export
oddsRatio <- function(tabela, alpha = 0.05, referencerow = 2, quiet = FALSE) {
  num_linhas   <- nrow(tabela)
  nomes_linhas <- rownames(tabela)

  for (i in 1:num_linhas) {
    doenca_ref    <- tabela[referencerow, 1]
    controle_ref  <- tabela[referencerow, 2]

    if (i != referencerow) {
      doenca_exp    <- tabela[i, 1]
      controle_exp  <- tabela[i, 2]

      total_exp <- doenca_exp + controle_exp
      total_ref <- doenca_ref + controle_ref

      prob_doenca_exp   <- doenca_exp   / total_exp
      prob_doenca_ref   <- doenca_ref   / total_ref
      prob_controle_exp <- controle_exp / total_exp
      prob_controle_ref <- controle_ref / total_ref

      OR <- round((prob_doenca_exp * prob_controle_ref) /
                    (prob_controle_exp * prob_doenca_ref), 2)

      if (!quiet) print(paste("OR = ", OR))

      nivel_confianca <- (1 - alpha) * 100
      ep_log_or <- sqrt((1 / doenca_exp) + (1 / controle_exp) +
                          (1 / doenca_ref) + (1 / controle_ref))
      z      <- stats::qnorm(1 - (alpha / 2))
      lim_inf <- round(OR * exp(-z * ep_log_or), 2)
      lim_sup <- round(OR * exp( z * ep_log_or), 2)

      if (!quiet)
        print(paste("IC", nivel_confianca, "% = [", lim_inf, ",", lim_sup, "]"))
    }
  }

  if (quiet && num_linhas == 2) OR
}
