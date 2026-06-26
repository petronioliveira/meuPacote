#' Função Risco Relativo
#'
#' Calcula o Risco Relativo a partir de uma tabela 2 x 2.
#'
#' @author Petronio Fagundes de Oliveira Filho
#'
#' @param tabela Tabela de tabulação cruzada 2 x 2.
#' @param alpha Nível de significância (padrão: 0.05).
#' @param referencerow Linha de referência da tabela (padrão: 2).
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
riscoRelativo <- function(tabela, alpha = 0.05, referencerow = 2) {
  num_linhas <- nrow(tabela)

  for (i in 1:num_linhas) {
    doenca_ref   <- tabela[referencerow, 1]
    controle_ref <- tabela[referencerow, 2]

    if (i != referencerow) {
      doenca_exp   <- tabela[i, 1]
      controle_exp <- tabela[i, 2]

      total_exp <- doenca_exp   + controle_exp
      total_ref <- doenca_ref   + controle_ref

      prob_doenca_exp <- doenca_exp / total_exp
      prob_doenca_ref <- doenca_ref / total_ref

      RR <- round(prob_doenca_exp / prob_doenca_ref, 2)
      print(paste("RR = ", RR))

      nivel_confianca <- (1 - alpha) * 100
      ep_log_rr <- sqrt((1 / doenca_exp) - (1 / total_exp) +
                          (1 / doenca_ref) - (1 / total_ref))
      z       <- stats::qnorm(1 - (alpha / 2))
      lim_inf <- round(RR * exp(-z * ep_log_rr), 2)
      lim_sup <- round(RR * exp( z * ep_log_rr), 2)

      print(paste("IC", nivel_confianca, "% = [", lim_inf, ",", lim_sup, "]"))
    }
  }
}
