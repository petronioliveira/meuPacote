#' Função Risco Relativo
#'
#' Esta função calcula o Risco Relativo a partir de uma tabela 2 x 2
#'
#' @author Petronio Fagundes de Oliveira Filho
#'
#' @param tabela é uma tabela de tabulação cruzada. A tabela 2 x 2 deve ser criada
#' antes e nomeada como tabela
#' @param alpha é o nível de significância, padrão é 0.05
#' @param referencerow é a linha da tabela que será a referência, o padrão é 2
#'
#'@details
#'
#' Basta criar uma tabela 2 x 2 para usar no modelo. Nomeia-a como quiser.
#'
#'
#'@export
riscoRelativo <- function(tabela,alpha=0.05,referencerow=2)
{
  numLinha <- nrow(tabela)
  nomeLinha <- rownames(tabela)
  for (i in 1:numLinha)
  {
    rowname <- nomeLinha[i]
    doença_NãoExposto <- tabela[referencerow,1]
    controle_NãoExposto <- tabela[referencerow,2]
    if (i != referencerow)
    {
      doença_Exposto <- tabela[i,1]
      controle_Exposto <- tabela[i,2]
      total_Exposto <- doença_Exposto + controle_Exposto
      total_NãoExposto <- doença_NãoExposto + controle_NãoExposto
      prob_doençaExposto <- doença_Exposto/total_Exposto
      prob_doençaNãoExposto <- doença_NãoExposto/total_NãoExposto
      prob_controleExposto <-  controle_Exposto/total_Exposto
      prob_controleNãoExposto <- controle_NãoExposto/total_NãoExposto

      # calcular o risco relativo
      RR <- round(prob_doençaExposto/prob_doençaNãoExposto, 2)
      print(paste("RR = ",RR))

      oddsRatio <- round((prob_doençaExposto*prob_controleNãoExposto)/
                           (prob_controleExposto*prob_doençaNãoExposto),2)

      # calcular o intervalo de confiança
      nivelConfiança <- (1 - alpha)*100
      sigma <- sqrt((1/doença_Exposto) - (1/total_Exposto) +
                      (1/doença_NãoExposto) - (1/total_NãoExposto))

      # sigma é o erro padrão da estimativa do log de risco relativo
      z <- qnorm(1-(alpha/2))
      limInf <- round((RR * exp(-z * sigma)), 2)
      limSup <- round((RR * exp( z * sigma)), 2)
      print(paste("IC",nivelConfiança,
                  "%  = [",limInf,",",limSup,"]"))
    }
  }
}

