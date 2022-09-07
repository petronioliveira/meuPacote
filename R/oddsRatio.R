#' Função Odds Ratio
#'
#' Esta função calcula a Odds Ratio a partir de uma tabela 2 x 2
#'
#' @author Petronio Fagundes de Oliveira Filho
#'
#' @param tabela é uma tabela de tabulação cruzada. A tabela 2 x 2 deve ser criada
#' antes e nomeada como tabela. Ver Details
#' @param alpha é o nível de significância, padrão é 0.05
#' @param referencerow é a linha da tabela que será a referência, o padrão é 2
#' @param quiet argumento lógico, indicando se a saída detalhada deve ser
#' suprimida. O padrão é FALSE
#'
#' @details
#'
#' Basta criar uma tabela 2 x 2 para calcular RR, usando o formato:
#'
#' tabela <- matrix(c(valor1, valor2, valor3, valor4), nrow=2, byrow=TRUE)
#'
#' colnames(tabela) <- c("Doença","Sem doença")
#'
#' rownames(tabela) <- c("Exposto","Não exposto")
#'
#' print(tabela)
#'
#' Usar a tabela no modelo
#'
#'@export
OddsRatio <- function(tabela,alpha=0.05,referencerow=2,quiet=FALSE)
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
      prob_controleExposto <- controle_Exposto/total_Exposto
      prob_controleNãoExposto <- controle_NãoExposto/total_NãoExposto

      # calcular o OR
      oddsRatio <- round((prob_doençaExposto*prob_controleNãoExposto)/
                           (prob_controleExposto*prob_doençaNãoExposto),2)
      if (quiet == FALSE)
      {
        print(paste("OR = ",oddsRatio))
      }
      # Calcular o intervalo de confiança
      nivelConfiança <- (1 - alpha)*100
      sigma <- sqrt((1/doença_Exposto)+(1/controle_Exposto)+
                      (1/doença_NãoExposto)+(1/controle_NãoExposto))
      # sigma é o erro padrão da estimativa do log da OR
      z <- qnorm(1-(alpha/2))
      limInf <- round((oddsRatio * exp(-z * sigma)), 2)
      limSup <- round((oddsRatio * exp(z * sigma)), 2)
      if (quiet == FALSE)
      {
        print(paste("IC",nivelConfiança,
                    "%  = [",limInf,",",limSup,"]"))
      }
    }
  }
  if (quiet == TRUE && numLinha == 2) # Se houver apenas dois tratamentos (exposto/não exposto)
  {
    oddsRatio
  }
}

