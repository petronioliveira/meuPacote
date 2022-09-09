#'Função Coeficiente de Variação
#'
#'Calcula o coeficiente de variação, expresso em porcentagem
#'
#'@author Petronio Fagundes de Oliveira Filho
#'
#'@param x é um vetor numérico

#'@examples
#'
#'library (readxl)
#'dados <- read_excel("C:/Users/petro/Dropbox/Estatística/Bioestatística usando o R/dadosPop.xlsx")
#'
#'coefVar (dados$altura)
#'
#'@export
coefVar <- function (x) {
  (sd(x, na.rm=TRUE) / mean(x, na.rm=TRUE))*100
}
