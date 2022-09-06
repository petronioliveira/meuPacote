#'Função Coeficiente de Variação
#'
#'Calcula o coeficiente de variação, expresso em porcentagem
#'
#'@author Marcos V C Vital, da Academia do R
#'
#'@param x é um vetor numérico

#'@examples
#'
#'library (dplyr)
#'library (readxl)
#'dados <- read_excel("C:/Users/petro/Dropbox/Estatística/Bioestatística usando o R/dadosPop.xlsx") %>%
#'select (altura)
#'
#'coef_var (dados$altura)
#'
#'@export
coef_var <- function (x) {
  (sd(x, na.rm=TRUE) / mean(x, na.rm=TRUE))*100
}
