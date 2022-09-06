#'Função Coeficiente de Variação
#'
#'Função criada por Marcos V C Vital, da Academia do R
#'Criada em R 4.0.3
#'
#'Calcula o coeficiente de variação, expresso em porcentagem
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
