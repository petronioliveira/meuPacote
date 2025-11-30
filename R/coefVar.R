#'Função Coeficiente de Variação
#'
#'Calcula o coeficiente de variação, expresso em porcentagem
#'
#'@author Petronio Fagundes de Oliveira Filho
#'
#'@param x é um vetor numérico

#'@examples
#'pesoRN <- c(3340, 3345, 3750, 3650, 3220, 4070, 3380, 3970,
#'            3060, 3180, 2865, 2815, 3245, 2051, 2630)
#'
#'sexo <- c(2, 2, 2, 1, 1, 1, 2, 1, 1, 1, 2, 2, 1, 1, 2)
#'
#'neonato <- data.frame(pesoRN, sexo)
#'
#'neonato$sexo <- as.factor(neonato$sexo)
#'
#'coefVar (neonato$pesoRN)
#'
#'@export
coefVar <- function (x) {
  (sd(x, na.rm=TRUE) / mean(x, na.rm=TRUE))*100
}
