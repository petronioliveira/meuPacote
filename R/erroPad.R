#'Função Erro Padrão
#'
#'Calcula o erro padrão, medida de variação de uma média amostral em relação à
#'média da população. Ajuda a verificar a confiabilidade da média amostral, pois
#'atrávés dele podemos estimar o intervalo de confiança para a média.
#'
#'@author Petronio Fagundes de Oliveira Filho
#'
#'@param x é um vetor numérico

#'@examples
#'
#'pesoRN <- c(3786, 3891, 2761, 2891, 3880, 2780, 3188, 3387,
#'            3394, 3458, 3992, 2897, 3284, 3412, 3527, 2732,
#'            3373)
#'dba9824e23d8686f7459f68f074895a0e40f2325
#'
#'erroPad (pesoRN)
#'
#'@export
erroPad <- function(x){
  sd(x)/sqrt(length(x))
}
