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
#'            3373, 3045, 2448, 3410, 3066, 2721, 2938, 3698,
#'            3821, 3348, 3522, 2892, 3260, 3671, 2698, 3594,
#'            1981, 3170, 2882, 3713, 3543, 4313, 2763, 2739,
#'            3563, 3209, 2752, 3645, 2549, 3258, 2816, 3040,
#'            2747, 2859, 2433, 3000, 3192, 2686, 1611, 3076,
#'            3710, 2796, 3017, 2753)
#'
#'erroPad (pesoRN)
#'
#'@export
erroPad <- function(x){
  sd(x)/sqrt(length(x))
}
