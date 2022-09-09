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
#'library (readxl)
#'dados <- read_excel("C:/Users/petro/Dropbox/Estatística/Bioestatística usando o R/dadosPop.xlsx")
#'
#'erroPad (dados$altura)
#'
#'@export
erroPad <- function(x){
  sd(x)/sqrt(length(x))
}
