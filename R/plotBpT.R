#'Função desenha boxplots com teste t de Student
#'
#'Esta função desenha os boxplots comparando dois grupos independentes e mostra
#'o resultado do teste t de Student
#'
#'@author Petronio Fagundes de Oliveira Filho
#'
#'@param df é um dataframe com os dados
#'@param var.x é uma variável categórica dicotômica (fator)
#'@param var.y é uma variável númerica
#'
#'@examples
#'
#' library (car)
#' dados <- read_excel("C:/Users/petro/Dropbox/Estatística/Bioestatística usando o R/dadosPop.xlsx")
#' str(dados)
#' dados$pop <- as.factor(dados$pop)
#'
#' by (data = dados$altura, INDICES = dados$pop, FUN = shapiro.test)
#'
#' leveneTest(altura ~ pop, center = mean, data = dados)
#'
#' plotBpT (df = dados, var.x = pop, var.y = altura)
#'
#'@export
plotBpT <- function(df, var.x, var.y){
  library(ggplot2)
  library(ggpubr)
  ggplot(df, aes(x = {{var.x}}, y = {{var.y}}, fill = {{var.x}})) +
    geom_errorbar(stat = "boxplot", width = 0.1) +
    geom_boxplot() +
    theme_classic() +
    theme(legend.position = "none") +
    stat_compare_means(method = "t.test", label.x = 0.5)
}
