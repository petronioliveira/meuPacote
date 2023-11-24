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
#' altura <- c(1.50, 1.56, 1.63, 1.66, 1.60, 1.65, 1.49, 1.60, 1.56, 1.58,
#'             1.55, 1.66, 1.60, 1.56, 1.72, 1.65, 1.65, 1.57, 1.54, 1.55,
#'             1.65, 1.73, 1.46, 1.59, 1.62, 1.65, 1.62, 1.60, 1.62, 1.65,
#'             1.44, 1.39, 1.54, 1.36, 1.32, 1.39, 1.37, 1.40, 1.44, 1.44,
#'             1.42, 1.35, 1.39, 1.34, 1.43, 1.34, 1.32, 1.61, 1.34, 1.30,
#'             1.37, 1.24, 1.36, 1.44, 1.38, 1.49, 1.41, 1.29, 1.38, 1.42)
#'
#' pop <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
#'          1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
#'          2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
#'
#' dados <- data.frame (altura, pop)
#'
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
