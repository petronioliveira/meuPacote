#'Função que desenha boxplots comparando duas médias com teste de Wilcox
#'
#'Esta calcula um teste de Wilcox para duas médias independentes e desenha os
#' boxplots com o resultado do teste
#'
#'@param df é um dataframe com os dados
#'@param var.x é uma variável categórica dicotômica
#'@param var.y é uma variável númerica
#'
#'@example
#'
#'plot_bpW (df, var.x, var.y)
#'
#'@export
plot_bpW <- function(df, var.x, var.y){
  library(ggplot2)
  ggplot(df, aes(x = {{var.x}}, y = {{var.y}}, fill = {{var.x}})) +
    geom_errorbar(stat = "boxplot", width = 0.1) +
    geom_boxplot() +
    theme_classic() +
    theme(legend.position = "none") +
    stat_compare_means(method = "wilcox.test", label.x = 0.5)
}
