#'Função de teste t com boxplots
#'
#'Esta calcula um teste t e desenha os boxplots com o resultado do teste
#'
#'@param df é um dataframe com os dados
#'@param x_col é uma variável categórica dicotômica
#'@param y_col é uma variável númerica
#'
#'@example
#'
#'plot_bp ()
#'
#'@export
plot_bp <- function(df, x_col, y_col){
  library(ggplot2)
  ggplot(df, aes(x = {{x_col}}, y = {{y_col}}, fill = {{x_col}})) +
    geom_errorbar(stat = "boxplot", width = 0.1) +
    geom_boxplot() +
    theme_classic() +
    theme(legend.position = "none") +
    stat_compare_means(method = "t.test", label.x = 0.5)
}
