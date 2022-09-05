#'Função de teste t com boxplots
#'
#'Esta calcula um teste t e desenha os boxplots com o resultado do teste
#'
#'@param df é um dataframe com os dados
#'@param var.x é uma variável categórica dicotômica (fator)
#'@param var.y é uma variável númerica
#'
#'@examples
#'
#' dados <- read_excel("C:/Users/petro/Dropbox/Estatística/Bioestatística usando o R/dadosPop.xlsx")
#' str(dados)
#' dados$pop <- as.factor(dados$pop)
#'
#' by (data = dados$altura, INDICES = dados$pop, FUN = shapiro.test)
#'
#' leveneTest(altura ~ pop, center = mean, data = dados)
#'
#' plot_bp (df = dados, var.x = dados$pop, var.y = dados$pop)
#'
#'@export
plot_bp <- function(df, var.x, var.y){
  library(ggplot2)
  ggplot(df, aes(x = {{var.x}}, y = {{var.y}}, fill = {{var.x}})) +
    geom_errorbar(stat = "boxplot", width = 0.1) +
    geom_boxplot() +
    theme_classic() +
    theme(legend.position = "none") +
    stat_compare_means(method = "t.test", label.x = 0.5)
}
