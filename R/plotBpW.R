#'Função desenha boxplots com teste de Wilcoxon (Mann-Whitney)
#'
#'Esta função desenha os boxplots comparando dois grupos independentes e mostra
#'o resultado do teste de Wilcoxon
#'
#'@author Petronio Fagundes de Oliveira Filho
#'
#'@param df é um dataframe com os dados
#'@param var.x é uma variável categórica dicotômica
#'@param var.y é uma variável númerica
#'
#'@examples
#'
#'cirurgia <- read_excel ("C:/Users/petro/Dropbox/Estatística/Bioestatística usando o R/dadosCirurgia.xlsx")
#'
#'cirurgia$infec <- as.factor(cirurgia$infec)
#'
#'plotBpW(df = cirurgia, var.x = infec, var.y = tempohosp)
#'
#'@export
plotBpW <- function(df, var.x, var.y){
  library(ggplot2)
  ggplot(df, aes(x = {{var.x}}, y = {{var.y}}, fill = {{var.x}})) +
    geom_errorbar(stat = "boxplot", width = 0.1) +
    geom_boxplot() +
    theme_classic() +
    theme(legend.position = "none") +
    stat_compare_means(method = "wilcox.test", label.x = 0.5)
}
