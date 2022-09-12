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
#'library (car)
#'tempohosp <- c(58, 11, 153,  30,  19,  19,  23,  37,  44, 117,
#'               109,  15,  30,  14,  50,  17,  22,38,  13,  27)
#'infec <- c('não', 'não', 'sim', 'sim', 'sim', 'não', 'não', 'sim', 'não', 'não',
#'           'não', 'não', 'não', 'não', 'sim', 'não', 'não', 'sim', 'não', 'não')
#'
#'cirurgia <- data.frame(tempohosp, infec)
#'
#'cirurgia$infec <- as.factor(cirurgia$infec)
#'
#'by (data = cirurgia$tempohosp, INDICES = cirurgia$infec, FUN = shapiro.test)
#'
#'leveneTest(tempohosp ~ infec, center = median, data = cirurgia)
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
