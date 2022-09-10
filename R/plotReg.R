#'Função desenha gráfico de dispersão
#'
#'Esta função desenha um gráfico de dispersão com reta de ajuste com IC95% e
#'equação da reta
#'
#'@author Petronio Fagundes de Oliveira Filho
#'
#'@param df é um dataframe com os dados
#'@param var_x é a variável preditora (independente) numérica
#'@param var_y é uma variável desfecho (dependente) númerica
#'
#'@examples
#'
#' library (readxl)
#' library (ggplot2)
#' library (ggpubr)
#'
#'# Dados de crianças até 36 meses idade
#'dados <- read_excel("C:/Users/petro/Dropbox/Estatística/Bioestatística usando o R/dadosReg.xlsx")
#'
#'shapiro.test(dados$idade)
#'shapiro.test(dados$comp)
#'
#'plotReg (df = dados, var_x = idade, var_y = comp) +
#'         labs(x="Idade (meses)", y="Comprimento (cm)")
#'
#'@export
plotReg <- function(df, var_x, var_y){
  ggplot(df, aes(x = {{var_x}}, y = {{var_y}}, fill = {{var_x}})) +
    geom_point() +
    geom_smooth(method='lm', formula= y~x) +
    stat_regline_equation () +
    theme_classic() +
    theme(legend.position = "none")
}
