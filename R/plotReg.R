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
#'idade <- c(33, 24, 34, 35, 33, 25, 31, 28, 30, 34,
#'           24, 30, 35, 27, 32, 19, 26, 26, 26, 36)
#'
#'comp <-  c(98.0, 90.5, 97.5, 97.0, 96.0, 89.5, 97.0, 91.5, 92.0, 99.0,
#'           90.0, 96.0, 100.0, 87.0, 93.5, 82.0, 92.0, 89.0, 87.5, 100.0)
#'
#'dados <-  data.frame(idade, comp)
#'
#'shapiro.test(dados$idade)
#'shapiro.test(dados$comp)
#'
#'plotReg (df = dados, var_x = idade, var_y = comp) +
#'  labs(x="Idade (meses)", y="Comprimento (cm)")
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
