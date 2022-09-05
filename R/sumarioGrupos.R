#' Função que calcula resumo de vários grupos
#'
#' Esta função calcula um resumo de vários grupos
#'
#' @param data é um dataframe com os dados
#' @param var.grupo é uma variável categórica que define os grupos
#' @param y é a variável numérica que será resumida
#'
#' @export
sumarioGrupos <- function(dados = NULL, var.grupo = NULL, var.teste = NULL){
      dados %>%
      group_by({{var.grupo}}) %>%
      summarise(n = n (),
                media = mean(altura, na.rm = TRUE),
                dp = sd (altura, na.rm = TRUE),
                mediana = median (altura, na.rm = TRUE),
                IIQ = IQR (altura, na.rm = TRUE),
                ic = plotrix::std.error(altura, na.rm = TRUE)*qt(1 - (0.05/2), n - 1),
                linf = media - ic,
                lsup = media + ic)
}

