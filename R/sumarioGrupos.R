#' Função que calcula resumo de vários grupos
#'
#' Esta função calcula um resumo de vários grupos
#'
#' @param dados é um dataframe com os dados
#' @param var.grupo é uma variável categórica que define os grupos
#' @param var.teste é a variável numérica que será resumida
#'
#' @examples
#' dados <- read_excel("C:/Users/petro/Dropbox/Estatística/Bioestatística usando o R/dadosPop.xlsx")
#' str(dados)
#' dados$pop <- as.factor(dados$pop)
#' sumarioGrupos(dados = dados, var.grupo = dados$pop, var.teste = dados$altura)
#'
#' @export
sumarioGrupos <- function(dados = NULL, var.grupo = NULL, var.teste = NULL){
      dados %>%
      group_by({{var.grupo}}) %>%
      summarise(n = n (),
                media = mean(var.teste, na.rm = TRUE),
                dp = sd (var.teste, na.rm = TRUE),
                mediana = median (var.teste, na.rm = TRUE),
                IIQ = IQR (var.teste, na.rm = TRUE),
                ic = plotrix::std.error(var.teste, na.rm = TRUE)*qt(1 - (0.05/2), n - 1),
                linf = media - ic,
                lsup = media + ic)
}

