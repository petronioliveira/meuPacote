#' Função que calcula resumo de vários grupos
#'
#' Esta função calcula um resumo de vários grupos
#'
#' @param data é um dataframe com os dados
#' @param var.grupo é uma variável categórica que define os grupos
#' @param y é a variável numérica que será resumida
#'
#' @export
sumarioGrupos <- function(data = NULL, var.grupo = NULL, y = NULL){

  if(is.null(data)) stop("Informar dados")
  if(is.null(var.grupo)) stop("Informar a variável grupo")
  if(is.null(y)) stop("Informar os dados a variável numérica y")

  sumario <- data %>%
    group_by({{var.grupo}}) %>%
    summarise(n = n (),
              media = mean({{y}}, na.rm = TRUE),
              dp = sd ({{y}}, na.rm = TRUE),
              mediana = median ({{y}}, na.rm = TRUE),
              IIQ = IQR ({{y}}, na.rm = TRUE),
              ic = std.error(y)*qt(1 - (0.05/2), n - 1),
              linf = media - ic,
              lsup = media + ic)
  print (sumario)
}
