#' Boxplots com Decisão Estatística Automática
#'
#' Desenha boxplots comparando dois grupos e seleciona automaticamente
#' o teste estatístico adequado (t de Student, t de Welch ou Wilcoxon)
#' com base nos pressupostos de normalidade (Shapiro-Wilk) e
#' homogeneidade de variâncias (Teste F).
#'
#' @author Petronio Fagundes de Oliveira Filho
#'
#' @param df Dataframe com os dados.
#' @param var.x Variável categórica dicotômica (sem aspas) que define os grupos.
#' @param var.y Variável numérica dependente (sem aspas).
#'
#' @return Um objeto ggplot com o boxplot e o p-valor do teste selecionado.
#'
#' @examples
#' dados <- data.frame(
#'   grupo = rep(c("A", "B"), each = 30),
#'   valor = c(rnorm(30, 10, 2), rnorm(30, 12, 2))
#' )
#' plotBpT(dados, grupo, valor)
#'
#' @importFrom dplyr pull
#' @importFrom ggplot2 ggplot aes geom_boxplot annotate theme_classic theme labs
#' @importFrom rlang enquos !!!  .data
#' @importFrom stats shapiro.test t.test var.test wilcox.test
#' @export
plotBpT <- function(df, var.x, var.y) {
  vars    <- rlang::enquos(var.x, var.y)
  vetor_x <- dplyr::pull(df, !!vars[[1]])
  vetor_y <- dplyr::pull(df, !!vars[[2]])

  if (!is.factor(vetor_x)) vetor_x <- as.factor(vetor_x)

  if (nlevels(vetor_x) != 2)
    stop("A variavel 'var.x' deve ser dicotomica (ter exatamente 2 niveis).")

  # Normalidade por grupo (Shapiro-Wilk)
  p_shapiro <- tapply(vetor_y, vetor_x, function(v) {
    if (length(v) < 3) return(0)
    stats::shapiro.test(v)$p.value
  })
  eh_normal <- all(p_shapiro > 0.05)

  # Homogeneidade de variancias (Teste F)
  eh_homogeneo <- if (eh_normal) {
    stats::var.test(vetor_y ~ vetor_x)$p.value > 0.05
  } else {
    FALSE
  }

  # Executa o teste selecionado e extrai p-valor
  if (!eh_normal) {
    resultado  <- stats::wilcox.test(vetor_y ~ vetor_x)
    metodo_txt <- "Wilcoxon"
  } else if (!eh_homogeneo) {
    resultado  <- stats::t.test(vetor_y ~ vetor_x, var.equal = FALSE)
    metodo_txt <- "t de Welch"
  } else {
    resultado  <- stats::t.test(vetor_y ~ vetor_x, var.equal = TRUE)
    metodo_txt <- "t de Student"
  }

  p_val <- resultado$p.value
  p_txt <- if (p_val < 0.001) "p < 0.001" else paste0("p = ", round(p_val, 3))
  label <- paste0(metodo_txt, ": ", p_txt)

  ggplot2::ggplot(df, ggplot2::aes(
    x    = !!vars[[1]],
    y    = !!vars[[2]],
    fill = !!vars[[1]]
  )) +
    ggplot2::geom_boxplot(alpha = 0.7) +
    ggplot2::annotate(
      "text",
      x     = 1.5,
      y     = max(vetor_y, na.rm = TRUE),
      label = label,
      size  = 4
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(
      subtitle = paste0("Teste: ", metodo_txt)
    )
}
