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
#' @return Um objeto ggplot com o boxplot e o resultado do teste estatístico.
#'
#' @examples
#' dados <- data.frame(
#'   grupo = rep(c("A", "B"), each = 30),
#'   valor = c(rnorm(30, 10, 2), rnorm(30, 12, 2))
#' )
#' plotBpT(dados, grupo, valor)
#'
#' @importFrom dplyr pull
#' @importFrom ggplot2 ggplot aes geom_boxplot theme_classic theme labs
#' @importFrom ggpubr stat_compare_means
#' @importFrom rlang enquos !!!  .data
#' @importFrom stats shapiro.test var.test
#' @export
plotBpT <- function(df, var.x, var.y) {
  vars   <- rlang::enquos(var.x, var.y)
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
  if (eh_normal) {
    eh_homogeneo <- stats::var.test(vetor_y ~ vetor_x)$p.value > 0.05
  } else {
    eh_homogeneo <- FALSE
  }

  # Decisao do teste
  if (!eh_normal) {
    metodo_teste <- "wilcox.test"
    args_teste   <- list()
    subtitulo    <- "Shapiro p<0.05 -> Teste Nao-Parametrico (Wilcoxon)"
  } else if (!eh_homogeneo) {
    metodo_teste <- "t.test"
    args_teste   <- list(var.equal = FALSE)
    subtitulo    <- "Var. Desiguais (p<0.05) -> Teste t de Welch"
  } else {
    metodo_teste <- "t.test"
    args_teste   <- list(var.equal = TRUE)
    subtitulo    <- "Var. Homogeneas -> Teste t de Student"
  }

  ggplot2::ggplot(df, ggplot2::aes(
    x    = !!vars[[1]],
    y    = !!vars[[2]],
    fill = !!vars[[1]]
  )) +
    ggplot2::geom_boxplot(alpha = 0.7) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(
      title    = "Comparacao de Grupos com Decisao Estatistica",
      subtitle = subtitulo
    ) +
    ggpubr::stat_compare_means(
      method      = metodo_teste,
      method.args = args_teste,
      label.x.npc = "center"
    )
}
