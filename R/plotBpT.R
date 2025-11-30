
#' Boxplots com Decisão Estatística Automática
#'
#' Esta função desenha boxplots comparando dois grupos e seleciona automaticamente
#' o teste estatístico adequado (t de Student, t de Welch ou Wilcoxon) baseando-se
#' nos pressupostos de normalidade (Shapiro-Wilk) e homogeneidade de variâncias (Teste F).
#'
#' @author Petronio Fagundes de Oliveira Filho
#'
#' @param df Dataframe com os dados.
#' @param var.x Variável categórica dicotômica (não citada) que define os grupos.
#' @param var.y Variável numérica dependente (não citada).
#' @return Um objeto ggplot com o boxplot e o resultado do teste estatístico.
#' @importFrom rlang enquos !!! .data
#' @importFrom ggplot2 ggplot aes geom_boxplot theme_classic theme labs
#' @importFrom ggpubr stat_compare_means
#' @importFrom dplyr pull
#' @importFrom stats shapiro.test var.test
#' @export
#' @examples
#' # Criando dados de exemplo
#' dados_normais <- data.frame(
#'    grupo = rep(c("A", "B"), each = 30),
#'    valor = c(rnorm(30, 10, 2), rnorm(30, 12, 2))
#' )
#' plotBpT(dados_normais, grupo, valor)
#'
plotBpT <- function(df, var.x, var.y){
  # Uso de rlang::enquos() para capturar as variáveis não citadas com segurança
  vars <- rlang::enquos(var.x, var.y)

  # 1. Extrair os vetores de dados (necessário para rodar os testes estatísticos)
  vetor_x <- dplyr::pull(df, !!vars[[1]])
  vetor_y <- dplyr::pull(df, !!vars[[2]])

  # Converter x para fator se não for
  if(!is.factor(vetor_x)) vetor_x <- as.factor(vetor_x)

  # Garantir que temos apenas 2 grupos
  if (nlevels(vetor_x) != 2) {
    stop("A variável 'var.x' deve ser dicotômica (ter exatamente 2 níveis/grupos).")
  }

  # 2. Verificar Normalidade (Shapiro-Wilk) por grupo
  # Se algum grupo tiver p < 0.05, consideramos não normal
  p_shapiro <- stats::tapply(vetor_y, vetor_x, function(x) {
    if (length(x) < 3) return(0) # Não testa com menos de 3 pontos
    stats::shapiro.test(x)$p.value
  })
  eh_normal <- all(p_shapiro > 0.05)

  # 3. Verificar Homogeneidade de Variâncias (Teste F / var.test)
  # Só faz sentido testar se for normal (embora var.test possa ser rodado sempre)
  if (eh_normal) {
    teste_var <- stats::var.test(vetor_y ~ vetor_x)
    eh_homogeneo <- teste_var$p.value > 0.05
  } else {
    # Se não é normal, ignoramos a homogeneidade e vamos direto para Wilcoxon
    eh_homogeneo <- FALSE
  }

  # 4. Decisão do Teste
  if (!eh_normal) {
    metodo_teste <- "wilcox.test"
    args_teste <- list()
    # Usamos paste0 para evitar espaços extras no subtitulo
    subtitulo <- paste0("Shapiro p<0.05 -> Teste Não-Paramétrico (Wilcoxon)")

  } else if (!eh_homogeneo) {
    metodo_teste <- "t.test"
    # var.equal = FALSE é o Teste t de Welch
    args_teste <- list(var.equal = FALSE)
    subtitulo <- paste0("Var. Desiguais (p<0.05) -> Teste t de Welch")

  } else {
    metodo_teste <- "t.test"
    # var.equal = TRUE é o Teste t de Student Clássico
    args_teste <- list(var.equal = TRUE)
    subtitulo <- paste0("Var. Homogêneas -> Teste t de Student Clássico")
  }

  # 5. Gerar o Gráfico
  p <- ggplot2::ggplot(df, ggplot2::aes(x = !!vars[[1]], y = !!vars[[2]], fill = !!vars[[1]])) +
    ggplot2::geom_boxplot(alpha = 0.7) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(
      title = "Comparação de Grupos com Decisão Estatística",
      subtitle = subtitulo
    ) +

    # Adiciona o valor p calculado usando o método decidido
    ggpubr::stat_compare_means(
      method = metodo_teste,
      method.args = args_teste,
      label.x.npc = "center"
    )
  return(p)
}
