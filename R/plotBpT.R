#' Boxplots com Decisão Estatística Automática
#'
#' Esta função desenha boxplots comparando dois grupos e seleciona automaticamente
#' o teste estatístico adequado (t de Student, t de Welch ou Wilcoxon) baseando-se
#' nos pressupostos de normalidade (Shapiro-Wilk) e homogeneidade de variâncias (Teste F).
#'
#' @author Petronio Fagundes de Oliveira Filho
#'
#' @param df Dataframe com os dados.
#' @param var.x Variável categórica dicotômica (fator) que define os grupos.
#' @param var.y Variável numérica dependente.
#'
#' @return Um objeto ggplot com o boxplot e o resultado do teste estatístico.
#' @export
#'
#' @examples
#' # Criando dados de exemplo
#' dados_normais <- data.frame(
#'   grupo = rep(c("A", "B"), each = 30),
#'   valor = c(rnorm(30, 10, 2), rnorm(30, 12, 2))
#' )
#' plotBpT(dados_normais, var.x = grupo, var.y = valor)
#'
plotBpT <- function(df, var.x, var.y){
  # Carregar bibliotecas necessárias internamente (boa prática em pacotes é usar ::)
  requireNamespace("ggplot2")
  requireNamespace("ggpubr")
  requireNamespace("dplyr")
  requireNamespace("stats")

  # 1. Extrair os vetores de dados usando tidy eval
  # Isso é necessário para rodar os testes fora do ggplot
  vetor_x <- dplyr::pull(df, {{var.x}})
  vetor_y <- dplyr::pull(df, {{var.y}})

  # Converter x para fator se não for
  if(!is.factor(vetor_x)) vetor_x <- as.factor(vetor_x)

  # 2. Verificar Normalidade (Shapiro-Wilk) por grupo
  # Se algum grupo tiver p < 0.05, consideramos não normal
  p_shapiro <- tapply(vetor_y, vetor_x, function(x) stats::shapiro.test(x)$p.value)
  eh_normal <- all(p_shapiro > 0.05)

  # 3. Verificar Homogeneidade de Variâncias (Teste F / var.test)
  # Usamos var.test (nativo) em vez de Levene para evitar dependência do pacote 'car'
  teste_var <- stats::var.test(vetor_y ~ vetor_x)
  eh_homogeneo <- teste_var$p.value > 0.05

  # 4. Decisão do Teste
  if (!eh_normal) {
    metodo_teste <- "wilcox.test"
    args_teste <- list()
    subtitulo <- paste("Shapiro p<0.05 -> Teste Não-Paramétrico (Wilcoxon)")

  } else if (!eh_homogeneo) {
    metodo_teste <- "t.test"
    args_teste <- list(var.equal = FALSE)
    subtitulo <- paste("Var. Desiguais (p<0.05) -> Teste t de Welch")

  } else {
    metodo_teste <- "t.test"
    args_teste <- list(var.equal = TRUE)
    subtitulo <- paste("Var. Homogêneas -> Teste t de Student Clássico")
  }

  # 5. Gerar o Gráfico
  p <- ggplot2::ggplot(df, ggplot2::aes(x = {{var.x}}, y = {{var.y}}, fill = {{var.x}})) +
    ggplot2::geom_boxplot(alpha = 0.7) + # Adicionei transparência para estética
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(subtitle = subtitulo) + # Mostra qual decisão foi tomada

    # Adiciona o valor p calculado
    ggpubr::stat_compare_means(
      method = metodo_teste,
      method.args = args_teste,
      label.x.npc = "center" # Centraliza o p-valor
    )

  return(p)
}
