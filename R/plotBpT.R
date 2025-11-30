<<<<<<< HEAD
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
=======
#'Função desenha boxplots com teste t de Student
#'
#'Esta função desenha os boxplots comparando dois grupos independentes e mostra
#'o resultado do teste t de Student
#'
#'@author Petronio Fagundes de Oliveira Filho
#'
#'@param df é um dataframe com os dados
#'@param var.x é uma variável categórica dicotômica (fator)
#'@param var.y é uma variável númerica
#'
#'@examples
#'
#' library (car)
#' altura <- c(1.50, 1.56, 1.63, 1.66, 1.60, 1.65, 1.49, 1.60, 1.56, 1.58,
#'             1.55, 1.66, 1.60, 1.56, 1.72, 1.65, 1.65, 1.57, 1.54, 1.55,
#'             1.65, 1.73, 1.46, 1.59, 1.62, 1.65, 1.62, 1.60, 1.62, 1.65,
#'             1.44, 1.39, 1.54, 1.36, 1.32, 1.39, 1.37, 1.40, 1.44, 1.44,
#'             1.42, 1.35, 1.39, 1.34, 1.43, 1.34, 1.32, 1.61, 1.34, 1.30,
#'             1.37, 1.24, 1.36, 1.44, 1.38, 1.49, 1.41, 1.29, 1.38, 1.42)
#'
#' pop <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
#'          1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
#'          2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
#'
#' dados <- data.frame (altura, pop)
#'
#' dados$pop <- as.factor(dados$pop)
#'
#' by (data = dados$altura, INDICES = dados$pop, FUN = shapiro.test)
#'
#' leveneTest(altura ~ pop, center = mean, data = dados)
#'
#' plotBpT (df = dados, var.x = pop, var.y = altura)
#'
#'@export
plotBpT <- function(df, var.x, var.y){
  library(ggplot2)
  library(ggpubr)
  ggplot(df, aes(x = {{var.x}}, y = {{var.y}}, fill = {{var.x}})) +
    geom_errorbar(stat = "boxplot", width = 0.1) +
    geom_boxplot() +
    theme_classic() +
    theme(legend.position = "none") +
    stat_compare_means(method = "t.test", label.x = 0.5)
>>>>>>> dba9824e23d8686f7459f68f074895a0e40f2325
}
