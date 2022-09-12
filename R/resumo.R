#' Função para calcular medidas resumidoras com gráficos
#'
#' Esta função calcula medidas resumidoras (média, desvio padrão, mediana, P25 e
#' P75), entregando junto um histograma com curva normal sobreposta e boxplot de
#' uma variável numérica
#'
#' @author Petronio Fagundes de Oliveira Filho
#'
#' @param x é um vetor de uma variável numérica
#' @param lab é o rótulo da variável númerica, escrita entre aspas
#'
#'@examples
#'# Peso de nascimento de 60 recém-nascidos:
#'pesoRN <- c(3786, 3891, 2761, 2891, 3880, 2780, 3188, 3387,
#'            3394, 3458, 3992, 2897, 3284, 3412, 3527, 2732,
#'            3373, 3045, 2448, 3410, 3066, 2721, 2938, 3698,
#'            3821, 3348, 3522, 2892, 3260, 3671, 2698, 3594,
#'            1981, 3170, 2882, 3713, 3543, 4313, 2763, 2739,
#'            3563, 3209, 2752, 3645, 2549, 3258, 2816, 3040,
#'            2747, 2859, 2433, 3000, 3192, 2686, 1611, 3076,
#'            3710, 2796, 3017, 2753)
#'
#'resumo(pesoRN, lab = "Peso ao nascer (g)")
#'
#'@export
resumo <- function(x, lab) {
  media = round(mean (x, na.rm = TRUE),3)
  dp = round(sd (x, na.rm = TRUE), 3)
  mediana = round(median (x, na.rm = TRUE), 3)
  P25 = round(quantile (x, 0.25, na.rm = TRUE), 3)
  P75 = round(quantile (x, 0.75, na.rm = TRUE), 3)
  tab <- matrix (c(media, dp, mediana, P25, P75))
  dimnames(tab) <- list(c("Média", "Desvio Padrão", "Mediana", "P25", "P75"),
                        "Valores")
  print(tab)
  h<-hist(x,
          breaks=15,
          col="red",
          xlab= lab,
          ylab = "Frequência",
          main="")
  xfit<-seq(min(x),max(x),length=40)
  yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
  yfit <- yfit*diff(h$mids[1:2])*length(x)
  lines(xfit, yfit, col="blue", lwd=2)
  boxplot(x, ylab = lab, col = "lightblue")
}

