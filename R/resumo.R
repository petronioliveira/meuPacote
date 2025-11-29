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

