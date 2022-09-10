#'Função gráfico barra de erro
#'
#'Esta função desenha um gráfico de barra de erro
#'
#'@author Petronio Fagundes de Oliveira Filho
#'
#'@param d é um dataframe com os dados
#'@param x é uma variável categórica (fator)
#'@param y é uma variável númerica
#'@param erro é a margem de erro usada, ver exemplo
#'
#'@examples
#'library (readxl)
#'library (dplyr)
#'library (ggplot2)
#'
#'smoke <- read_excel("C:/Users/petro/Dropbox/Estatística/Bioestatística usando o R/dadosFumo.xlsx")
#'
#'smoke$fumo <- factor(smoke$fumo,
#'                     levels = c(1, 2, 3, 4),
#'                     labels = c("não", "leve", "moderado", "pesado"))
#'
#'smoke1 <- smoke %>%
#'dplyr::group_by(fumo) %>%
#'  summarise(n = n(),
#'            media = mean(pesoRN),
#'            dp = sd (pesoRN),
#'            me = dp/sqrt(n) * qt(1 - (0.05/2), n - 1))
#'
#'erroBarra (d = smoke1, y = "media", x = "fumo", erro = "me") +
#'           labs(x="Tabagismo", y="Peso ao nascer")
#'
#'@export
erroBarra <- function(d=NULL, x=NULL, y=NULL, erro=NULL){
library(ggplot2)

if(is.null(d)) stop("Informar dados")
if(is.null(x)) stop("Informar os dados para o eixo x")
if(is.null(y)) stop("Informar os dados para o eixo y")
if(is.null(erro)) stop("Informar erro")

cor.std <- adjustcolor('brown', alpha.f = .3)
fill.std <- adjustcolor('brown', alpha.f = .1)

d$li <- d[[y]]-d[[erro]]
d$ls <- d[[y]]+d[[erro]]

ggplot(d, aes_string(x=x, y=y, group=1)) +
  geom_point() +
  geom_errorbar(aes(ymin=li,
                    ymax=ls),
                col='black', width =.2) +
  geom_col(col='brown', fill='brown', alpha=.5) +
  theme_classic()+
  labs(x=NULL,
       y=NULL)
}
