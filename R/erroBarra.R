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
#'library (dplyr)
#'pesoRN <- c(3786, 3891, 2761, 2891, 3880, 2780, 3188, 3387,
#'            3394, 3458, 3992, 2897, 3284, 3412, 3527, 2732,
#'            3373, 3045, 2448, 3410, 3066, 2721, 2938, 3698,
#'            3821, 3348, 3522, 2892, 3260, 3671, 2698, 3594,
#'            1981, 3170, 2882, 3713, 3543, 4313, 2763, 2739,
#'            3563, 3209, 2752, 3645, 2549, 3258, 2816, 3040,
#'            2747, 2859, 2433, 3000, 3192, 2686, 1611, 3076,
#'            3710, 2796, 3017, 2753)
#'fumo <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
#'          2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
#'          3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
#'          4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4)
#'
#'tabagismo <- data.frame (pesoRN, fumo)
#'
#'tabagismo$fumo <- factor(tabagismo$fumo,
#'                         levels = c(1, 2, 3, 4),
#'                         labels = c("Não", "Leve", "Moderado", "Pesado"))
#'
#'smoke <- tabagismo %>%
#'  group_by(fumo) %>%
#'  summarise(n = n(),
#'            media = mean(pesoRN),
#'            dp = sd (pesoRN),
#'            me = dp/sqrt(n) * qt(1 - (0.05/2), n - 1))
#'
#'erroBarra (d = smoke,
#'           y = "media",
#'           x = "fumo",
#'           erro = "me") +
#'  labs(x="Tabagismo",
#'       y="Peso ao nascer (g)")
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

<<<<<<< HEAD
d$li <- d[[y]]
=======
d$li <- d[[y]]-d[[erro]]
>>>>>>> dba9824e23d8686f7459f68f074895a0e40f2325
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
