% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/histNormal.R
\name{histNormal}
\alias{histNormal}
\title{Função curva normal sobreposta ao histograma}
\usage{
histNormal(dados = NULL, x = NULL, xlab = NULL, ylab = NULL)
}
\arguments{
\item{dados}{é um dataframe com os dados}

\item{x}{é uma variável numérica base para o histograma}

\item{xlab}{é o rotulo do eixo x}

\item{ylab}{é o rótulo do eixo y}
}
\description{
Esta função constroi um histograma e coloca uma curva normal sobreposta
}
\examples{
pesoRN <- c(3340, 3345, 3750, 3650, 3220, 4070, 3380, 3970,
           3060, 3180, 2865, 2815, 3245, 2051, 2630)
sexo <- c(2, 2, 2, 1, 1, 1, 2, 1, 1, 1, 2, 2, 1, 1, 2)

neonato <- data.frame(pesoRN, sexo)

neonato$sexo <- as.factor(neonato$sexo)

histNormal(dados = neonato,
           x = neonato$pesoRN,
           ylab = "Densidade de Probabilidade",
           xlab = "Peso ao nascer (g)")

}
\author{
Petronio Fagundes de Oliveira Filho
}
