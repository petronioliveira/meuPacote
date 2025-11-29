
# meuPacote

<!-- badges: start -->
<!-- badges: end -->

O objetivo do meu `meuPacote` é calcular algumas estatísticas básicas a partir de dados do usuário. As estatísticas calculadas incluem:  
  - d de Cohen,   
  - odds ratio,   
  - risco relativo,     
  - gráfico de barra de erro,  
  - gráfico de linha com barra de erro,  
  - histograma com curva normal,  
  - boxplot com teste de comparação de duas médias.

## Installation

Você pode instalar a versão de desenvolvimento do `meuPacote` diretamente do [GitHub](https://github.com/petronioliveira/petrusR) com o pacote `remotes`:

```r
# install.packages("remotes") 
remotes::install_github("petronioliveira/meuPacote")
```  

## Exemplo de uso

Este é um exemplo básico que mostra como resolver um problema comum:

``` r
library(meuPacote)
# 1. Criando dados simulados para exemplo
set.seed(123) # Garante que os números sejam iguais para todo mundo
dados_medicos <- data.frame(
  Grupo = rep(c("Placebo", "Medicamento"), each = 30),
  Resposta = c(rnorm(30, mean = 10, sd = 2), rnorm(30, mean = 12, sd = 2.5))
)

# 2. Gerando o gráfico com a decisão estatística
plotBpT(df = dados_medicos, var.x = Grupo, var.y = Resposta)
```

