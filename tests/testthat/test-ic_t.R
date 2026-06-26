x <- c(50, 48, 52, 48, 50, 51, 50, 51, 47, 47, 47, 49, 51, 50, 44)

# --- Estrutura do retorno ---

test_that("ic_t retorna tibble com colunas corretas", {
  result <- ic_t(x)
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("n", "media_amostral", "dp_amostral", "gl",
                         "t_critico", "margem_erro",
                         "limite_inferior", "limite_superior"))
  expect_equal(nrow(result), 1L)
})

test_that("ic_t retorna de forma invisivel", {
  expect_equal(withVisible(ic_t(x))$visible, FALSE)
})

# --- Valores calculados ---

test_that("ic_t calcula valores corretos para conf_level = 0.95", {
  result <- ic_t(x)
  t_crit <- qt(0.975, df = 14)
  me_esperado <- t_crit * sd(x) / sqrt(15)
  expect_equal(result$n, 15L)
  expect_equal(result$media_amostral, mean(x))
  expect_equal(result$dp_amostral, sd(x))
  expect_equal(result$gl, 14L)
  expect_equal(result$t_critico, t_crit)
  expect_equal(result$margem_erro, me_esperado)
  expect_equal(result$limite_inferior, mean(x) - me_esperado)
  expect_equal(result$limite_superior, mean(x) + me_esperado)
})

test_that("ic_t com conf_level = 0.99 produz margem maior que 0.95", {
  r95 <- ic_t(x, conf_level = 0.95)
  r99 <- ic_t(x, conf_level = 0.99)
  expect_gt(r99$margem_erro, r95$margem_erro)
})

test_that("ic_t produz IC mais largo que ic_z (sigma estimado vs conhecido)", {
  r_t <- ic_t(x)
  r_z <- ic_z(x, sigma = sd(x))
  expect_gt(r_t$margem_erro, r_z$margem_erro)
})

# --- Mensagem formatada ---

test_that("ic_t imprime mensagem no formato correto", {
  expect_snapshot(ic_t(x))
})

# --- Validação de entrada ---

test_that("ic_t erro: x invalido ou muito curto", {
  expect_snapshot(error = TRUE, ic_t("a"))
  expect_snapshot(error = TRUE, ic_t(42))
})

test_that("ic_t erro: conf_level invalido", {
  expect_snapshot(error = TRUE, ic_t(x, conf_level = 0))
  expect_snapshot(error = TRUE, ic_t(x, conf_level = 1.5))
})
