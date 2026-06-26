x <- c(50, 48, 52, 48, 50, 51, 50, 51, 47, 47, 47, 49, 51, 50, 44)

# --- Estrutura do retorno ---

test_that("ic_z retorna tibble com colunas corretas", {
  result <- ic_z(x, sigma = 2)
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("n", "media_amostral", "margem_erro",
                         "limite_inferior", "limite_superior"))
  expect_equal(nrow(result), 1L)
})

test_that("ic_z retorna de forma invisivel", {
  expect_equal(withVisible(ic_z(x, sigma = 2))$visible, FALSE)
})

# --- Valores calculados ---

test_that("ic_z calcula valores corretos para conf_level = 0.95", {
  result <- ic_z(x, sigma = 2)
  me_esperado <- qnorm(0.975) * 2 / sqrt(15)
  expect_equal(result$n, 15L)
  expect_equal(result$media_amostral, mean(x))
  expect_equal(result$margem_erro, me_esperado)
  expect_equal(result$limite_inferior, mean(x) - me_esperado)
  expect_equal(result$limite_superior, mean(x) + me_esperado)
})

test_that("ic_z com conf_level = 0.99 produz margem maior que 0.95", {
  r95 <- ic_z(x, sigma = 2, conf_level = 0.95)
  r99 <- ic_z(x, sigma = 2, conf_level = 0.99)
  expect_gt(r99$margem_erro, r95$margem_erro)
})

# --- Mensagem formatada ---

test_that("ic_z imprime mensagem no formato correto", {
  expect_snapshot(ic_z(x, sigma = 2))
})

# --- Validação de entrada ---

test_that("ic_z erro: x invalido", {
  expect_snapshot(error = TRUE, ic_z("a", sigma = 2))
  expect_snapshot(error = TRUE, ic_z(numeric(0), sigma = 2))
})

test_that("ic_z erro: sigma invalido", {
  expect_snapshot(error = TRUE, ic_z(x, sigma = 0))
  expect_snapshot(error = TRUE, ic_z(x, sigma = -1))
  expect_snapshot(error = TRUE, ic_z(x, sigma = "dois"))
})

test_that("ic_z erro: conf_level invalido", {
  expect_snapshot(error = TRUE, ic_z(x, sigma = 2, conf_level = 0))
  expect_snapshot(error = TRUE, ic_z(x, sigma = 2, conf_level = 1.5))
})
