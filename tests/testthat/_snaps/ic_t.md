# ic_t imprime mensagem no formato correto

    Code
      ic_t(x)
    Message
      IC 95%: [47.8 ; 50.2]

# ic_t erro: x invalido ou muito curto

    Code
      ic_t("a")
    Condition
      Error in `ic_t()`:
      ! `x` deve ser um vetor numérico com pelo menos 2 elementos.

---

    Code
      ic_t(42)
    Condition
      Error in `ic_t()`:
      ! `x` deve ser um vetor numérico com pelo menos 2 elementos.

# ic_t erro: conf_level invalido

    Code
      ic_t(x, conf_level = 0)
    Condition
      Error in `ic_t()`:
      ! `conf_level` deve estar entre 0 e 1 (exclusive).

---

    Code
      ic_t(x, conf_level = 1.5)
    Condition
      Error in `ic_t()`:
      ! `conf_level` deve estar entre 0 e 1 (exclusive).

