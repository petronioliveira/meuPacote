# ic_z imprime mensagem no formato correto

    Code
      ic_z(x, sigma = 2)
    Message
      IC 95%: [48.0 ; 50.0]

# ic_z erro: x invalido

    Code
      ic_z("a", sigma = 2)
    Condition
      Error in `ic_z()`:
      ! `x` deve ser um vetor numerico com pelo menos 1 elemento.

---

    Code
      ic_z(numeric(0), sigma = 2)
    Condition
      Error in `ic_z()`:
      ! `x` deve ser um vetor numerico com pelo menos 1 elemento.

# ic_z erro: sigma invalido

    Code
      ic_z(x, sigma = 0)
    Condition
      Error in `ic_z()`:
      ! `sigma` deve ser um numero positivo.

---

    Code
      ic_z(x, sigma = -1)
    Condition
      Error in `ic_z()`:
      ! `sigma` deve ser um numero positivo.

---

    Code
      ic_z(x, sigma = "dois")
    Condition
      Error in `ic_z()`:
      ! `sigma` deve ser um numero positivo.

# ic_z erro: conf_level invalido

    Code
      ic_z(x, sigma = 2, conf_level = 0)
    Condition
      Error in `ic_z()`:
      ! `conf_level` deve estar entre 0 e 1 (exclusive).

---

    Code
      ic_z(x, sigma = 2, conf_level = 1.5)
    Condition
      Error in `ic_z()`:
      ! `conf_level` deve estar entre 0 e 1 (exclusive).

