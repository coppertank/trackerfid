test_that("i gruppi sono numerati nell'ordine in cui compaiono", {
  expect_identical(
    indice_gruppi(c("b", "a", "b", "c", "a")),
    c(1L, 2L, 1L, 3L, 2L)
  )
  expect_identical(indice_gruppi(character(0)), integer(0))
  # i valori mancanti formano un gruppo come gli altri
  expect_identical(indice_gruppi(c(NA, "a", NA)), c(1L, 2L, 1L))
})

test_that("primo, ultimo e ultimo valido seguono l'ordine delle righe", {
  gruppo <- c(1L, 2L, 1L, 2L, 3L, 1L)
  x <- c("a", "b", NA, "d", NA, "f")
  expect_identical(primo_per_gruppo(x, gruppo), c("a", "b", NA))
  expect_identical(ultimo_per_gruppo(x, gruppo), c("f", "d", NA))

  # l'ultimo valido salta i mancanti, l'ultimo no
  y <- c("a", "b", "c", NA, NA, NA)
  expect_identical(ultimo_per_gruppo(y, gruppo), rep(NA_character_, 3))
  expect_identical(ultimo_valido_per_gruppo(y, gruppo), c("c", "b", NA))

  # un gruppo senza righe resta mancante
  expect_identical(
    primo_per_gruppo(c(10, 20), c(1L, 3L), 4),
    c(10, NA, 20, NA)
  )
  expect_identical(
    ultimo_valido_per_gruppo(c(10, NA), c(1L, 3L), 3),
    c(10, NA, NA)
  )
  # il risultato ha il tipo dei valori
  date <- as.Date("2025-01-01") + 0:2
  expect_identical(ultimo_per_gruppo(date, c(1L, 1L, 2L)), date[2:3])
})

test_that("somma e media per gruppo coincidono con il calcolo gruppo per gruppo", {
  set.seed(1)
  gruppo <- c(1:40, sample(40, 460, replace = TRUE))
  x <- stats::rnorm(500, 45, 0.01)
  parti <- split(x, gruppo)
  expect_equal(
    somma_per_gruppo(x, gruppo),
    unname(vapply(parti, sum, numeric(1)))
  )
  expect_equal(
    media_per_gruppo(x, gruppo),
    unname(vapply(parti, mean, numeric(1))),
    tolerance = 1e-14
  )
  # un gruppo senza righe ha somma zero
  expect_identical(somma_per_gruppo(c(1, 2), c(1L, 3L), 3), c(1, 0, 2))
  # i valori logici si contano
  expect_identical(
    somma_per_gruppo(c(TRUE, FALSE, TRUE), c(1L, 1L, 2L)),
    c(1, 1)
  )
})

test_that("estremi e quantili per gruppo seguono min, max e quantile", {
  set.seed(2)
  # il gruppo 31 ha una riga sola; i valori arrotondati si ripetono
  gruppo <- c(1:30, sample(30, 370, replace = TRUE), 31L)
  x <- round(stats::runif(length(gruppo), 0, 100), 1)
  parti <- split(x, gruppo)

  estremi <- estremi_per_gruppo(x, gruppo)
  expect_identical(estremi$minimo, unname(vapply(parti, min, numeric(1))))
  expect_identical(estremi$massimo, unname(vapply(parti, max, numeric(1))))

  for (prob in c(0, 0.25, 0.5, 0.9, 1)) {
    attesi <- vapply(
      parti,
      stats::quantile,
      numeric(1),
      probs = prob,
      names = FALSE
    )
    expect_equal(
      quantile_per_gruppo(x, gruppo, prob),
      unname(attesi),
      info = prob
    )
  }
})

test_that("i valori distinti e il più frequente ignorano i mancanti", {
  gruppo <- c(1L, 1L, 1L, 2L, 2L, 3L, 3L, 3L, 3L)
  x <- c("a", "a", "b", NA, NA, "c", "d", "d", "c")
  expect_identical(n_distinti_per_gruppo(x, gruppo), c(2L, 0L, 2L))
  # a parità di frequenza vince il valore comparso per ultimo
  expect_identical(piu_frequente_per_gruppo(x, gruppo), c("a", NA, "c"))
  expect_identical(
    piu_frequente_per_gruppo(rep(NA_character_, 3), c(1L, 1L, 2L)),
    rep(NA_character_, 2)
  )
  # i gruppi senza righe restano mancanti
  expect_identical(
    piu_frequente_per_gruppo(c("a", "b", "b"), c(1L, 3L, 3L), 3),
    c("a", NA, "b")
  )
})
