pval_cor_t <- function(data, t = 0, prefix = "X") {

  X <- data[, grepl(paste0("^", prefix), names(data))]

  p <- ncol(X)

  out <- matrix(
    NA_real_,
    p, p,
    dimnames = list(names(X), names(X))
  )

  for(i in seq_len(p)) {
    for(j in i:p) {

      if(i == j) next

      cc <- complete.cases(X[[i]], X[[j]])

      x <- X[[i]][cc]
      y <- X[[j]][cc]

      n <- length(x)

      if(n < 4) {
        pv <- NA_real_
      } else {

        r <- cor(x, y)

        z <- (atanh(r) - atanh(t)) * sqrt(n - 3)

        pv <- 1 - pnorm(z)
      }

      out[i, j] <- pv
      out[j, i] <- pv
    }
  }

  round(out, 2) -> out
  out
}


autofa <- function(db, partition) {

  db_std <- db
  db_std[setdiff(names(db), "id")] <- scale(db_std[setdiff(names(db), "id")])

  purrr::map_dfr(seq_along(partition), function(i) {

    vars <- strsplit(partition[i], ",")[[1]]

    eta <- tryCatch({
      fa <- factanal(db_std[, vars], factors = 1, scores = "regression")
      fa$scores[, 1]
    }, error = function(e) {
      rep(NA, nrow(db_std))
    })

    tibble::tibble(
      id = db_std$id,
      Eta = eta,
      X = partition[i]
    ) %>%
      mutate(
        rank = rank(-Eta, ties.method = "first")
      )
  })
}


autofa <- function(db, partition) {

  methods <- expand.grid(
    extraction = c("ml", "pa", "minres", "gls"),
    scoring = c("regression", "Bartlett"),
    stringsAsFactors = FALSE
  )

  db_std <- db

  db_std[setdiff(names(db_std), "id")] <-
    scale(db_std[setdiff(names(db_std), "id")])

  purrr::pmap_dfr(
    methods,
    function(extraction, scoring) {

      purrr::map_dfr(seq_along(partition), function(i) {

        vars <- strsplit(partition[i], ",")[[1]]

        eta <- tryCatch({

          fa_fit <- psych::fa(
            r = db_std[, vars],
            nfactors = 1,
            fm = extraction,
            scores = scoring
          )

          as.numeric(fa_fit$scores[, 1])

        }, error = function(e) {

          rep(NA_real_, nrow(db_std))

        })

        tibble::tibble(
          id = db_std$id,
          Eta = eta,
          rank = rank(-eta, ties.method = "first"),
          X = partition[i],
          extraction = extraction,
          scoring = scoring
        )

      })

    }
  )
}
