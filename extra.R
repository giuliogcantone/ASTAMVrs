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
