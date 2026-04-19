library(tidyverse)
library(psych)
library(factoextra)
set.seed(1810)

R2_vars_fun <- function(X, scores) {
  X |>
    (\(Xmat) apply(Xmat, 2, \(x)
                   summary(lm(x ~ scores, na.action = na.omit))$r.squared
    ))() |>
    mean()
}

n <- 100
lambda_vals <- c(.5, .75, 1, 1.5, 2)

alpha_silent <- function(X) {
  suppressWarnings(
    suppressMessages(
      psych::alpha(X, check.keys = FALSE)
    )
  )
}

