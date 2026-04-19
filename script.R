source("setup.R")

results <- crossing(
  i = 1:3,
  lambda = lambda_vals
) %>%
  mutate(sim = pmap(list(i, lambda), function(i, lambda) {

    # 1. Variabili latenti
    Eta <- rnorm(n, 0, 1) |> sort(decreasing = TRUE)
    Csi <- rnorm(n, 0, 1)

    # 2. Errori
    e1 <- rnorm(n)
    e2 <- rnorm(n)
    e3 <- rnorm(n)
    e4 <- rnorm(n)
    e5 <- rnorm(n)

    # 3. Variabili osservate
    X1 <- Eta + lambda * Csi + e1
    X2 <- Eta + lambda * Csi + e2
    X3 <- Eta + e3
    X4 <- Eta + e4
    X5 <- Eta + e5

    datasets <- list(
      "X1_X2_X3"        = cbind(X1, X2, X3),
      "X1_X2_Csi"       = cbind(X1, X2, Csi),
      "X1_X2_X3_Csi"    = cbind(X1, X2, X3, Csi),
      "X1_X2_X3_X4"     = cbind(X1, X2, X3, X4),
      "X1_X2_X3_X4_Csi" = cbind(X1, X2, X3, X4, Csi),
      "X1_X2_X3_X4_X5"  = cbind(X1, X2, X3, X4, X5)
    )

    map_dfr(names(datasets), function(f) {

      X <- scale(datasets[[f]])

      alpha_val <- alpha_silent(X)$total$raw_alpha

      fit <- factanal(X, factors = 1, scores = "regression")
      Eta_hat <- fit$scores

      tibble(
        formula = f,
        alpha = alpha_val,
        R2 = R2_vars_fun(X, Eta_hat),
        H2 = mean(1 - fit$uniquenesses),
        id_top_hat = which.max(Eta_hat),
        cor = cor(Eta, Eta_hat),
       # Eta_hat = list(Eta_hat)  # opzionale
      )
    })
  })) %>%
  unnest(sim)

# Controllo
nrow(results)  # 3 * 6 * 6 = 108
