db <- read.csv("data.csv")
source("extra.R")
library(dplyr)
library(corrplot)

cor_mat <- db |>
  select(starts_with("X")) |>
  cor(use = "complete.obs") %>%
  .^2 %>%
  sqrt()

p_mat <- pval_cor(db, .05)

n <- n

corrplot::corrplot(
  cor_mat,
  method = "color",
  type = "lower",
  tl.col = "black",
  col = colorRampPalette(c("white", "black"))(200),
  col.lim = c(0, 1),
  diag = FALSE
)

for (i in 1:n) {
  for (j in 1:n) {

    if (i <= j) next

    text(
      j,
      n - i + 1,
      labels = round(cor_mat[i, j], 2),
      col = if (p_mat[i, j] <= 0.05) "white" else "goldenrod",
      cex = 0.8
    )
  }
}
