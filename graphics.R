db <- read.csv("data.csv")
source("extra.R")
library(dplyr)
library(corrplot)

pval_cor(db, .05) %>%
  {
    .[rowSums(. > 0.05, na.rm = TRUE) <= 1,
      rowSums(. > 0.05, na.rm = TRUE) <= 1]
  } %>%
  rownames() -> selection

cor_mat <- db |>
  select(starts_with("X")) |>
  cor(use = "complete.obs") %>%
  .^2 %>%
  sqrt()

p_mat <- pval_cor(db, .05)

n <- length(p_mat[1,])


label_cols <- ifelse(
  rownames(cor_mat) %in% selection,
  "black",
  "red"
)


###

postscript(
  file = "correlogramma.eps",
  horizontal = FALSE,
  onefile = FALSE,
  paper = "special",
  width = 8,
  height = 8,
  family = "Helvetica"
)

cex_num <- min(0.8, 12 / n)

corrplot::corrplot(
  cor_mat,
  method = "color",
  type = "lower",
  tl.col = label_cols,
  col = colorRampPalette(c("white", "black"))(200),
  col.lim = c(0, 1),
  diag = TRUE
)

for (i in 1:n) {
  for (j in 1:n) {

    if (i < j) next

    text(
      x = j,
      y = n - i + 1,
      labels = sprintf("%.2f", cor_mat[i, j]),
      col = if (i == j || p_mat[i, j] <= 0.05) "white" else "goldenrod",
      cex = cex_num
    )
  }
}

dev.off()

browseURL("correlogramma.eps")
