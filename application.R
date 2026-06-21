library(tidyverse)
source("extra.R")


db <- read.csv("synthetic_data.csv")

db |>
  select(starts_with("X")) |>
  cor(use = "complete.obs") |>
  corrplot::corrplot(method = "color", type = "upper", tl.col = "black")

pval_cor_t(db, .1) %>%
  {rowSums(. > 0.05, na.rm = TRUE)}

pval_cor_t(db, .1) %>%
  {
    .[rowSums(. > 0.05, na.rm = TRUE) <= 1,
      rowSums(. > 0.05, na.rm = TRUE) <= 1]
  } %>%
  rownames() -> selection

db |>
  select(selection) |>
  cor(use = "complete.obs") |> View()
  corrplot::corrplot(method = "color", type = "upper", tl.col = "black")
