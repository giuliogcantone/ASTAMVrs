library(tidyverse)


db <- read.csv("synthetic_data.csv")

db |>
  select(starts_with("X")) |>
  cor(use = "complete.obs") |>
  corrplot(method = "color", type = "upper", tl.col = "black")
