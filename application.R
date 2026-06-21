library(tidyverse)
source("extra.R")
library(combinat)


db <- read.csv("synthetic_data.csv")

db |>
  select(starts_with("X")) |>
  cor(use = "complete.obs") |>
  corrplot::corrplot(method = "color", type = "upper", tl.col = "black")

pval_cor_t(db, .05) %>%
  {rowSums(. > 0.05, na.rm = TRUE)}

pval_cor_t(db, .05) %>%
  {
    .[rowSums(. > 0.05, na.rm = TRUE) <= 1,
      rowSums(. > 0.05, na.rm = TRUE) <= 1]
  } %>%
  rownames() -> selection

db |>
  select(selection) |>
  cor(use = "complete.obs") |> View()
  corrplot::corrplot(method = "color", type = "upper", tl.col = "black")


db2 <- db %>%
    select(id, all_of(selection))


partition <- db2 %>%
  {
    x_vars <- names(.)[startsWith(names(.), "X")]

    map(
      3:length(x_vars),
      ~ combn(x_vars, .x, simplify = FALSE)
    ) %>%
      unlist(recursive = FALSE) %>%
      map_chr(~ paste(.x, collapse = ",")) %>%
      unlist()
  }

autofa(db2,partition) -> multiverse

readr::write_csv(multiverse,
                 "multiverse.csv")
