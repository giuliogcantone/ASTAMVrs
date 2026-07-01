library(tidyverse)
library(pcaPP)
library(xtable)
library(patchwork)

read.csv("multiverse.csv") |>
  as_tibble() -> mv

#-----------------------
# Part A
#-----------------------
p1 <- mv %>%
  group_by(id) %>%
  summarise(
    mean_rank = mean(rank, na.rm = TRUE),
    sd_rank   = sd(rank, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = mean_rank, y = sd_rank)) +
  geom_point(
    shape = 16,
    size = 2.4,
    colour = "#2B2B2B",
    alpha = 0.85
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x + I(x^2),
    se = FALSE,
    linewidth = 0.9,
    colour = "#B30000"
  ) +
  labs(x = "Avg. rank", y = "SD(rank)") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey92"),
    axis.title = element_text(size = 11),
    axis.text  = element_text(size = 10)
  ) +
  scale_x_continuous(breaks = c(1, 15, 30, 45, 60, 75))

#-----------------------
# Part B
#-----------------------
for_plots <- mv %>%
  group_by(id) %>%
  summarise(
    mean_rank = mean(rank, na.rm = TRUE),
    sd_rank   = sd(rank, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  {
    low_mean <- slice_min(., mean_rank, n = 5, with_ties = FALSE)
    high_mean <- anti_join(., low_mean, by = "id") %>%
      slice_max(mean_rank, n = 5, with_ties = FALSE)
    high_sd <- anti_join(., bind_rows(low_mean, high_mean), by = "id") %>%
      slice_max(sd_rank, n = 5, with_ties = FALSE)

    bind_rows(
      mutate(low_mean,  group = "Mean↓"),
      mutate(high_mean, group = "Mean↑"),
      mutate(high_sd,   group = "SD↑")
    )
  } %>%
  mutate(group = factor(group, levels = c("Mean↓", "Mean↑", "SD↑")))

plot_df <- mv %>%
  semi_join(for_plots, by = "id") %>%
  count(id, rank) %>%
  group_by(id) %>%
  mutate(freq = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    rank  = as.integer(rank),
    group = for_plots$group[match(id, for_plots$id)],
    id    = factor(id, levels = for_plots$id)
  ) %>%
  filter(!is.na(rank))

p2 <- ggplot(plot_df, aes(rank, freq, fill = group)) +
  geom_col(width = 1, colour = NA) +
  facet_wrap(~id, ncol = 5, nrow = 3, scales = "free_y") +
  scale_fill_manual(values = c(
    "Mean↓" = "#0072B2",
    "Mean↑" = "#D55E00",
    "SD↑"   = "#009E73"
  )) +
  scale_x_continuous(breaks = c(1, 25, 50, 75)) +
  coord_cartesian(xlim = c(1, 75)) +
  guides(fill = "none") +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    panel.spacing = unit(0.7, "lines")
  ) +
  ylab("f(rank)")

#-----------------------
# Print
#-----------------------
final_plot <-
  p1 / p2 +
  plot_layout(heights = c(0.75, 2.25)) +
  plot_annotation(tag_levels = "A") &
  theme(
    plot.tag = element_text(face = "bold", size = 16),
    plot.tag.position = c(0.985, 0.975)
  )

ggsave(
  "results.eps",
  final_plot,
  device = cairo_ps,
  width = 18,
  height = 24,
  units = "cm",
  fallback_resolution = 1200
)

### ANOVA

library(effectsize)
library(relaimpo)

mv %>%
  filter(extraction == "ml",
         scoring == "regression") %>%
  aov(rank ~ id + X, data = .) %>%
  omega_squared(partial = TRUE)

mv %>%
  filter(extraction == "minres",
         scoring == "regression") %>%
  aov(rank ~ id + X, data = .) %>%
  omega_squared(partial = TRUE)

lm(rank ~ id + X, data = mv)


df_sub <- mv %>%
  filter(extraction == "ml",
         scoring == "regression") %>%
  slice_sample(n = 1000) %>%
  mutate(
    rank = as.numeric(rank),
    X = as.factor(X)
  ) %>%
  na.omit()

mod <- lm(rank ~ id + X, data = df_sub)

calc.relimp(mod, type = "lmg", rela = TRUE)


###

mv %>%
  select(id, X, extraction, scoring, rank) %>%
  pivot_wider(
    names_from = extraction,
    values_from = rank,
    id_cols = c(id, X, scoring),
  ) %>%
  select(-id, -X, -scoring) %>%
  as.matrix() %>%
  cor.fk() %>%
  round(3) %>%
  xtable(digits = 3) %>%
  print(
    include.rownames = TRUE,
    floating = FALSE,
    comment = FALSE,
    print.results = FALSE
  ) %>%
  cat()


mv %>%
  select(id, X, extraction, scoring, rank) %>%
  pivot_wider(
    names_from = scoring,
    values_from = rank,
  ) %>%
  summarise(
    cor = cor.fk(Bartlett,regression)
  )
