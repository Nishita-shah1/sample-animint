# Simple Random Sampling - animint2 port
# animation package original: https://yihui.org/animation/example/sample-simple/
# GSoC 2026 - animint2 medium task

library(animint2)

set.seed(99)

N       <- 100
samp_n  <- 20
n_draws <- 40

population <- data.frame(
  id    = seq_len(N),
  x     = runif(N, 1, 10),
  y     = runif(N, 1, 10),
  value = rnorm(N, mean = 50, sd = 10)
)

pop_mean <- mean(population$value)

sample_df <- do.call(rbind, lapply(seq_len(n_draws), function(draw) {
  ids <- sample(seq_len(N), size = samp_n, replace = FALSE)
  data.frame(
    draw     = draw,
    id       = ids,
    x        = population$x[ids],
    y        = population$y[ids],
    value    = population$value[ids],
    point_id = seq_along(ids)
  )
}))

unsampled_df <- do.call(rbind, lapply(seq_len(n_draws), function(draw) {
  ids <- sample_df$id[sample_df$draw == draw]
  unsampled_ids <- setdiff(seq_len(N), ids)
  data.frame(
    draw     = draw,
    id       = unsampled_ids,
    x        = population$x[unsampled_ids],
    y        = population$y[unsampled_ids],
    point_id = seq_along(unsampled_ids)
  )
}))

metrics_df <- do.call(rbind, lapply(seq_len(n_draws), function(draw) {
  vals <- sample_df$value[sample_df$draw == draw]
  data.frame(
    draw        = draw,
    sample_mean = mean(vals),
    sample_sd   = sd(vals),
    pop_mean    = pop_mean
  )
}))

cum_mean_df <- do.call(rbind, lapply(seq_len(n_draws), function(draw) {
  all_vals <- sample_df$value[sample_df$draw <= draw]
  data.frame(
    draw     = draw,
    cum_mean = mean(all_vals)
  )
}))

# --- plots ---

pscatter <- ggplot() +
  geom_point(
    data         = unsampled_df,
    aes(x = x, y = y, key = id),
    showSelected = "draw",
    color        = "gray70",
    size         = 3
  ) +
  geom_point(
    data         = sample_df,
    aes(x = x, y = y, key = id),
    showSelected = "draw",
    color        = "steelblue",
    size         = 4,
    alpha        = 0.9
  ) +
  labs(
    title = "Population (gray) and Selected Sample (blue)",
    x     = "x",
    y     = "y"
  ) +
  theme_bw()

pmeans <- ggplot() +
  geom_tallrect(
    data         = metrics_df,
    aes(xmin = draw - 0.5, xmax = draw + 0.5),
    clickSelects = "draw",
    alpha        = 0.2,
    fill         = "gold"
  ) +
  geom_hline(
    yintercept = pop_mean,
    color      = "red",
    linetype   = "dashed",
    size       = 1
  ) +
  geom_line(
    data  = metrics_df,
    aes(x = draw, y = sample_mean),
    color = "gray50",
    size  = 0.8
  ) +
  geom_point(
    data  = metrics_df,
    aes(x = draw, y = sample_mean),
    color = "steelblue",
    size  = 2
  ) +
  geom_point(
    data         = metrics_df,
    aes(x = draw, y = sample_mean, key = draw),
    showSelected = "draw",
    color        = "orange",
    size         = 5
  ) +
  labs(
    title = "Sample Mean per Draw (red = population mean)",
    x     = "Draw",
    y     = "Sample mean"
  ) +
  theme_bw()

pcum <- ggplot() +
  geom_tallrect(
    data         = metrics_df,
    aes(xmin = draw - 0.5, xmax = draw + 0.5),
    clickSelects = "draw",
    alpha        = 0.2,
    fill         = "gold"
  ) +
  geom_hline(
    yintercept = pop_mean,
    color      = "red",
    linetype   = "dashed",
    size       = 1
  ) +
  geom_line(
    data  = cum_mean_df,
    aes(x = draw, y = cum_mean),
    color = "darkgreen",
    size  = 0.9
  ) +
  geom_point(
    data         = cum_mean_df,
    aes(x = draw, y = cum_mean, key = draw),
    showSelected = "draw",
    color        = "orange",
    size         = 5
  ) +
  labs(
    title = "Cumulative Mean converging to population mean",
    x     = "Draw",
    y     = "Cumulative mean"
  ) +
  theme_bw()

psd <- ggplot() +
  geom_tallrect(
    data         = metrics_df,
    aes(xmin = draw - 0.5, xmax = draw + 0.5),
    clickSelects = "draw",
    alpha        = 0.2,
    fill         = "gold"
  ) +
  geom_line(
    data  = metrics_df,
    aes(x = draw, y = sample_sd),
    color = "steelblue",
    size  = 0.9
  ) +
  geom_point(
    data         = metrics_df,
    aes(x = draw, y = sample_sd, key = draw),
    showSelected = "draw",
    color        = "orange",
    size         = 5
  ) +
  labs(
    title = "Sample SD per Draw",
    x     = "Draw",
    y     = "Sample SD"
  ) +
  theme_bw()

viz <- animint(
  scatter  = pscatter,
  means    = pmeans,
  cumplot  = pcum,
  sdplot   = psd,
  first    = list(draw = 1),
  duration = list(draw = 500),
  time     = list(variable = "draw", ms = 1000),
  title    = "Simple Random Sampling",
  source   = "https://github.com/Nishita-shah1/sample-animint"
)

animint2pages(viz, "sample-animint")
print(viz)
