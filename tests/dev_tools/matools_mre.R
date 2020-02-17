# matools_mre.R
# minimal reproducible example (MRE)

library(dplyr)
library(ggplot2)
library(tibble)
library(matools)

#### single real experiment values ####
test_file_path <- system.file("extdata/demo_data",
  "cell1.ASC",
  package = "matools",
  mustWork = TRUE
)
test_param_path <- system.file("extdata/demo_data",
  "demo_data_project_parameters.csv",
  package = "matools",
  mustWork = TRUE
)

test_df <- asc_to_tibble(file_path = test_file_path, merge_iei = FALSE)

test_df %>%
  top_n(100) %>%
  summary()

test_df %>%
  top_n(100) %>%
  summarise_all(~ mean(.x, na.rm = TRUE))

test_df %>%
  top_n(100) %>%
  summarise_all(~ sd(.x, na.rm = TRUE))

ggplot(test_df %>% top_n(100), aes(decay_ms)) +
  geom_histogram(binwidth = 0.25)

#### create toy example values ####
n <- 100
matools_mre_data <- tibble(
  ma_row = 1:n,
  rec_time_ms = seq(1400.50, (1400.50 * n), length.out = n),
  amplitude = stats::runif(n, 100, 200),
  rise_ms = stats::rnorm(n, 0.40, 0.15),
  decay_ms = stats::rbeta(n, 0.05, 0.50),
  area = stats::runif(n, 10, 110),
  baseline = stats::runif(n, -80, 10),
  noise = stats::runif(n, -10, 10),
  group = rep(0, n),
  channel = rep(0, n),
  x10_90_rise = stats::rnorm(n, 0.15, 0.15),
  halfwidth = stats::rnorm(n, 0.50, 1),
  rise50 = stats::rnorm(n, 0.30, 0.15),
  peak_direction = rep(-1, n),
  burst = rep(0, n),
  burste = rep(0, n),
  x10_90slope = rep(0, n),
  rel_time = rep(0, n)
)

matools_mre_data %>%
  top_n(100) %>%
  summary()
matools_mre_data %>%
  top_n(100) %>%
  summarise_all(~ mean(.x, na.rm = TRUE))
matools_mre_data %>%
  top_n(100) %>%
  summarise_all(~ sd(.x, na.rm = TRUE))

ggplot(matools_mre_data, aes(decay_ms)) +
  geom_histogram(binwidth = 0.25)
