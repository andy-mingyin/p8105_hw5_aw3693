Homework 5
================
Mingyin Wang
2024-11-13

load packages

## Problem 1

## Problem 2

Function to simulate data and perform a t-test

``` r
simulate_t_test <- function(mu, n = 30, sigma = 5) {
  x <- rnorm(n, mean = mu, sd = sigma)
  t_test_result <- t.test(x, mu = 0)
  tidy(t_test_result)
}
```

Define parameters

``` r
mu_values <- 0:6
num_simulations <- 5000
alpha <- 0.05
```

``` r
# Set up simulation grid and use map to apply the function
simulation_results <- expand_grid(
  mu = mu_values,
  sim = 1:num_simulations
) %>%
  mutate(
    test_result = map(mu, ~simulate_t_test(.x))
  ) %>%
  unnest_wider(test_result)
```

Calculate power for each mu

``` r
power_results <- simulation_results %>%
  group_by(mu) %>%
  summarize(power = mean(p.value < alpha))
```

Plot power vs mu

``` r
ggplot(power_results, aes(x = mu, y = power)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Power vs. True Mean (mu)",
    x = "True Mean (mu)",
    y = "Power (Proportion of Rejections)"
  )
```

![](homework_5_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Calculate average estimate of mu_hat and for rejected samples only

``` r
average_results <- simulation_results %>%
  group_by(mu) %>%
  summarize(
    avg_mu_hat = mean(estimate),
    avg_mu_hat_rejected = mean(estimate[p.value < alpha])
  )
```

Plot average estimate of mu_hat vs true mu

``` r
ggplot(average_results, aes(x = mu)) +
  geom_line(aes(y = avg_mu_hat), color = "blue") +
  geom_point(aes(y = avg_mu_hat), color = "blue") +
  geom_line(aes(y = avg_mu_hat_rejected), color = "red", linetype = "dashed") +
  geom_point(aes(y = avg_mu_hat_rejected), color = "red") +
  labs(
    title = "Average Estimate of Mu_hat vs True Mu",
    x = "True Mean (mu)",
    y = "Average Estimate of Mu_hat"
  ) +
  theme_minimal()
```

![](homework_5_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->