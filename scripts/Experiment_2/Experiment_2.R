######## EXPERIMENT 2 #########

#### Package setup ####

# Read packages
pkgs <- c(
  "pak","dplyr","ggplot2",
  "tidyr","purrr",
  "tibble","writexl"
)
lapply(pkgs, library, character.only = TRUE)

# Install signatselect
pak::pkg_install("benmarwick/signatselect")
library(signatselect)

#### PARAMETER SWEEP LISTS ####

set.seed(1234)

# CONTENT BIAS SNAPSHOT --------------------------------------------------------

# Innovation rate
cb_snap_mu_params <- list(
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.025, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.05, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.075, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.1, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.125, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.15, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.175, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.2, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000)
)

# Population size
cb_snap_N_params <- list(
  list(N=10, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000),
  list(N=50, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000),
  list(N=150, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000),
  list(N=200, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000),
  list(N=250, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000),
  list(N=300, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000),
  list(N=350, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000),
  list(N=400, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000),
  list(N=500, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000),
  list(N=600, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000),
  list(N=700, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000),
  list(N=800, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000),
  list(N=900, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000),
  list(N=1000, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000))


# Content bias
cb_snap_b_params <- list(
  list(N=100, mu=0.01, b = 0, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.01, b = 0.05, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.01, b = 0.1, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.01, b = 0.2, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.01, b = 0.25, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.01, b = 0.3, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.01, b = 0.35, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.01, b = 0.4, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000)
)

# Time series
cb_snap_time_params <- list(
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=10, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=150, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=200, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=250, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=300, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=350, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=400, p_value_lvl=0.05, n_runs=1000)
)

# Runs
cb_snap_time_params <- list(
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=10),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=50),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=100),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=500),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=5000),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=10000),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=15000),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=100000)
)



# INTERACTIONS

# mu x b
cb_snap_mu_b <- list(
  list(N=100, mu=0.01, b =0.01, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.05, b =0.05, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.1, b =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.15, b =0.15, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.2, b =0.2, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.25, b =0.25, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.4, b =0.4, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000)
)

# N x b
cb_snap_N_b <- list(
  list(N=10, mu=0.02, b =0.01, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=50, mu=0.02, b =0.05, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.02, b =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=150, mu=0.02, b =0.15, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=200, mu=0.02, b =0.2, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=250, mu=0.02, b =0.25, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=400, mu=0.02, b =0.4, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000)
)

# time x b
cb_snap_t_b <- list(
  list(N=100, mu=0.02, b =0.01, burnin=1000, timesteps=10, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.02, b =0.05, burnin=1000, timesteps=20, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.02, b =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.02, b =0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.02, b =0.2, burnin=1000, timesteps=150, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.02, b =0.25, burnin=1000, timesteps=200, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.02, b =0.4, burnin=1000, timesteps=400, p_value_lvl=0.05, n_runs=1000)
)


# Run and store

# INNOVATION RATE (MU)
cb_snap_mu_results <- map_dfr(cb_snap_mu_params, ~ {
  sim <- do.call(content_bias_snapshot, args = .x)
  tibble(N  = .x$N,
         "µ" = .x$mu,
         b = .x$b,
         "Burn-in" = .x$burnin,
         "Time steps" = .x$timesteps,
         "α" = .x$p_value_lvl,
         SSR = sim$SSR,
         FNR = sim$FNR,
         proportionNA = sim$proportionNA,
         "Runs" = .x$n_runs
  )
})
write_xlsx(cb_snap_mu_results, "tables/cb_snap_output/cb_snap_mu_params.xlsx") # mu


# POPULATION SIZE (N)
cb_snap_N_results <- map_dfr(cb_snap_N_params, ~ {
  sim <- do.call(content_bias_snapshot, args = .x)
  tibble(N  = .x$N,
         "µ" = .x$mu,
         b = .x$b,
         "Burn-in" = .x$burnin,
         "Time steps" = .x$timesteps,
         "α" = .x$p_value_lvl,
         SSR = sim$SSR,
         FNR = sim$FNR,
         proportionNA = sim$proportionNA,
         "Runs" = .x$n_runs
  )
})
write_xlsx(cb_snap_N_results, "tables/cb_snap_output/cb_snap_N_params.xlsx") # N


# CONTENT BIAS (b)
cb_snap_b_results <- map_dfr(cb_snap_b_params, ~ {
  sim <- do.call(content_bias_snapshot, args = .x)
  tibble(N  = .x$N,
         "µ" = .x$mu,
         b = .x$b,
         "Burn-in" = .x$burnin,
         "Time steps" = .x$timesteps,
         "α" = .x$p_value_lvl,
         SSR = sim$SSR,
         FNR = sim$FNR,
         proportionNA = sim$proportionNA,
         "Runs" = .x$n_runs
  )
})
write_xlsx(cb_snap_b_results, "tables/cb_snap_output/cb_snap_b_params.xlsx") # b

# TIME SERIES
cb_snap_time_results <- map_dfr(cb_snap_time_params, ~ {
  sim <- do.call(content_bias_snapshot, args = .x)
  tibble(N  = .x$N,
         "µ" = .x$mu,
         b = .x$b,
         "Burn-in" = .x$burnin,
         "Time steps" = .x$timesteps,
         "α" = .x$p_value_lvl,
         SSR = sim$SSR,
         FNR = sim$FNR,
         proportionNA = sim$proportionNA,
         "Runs" = .x$n_runs
  )
})
write_xlsx(cb_snap_time_params, "tables/cb_snap_output/cb_snap_time_params.xlsx") # time series


# RUNS
cb_snap_runs_results <- map_dfr(cb_snap_runs_params, ~ {
  sim <- do.call(content_bias_snapshot, args = .x)
  tibble(N  = .x$N,
         "µ" = .x$mu,
         b = .x$b,
         "Burn-in" = .x$burnin,
         "Time steps" = .x$timesteps,
         "α" = .x$p_value_lvl,
         SSR = sim$SSR,
         FNR = sim$FNR,
         proportionNA = sim$proportionNA,
         "Runs" = .x$n_runs
  )
})
write_xlsx(cb_snap_runs_params, "tables/cb_snap_output/cb_snap_runs_params.xlsx") # runs

# INTERACTIONS
# mu x b
cb_snap_mu_b <- map_dfr(cb_snap_mu_b, ~ {
  sim <- do.call(content_bias_snapshot, args = .x)
  tibble(N  = .x$N,
         "µ" = .x$mu,
         b = .x$b,
         "Burn-in" = .x$burnin,
         "Time steps" = .x$timesteps,
         "α" = .x$p_value_lvl,
         SSR = sim$SSR,
         FNR = sim$FNR,
         proportionNA = sim$proportionNA,
         "Runs" = .x$n_runs
  )
})
write_xlsx(cb_snap_mu_b, "tables/cb_snap_output/int/cb_snap_mu_b.xlsx") # mu x b


# N x b
cb_snap_N_b <- map_dfr(cb_snap_N_b, ~ {
  sim <- do.call(content_bias_snapshot, args = .x)
  tibble(N  = .x$N,
         "µ" = .x$mu,
         b = .x$b,
         "Burn-in" = .x$burnin,
         "Time steps" = .x$timesteps,
         "α" = .x$p_value_lvl,
         SSR = sim$SSR,
         FNR = sim$FNR,
         proportionNA = sim$proportionNA,
         "Runs" = .x$n_runs
  )
})
write_xlsx(cb_snap_N_b, "tables/cb_snap_output/int/cb_snap_N_b.xlsx") # N x b

# t x b
cb_snap_t_b <- map_dfr(cb_snap_t_b, ~ {
  sim <- do.call(content_bias_snapshot, args = .x)
  tibble(N  = .x$N,
         "µ" = .x$mu,
         b = .x$b,
         "Burn-in" = .x$burnin,
         "Time steps" = .x$timesteps,
         "α" = .x$p_value_lvl,
         SSR = sim$SSR,
         FNR = sim$FNR,
         proportionNA = sim$proportionNA,
         "Runs" = .x$n_runs
  )
})
write_xlsx(cb_snap_t_b, "tables/cb_snap_output/int/cb_snap_t_b.xlsx") # t x b

# PRINT RESULTS
print(cb_snap_mu_results)
print(cb_snap_N_results)
print(cb_snap_b_results)
print(cb_snap_time_results)
print(cb_snap_runs_results)


# CONTENT BIAS TIME AVERAGING --------------------------------------------------

# Innovation rate
cb_ta_mu_params <- list(
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=100, mu=0.025, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=100, mu=0.05, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=100, mu=0.075, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=100, mu=0.1, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=100, mu=0.125, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=100, mu=0.15, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=100, mu=0.175, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=100, mu=0.2, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 10)
)

# Population size
cb_ta_N_params <- list(
  list(N=10, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=50, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=150, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=200, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=250, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=300, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=350, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=400, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 10)
)

# Content bias
cb_ta_b_params <- list(
  list(N=100, mu=0.01, b = 0, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=100, mu=0.01, b = 0.05, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=100, mu=0.01, b = 0.1, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=100, mu=0.01, b = 0.2, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=100, mu=0.01, b = 0.25, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=100, mu=0.01, b = 0.3, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=100, mu=0.01, b = 0.35, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=100, mu=0.01, b = 0.4, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 10)
)

# Time series
cb_ta_time_params <- list(
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=10, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=150, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=200, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=250, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=300, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=350, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=400, p_value_lvl=0.05, n_runs=1000, time_window = 10)
)

# Runs
cb_ta_runs_params <- list(
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=10, time_window = 10),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=50, time_window = 10),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=100, time_window = 10),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=500, time_window = 10),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=5000, time_window = 10),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=10000, time_window = 10),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=15000, time_window = 10),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=100000, time_window = 10))

# Time window size
cb_ta_tw_params <- list(
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000, time_window = 5),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=200, p_value_lvl=0.05, n_runs=1000, time_window = 20),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=400, p_value_lvl=0.05, n_runs=1000, time_window = 40),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=500, p_value_lvl=0.05, n_runs=1000, time_window = 50),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=700, p_value_lvl=0.05, n_runs=1000, time_window = 70),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=800, p_value_lvl=0.05, n_runs=1000, time_window = 80),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=1000, p_value_lvl=0.05, n_runs=1000, time_window = 100),
  list(N=100, mu=0.01, b = 0.15, burnin=1000, timesteps=2000, p_value_lvl=0.05, n_runs=1000, time_window = 200))


# Run and store

# INNOVATION RATE
cb_ta_mu_results <- map_dfr(cb_ta_mu_params, ~ {
  sim <- do.call(content_bias_ta, args = .x)
  tibble(N  = .x$N,
         "µ" = .x$mu,
         b = .x$b,
         "Burn-in" = .x$burnin,
         "Time steps" = .x$timesteps,
         "w" = .x$time_window,
         "α" = .x$p_value_lvl,
         SSR = sim$SSR,
         FNR = sim$FNR,
         proportionNA = sim$proportionNA,
         "Runs" = .x$n_runs
  )
})
write_xlsx(cb_ta_mu_results, "tables/cb_ta_output/cb_ta_mu_params2.xlsx") # mu


# POPULATION SIZE
cb_ta_N_results <- map_dfr(cb_ta_N_params, ~ {
  sim <- do.call(content_bias_ta, args = .x)
  tibble(N  = .x$N,
         "µ" = .x$mu,
         b = .x$b,
         "Burn-in" = .x$burnin,
         "Time steps" = .x$timesteps,
         "w" = .x$time_window,
         "α" = .x$p_value_lvl,
         SSR = sim$SSR,
         FNR = sim$FNR,
         proportionNA = sim$proportionNA,
         "Runs" = .x$n_runs
  )
})
write_xlsx(cb_ta_N_results, "tables/cb_ta_output/cb_ta_N_params2.xlsx") # N


# CONTENT BIAS
cb_ta_b_results <- map_dfr(cb_ta_b_params, ~ {
  sim <- do.call(content_bias_ta, args = .x)
  tibble(N  = .x$N,
         "µ" = .x$mu,
         b = .x$b,
         "Burn-in" = .x$burnin,
         "Time steps" = .x$timesteps,
         "w" = .x$time_window,
         "α" = .x$p_value_lvl,
         SSR = sim$SSR,
         FNR = sim$FNR,
         proportionNA = sim$proportionNA,
         "Runs" = .x$n_runs
  )
})
write_xlsx(cb_ta_b_results, "tables/cb_ta_output/cb_ta_b_params2.xlsx") # b


# TIME SERIES LENGTH
cb_ta_time_results <- map_dfr(cb_ta_time_params, ~ {
  sim <- do.call(content_bias_ta, args = .x)
  tibble(N  = .x$N,
         "µ" = .x$mu,
         b = .x$b,
         "Burn-in" = .x$burnin,
         "Time steps" = .x$timesteps,
         "w" = .x$time_window,
         "α" = .x$p_value_lvl,
         SSR = sim$SSR,
         FNR = sim$FNR,
         proportionNA = sim$proportionNA,
         "Runs" = .x$n_runs
  )
})
write_xlsx(cb_ta_time_results, "tables/cb_ta_output/cb_ta_time_params2.xlsx") # time series


# RUNS
cb_ta_runs_results <- map_dfr(cb_ta_runs_params, ~ {
  sim <- do.call(content_bias_ta, args = .x)
  tibble(N  = .x$N,
         "µ" = .x$mu,
         b = .x$b,
         "Burn-in" = .x$burnin,
         "Time steps" = .x$timesteps,
         "w" = .x$time_window,
         "α" = .x$p_value_lvl,
         SSR = sim$SSR,
         FNR = sim$FNR,
         proportionNA = sim$proportionNA,
         "Runs" = .x$n_runs
  )
})
write_xlsx(cb_ta_runs_results, "tables/cb_ta_output/cb_ta_runs_params2.xlsx") # runs


# TIME WINDOW SIZE
cb_ta_tw_results <- map_dfr(cb_ta_tw_params, ~ {
  sim <- do.call(content_bias_ta, args = .x)
  tibble(N  = .x$N,
         "µ" = .x$mu,
         b = .x$b,
         "Burn-in" = .x$burnin,
         "Time steps" = .x$timesteps,
         "w" = .x$time_window,
         "α" = .x$p_value_lvl,
         SSR = sim$SSR,
         FNR = sim$FNR,
         proportionNA = sim$proportionNA,
         "Runs" = .x$n_runs
  )
})
write_xlsx(cb_ta_tw_results, "tables/cb_ta_output/cb_ta_tw_params2.xlsx") # time window size


print(cb_ta_mu_results)
print(cb_ta_N_results)
print(cb_ta_b_results)
print(cb_ta_time_results)
print(cb_ta_runs_results)
print(cb_ta_tw_results)

# CONFORMIST BIAS SNAPSHOT -----------------------------------------------------

# N
conf_snap_N <- list(
  list(N=10, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=50, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=150, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=200, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=250, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=300, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=350, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=400, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=500, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=600, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=700, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=800, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=900, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=1000, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000)
)

# mu
conf_snap_mu <- list(
  list(N=100, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.025, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.05, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.075, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.1, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.125, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.15, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.175, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.2, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000)
)

# c
conf_snap_c <- list(
  list(N=100, mu=0.01, c =0.01, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.01, c =0.05, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.01, c =0.15, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.01, c =0.2, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.01, c =0.25, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.01, c =0.3, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.01, c =0.35, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.01, c =0.4, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000)
)

# t
conf_snap_t <- list(
  list(N=100, mu=0.01, c =0.1, burnin=1000, timesteps=10, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.01, c =0.1, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.01, c =0.1, burnin=1000, timesteps=150, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.01, c =0.1, burnin=1000, timesteps=200, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.01, c =0.1, burnin=1000, timesteps=250, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.01, c =0.1, burnin=1000, timesteps=300, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.01, c =0.1, burnin=1000, timesteps=350, p_value_lvl=0.05, n_runs=1000),
  list(N=100, mu=0.01, c =0.01, burnin=1000, timesteps=400, p_value_lvl=0.05, n_runs=1000)
)


# Run and store in a spreadsheet

# N
conf_snap_N_results <- map_dfr(conf_snap_N, ~ {
  sim <- do.call(conformist_bias_snapshot, args = .x)
  tibble(N  = .x$N,
         mu = .x$mu,
         c = .x$c,
         "Burn-in" = .x$burnin,
         "Time steps" = .x$timesteps,
         alpha = .x$p_value_lvl,
         SSR = sim$SSR,
         FNR = sim$FNR,
         proportionNA = sim$proportionNA,
         "Runs" = .x$n_runs
  )
})
write_xlsx(conf_snap_N_results, "data/conf_snap_output/conf_snap_N.xlsx") # N


# mu
conf_snap_mu_results <- map_dfr(conf_snap_mu, ~ {
  sim <- do.call(conformist_bias_snapshot, args = .x)
  tibble(N  = .x$N,
         "µ" = .x$mu,
         c = .x$c,
         "Burn-in" = .x$burnin,
         "Time steps" = .x$timesteps,
         "α" = .x$p_value_lvl,
         SSR = sim$SSR,
         FNR = sim$FNR,
         proportionNA = sim$proportionNA,
         "Runs" = .x$n_runs
  )
})
write_xlsx(conf_snap_mu_results, "data/conf_snap_output/conf_snap_mu.xlsx") # mu

# c
conf_snap_c_results <- map_dfr(conf_snap_c, ~ {
  sim <- do.call(conformist_bias_snapshot, args = .x)
  tibble(N  = .x$N,
         "µ" = .x$mu,
         c = .x$c,
         "Burn-in" = .x$burnin,
         "Time steps" = .x$timesteps,
         "α" = .x$p_value_lvl,
         SSR = sim$SSR,
         FNR = sim$FNR,
         proportionNA = sim$proportionNA,
         "Runs" = .x$n_runs
  )
})
write_xlsx(conf_snap_c_results, "data/conf_snap_output/conf_snap_c.xlsx") # c


# t
conf_snap_t_results <- map_dfr(conf_snap_t, ~ {
  sim <- do.call(conformist_bias_snapshot, args = .x)
  tibble(N  = .x$N,
         "µ" = .x$mu,
         c = .x$c,
         "Burn-in" = .x$burnin,
         "Time steps" = .x$timesteps,
         "α" = .x$p_value_lvl,
         SSR = sim$SSR,
         FNR = sim$FNR,
         proportionNA = sim$proportionNA,
         "Runs" = .x$n_runs
  )
})
write_xlsx(conf_snap_t_results, "data/conf_snap_output/conf_snap_t.xlsx") # t


# CONFORMIST BIAS TIME AVERAGING -----------------------------------------------

# N
conf_ta_N <- list(
  list(N=10, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=100, time_window = 5),
  list(N=50, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=100, time_window = 5),
  list(N=100, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=100, time_window = 5),
  list(N=150, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=100, time_window = 5),
  list(N=200, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=100, time_window = 5),
  list(N=250, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=100, time_window = 5),
  list(N=300, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=100, time_window = 5),
  list(N=350, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=100, time_window = 5),
  list(N=400, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=100, time_window = 5)
)

conf_ta_N_2 <- list(
  list(N=10, mu=0.01, c =0.05, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=250, time_window = 5),
  list(N=50, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=250, time_window = 5),
  list(N=100, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=250, time_window = 5),
  list(N=150, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=250, time_window = 5),
  list(N=200, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=250, time_window = 5),
  list(N=250, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=250, time_window = 5),
  list(N=300, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=250, time_window = 5),
  list(N=350, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=250, time_window = 5),
  list(N=400, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=250, time_window = 5),
  list(N=500, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=250, time_window = 5),
  list(N=600, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=250, time_window = 5),
  list(N=700, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=250, time_window = 5),
  list(N=800, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=250, time_window = 5),
  list(N=900, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=250, time_window = 5),
  list(N=1000, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=250, time_window = 5)
)

# mu
conf_ta_mu <- list(
  list(N=100, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000, time_window = 5),
  list(N=100, mu=0.025, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000, time_window = 5),
  list(N=100, mu=0.05, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000, time_window = 5),
  list(N=100, mu=0.075, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000, time_window = 5),
  list(N=100, mu=0.1, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000, time_window = 5),
  list(N=100, mu=0.125, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000, time_window = 5),
  list(N=100, mu=0.15, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000, time_window = 5),
  list(N=100, mu=0.175, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000, time_window = 5),
  list(N=100, mu=0.2, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000, time_window = 5)
)

# c
conf_ta_c <- list(
  list(N=100, mu=0.01, c =0.01, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000, time_window = 5),
  list(N=100, mu=0.01, c =0.05, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000, time_window = 5),
  list(N=100, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000, time_window = 5),
  list(N=100, mu=0.01, c =0.15, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000, time_window = 5),
  list(N=100, mu=0.01, c =0.2, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000, time_window = 5),
  list(N=100, mu=0.01, c =0.25, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000, time_window = 5),
  list(N=100, mu=0.01, c =0.3, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000, time_window = 5),
  list(N=100, mu=0.01, c =0.35, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000, time_window = 5),
  list(N=100, mu=0.01, c =0.4, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000, time_window = 5)
)

# t
conf_ta_t <- list(
  list(N=100, mu=0.01, c =0.1, burnin=1000, timesteps=10, p_value_lvl=0.05, n_runs=1000, time_window = 5),
  list(N=100, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000, time_window = 5),
  list(N=100, mu=0.01, c =0.1, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 5),
  list(N=100, mu=0.01, c =0.1, burnin=1000, timesteps=150, p_value_lvl=0.05, n_runs=1000, time_window = 5),
  list(N=100, mu=0.01, c =0.1, burnin=1000, timesteps=200, p_value_lvl=0.05, n_runs=1000, time_window = 5),
  list(N=100, mu=0.01, c =0.1, burnin=1000, timesteps=250, p_value_lvl=0.05, n_runs=1000, time_window = 5),
  list(N=100, mu=0.01, c =0.1, burnin=1000, timesteps=300, p_value_lvl=0.05, n_runs=1000, time_window = 5),
  list(N=100, mu=0.01, c =0.1, burnin=1000, timesteps=350, p_value_lvl=0.05, n_runs=1000, time_window = 5),
  list(N=100, mu=0.01, c =0.1, burnin=1000, timesteps=400, p_value_lvl=0.05, n_runs=1000, time_window = 5)
)

# tw
conf_ta_tw <- list(
  list(N=100, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000, time_window = 1),
  list(N=100, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000, time_window = 5),
  list(N=100, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=100, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000, time_window = 25),
  list(N=100, mu=0.01, c =0.1, burnin=1000, timesteps=50, p_value_lvl=0.05, n_runs=1000, time_window = 1),
  list(N=100, mu=0.01, c =0.1, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 2),
  list(N=100, mu=0.01, c =0.1, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 5),
  list(N=100, mu=0.01, c =0.1, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 10),
  list(N=100, mu=0.01, c =0.1, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 25),
  list(N=100, mu=0.01, c =0.1, burnin=1000, timesteps=100, p_value_lvl=0.05, n_runs=1000, time_window = 50)
)


# Run and store

# N
conf_ta_N_results <- map_dfr(conf_ta_N, ~ {
  sim <- do.call(conformist_bias_ta, args = .x)
  tibble(N  = .x$N,
         "µ" = .x$mu,
         c =.x$c,
         "Burn-in" = .x$burnin,
         "Time steps" = .x$timesteps,
         "w" = .x$time_window,
         "α" = .x$p_value_lvl,
         SSR = sim$SSR,
         FNR = sim$FNR,
         proportionNA = sim$proportionNA,
         "Runs" = .x$n_runs
  )
})
write_xlsx(conf_ta_N_results, "data/conf_ta_output/conf_ta_N.xlsx") # N

conf_ta_N_results_2 <- map_dfr(conf_ta_N_2, ~ {
  sim <- do.call(conformist_bias_ta, args = .x)
  tibble(N  = .x$N,
         "µ" = .x$mu,
         c =.x$c,
         "Burn-in" = .x$burnin,
         "Time steps" = .x$timesteps,
         "w" = .x$time_window,
         "α" = .x$p_value_lvl,
         SSR = sim$SSR,
         FNR = sim$FNR,
         proportionNA = sim$proportionNA,
         "Runs" = .x$n_runs
  )
})
write_xlsx(conf_ta_N_results_2, "data/conf_ta_output/conf_ta_N_2.xlsx") # N


# mu
conf_ta_mu_results <- map_dfr(conf_ta_mu, ~ {
  sim <- do.call(conformist_bias_ta, args = .x)
  tibble(N  = .x$N,
         "µ" = .x$mu,
         c =.x$c,
         "Burn-in" = .x$burnin,
         "Time steps" = .x$timesteps,
         "w" = .x$time_window,
         "α" = .x$p_value_lvl,
         SSR = sim$SSR,
         FNR = sim$FNR,
         proportionNA = sim$proportionNA,
         "Runs" = .x$n_runs
  )
})
write_xlsx(conf_ta_mu_results, "data/conf_ta_output/conf_ta_mu.xlsx") # mu


# c
conf_ta_c_results <- map_dfr(conf_ta_c, ~ {
  sim <- do.call(conformist_bias_ta, args = .x)
  tibble(N  = .x$N,
         "µ" = .x$mu,
         c = .x$c,
         "Burn-in" = .x$burnin,
         "Time steps" = .x$timesteps,
         "w" = .x$time_window,
         "α" = .x$p_value_lvl,
         SSR = sim$SSR,
         FNR = sim$FNR,
         proportionNA = sim$proportionNA,
         "Runs" = .x$n_runs
  )
})
write_xlsx(conf_ta_c_results, "data/conf_ta_output/conf_ta_c.xlsx") # c


# t
conf_ta_t_results <- map_dfr(conf_ta_t, ~ {
  sim <- do.call(conformist_bias_ta, args = .x)
  tibble(N  = .x$N,
         "µ" = .x$mu,
         c =.x$c,
         "Burn-in" = .x$burnin,
         "Time steps" = .x$timesteps,
         "w" = .x$time_window,
         "α" = .x$p_value_lvl,
         SSR = sim$SSR,
         FNR = sim$FNR,
         proportionNA = sim$proportionNA,
         "Runs" = .x$n_runs
  )
})
write_xlsx(conf_ta_t_results, "data/conf_ta_output/conf_ta_t.xlsx") # t

# tw
conf_ta_tw_results <- map_dfr(conf_ta_tw, ~ {
  sim <- do.call(conformist_bias_ta, args = .x)
  tibble(N  = .x$N,
         "µ" = .x$mu,
         c =.x$c,
         "Burn-in" = .x$burnin,
         "Time steps" = .x$timesteps,
         "w" = .x$time_window,
         "α" = .x$p_value_lvl,
         SSR = sim$SSR,
         FNR = sim$FNR,
         proportionNA = sim$proportionNA,
         "Runs" = .x$n_runs
  )
})
write_xlsx(conf_ta_tw_results, "data/conf_ta_output/conf_ta_tw.xlsx") # tw

