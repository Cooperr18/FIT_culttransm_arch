#########################################################
############## REPRODUCTION OF FIGURES ##################
#########################################################


# INITIALIZE PACKAGES

pkgs <- c(
  "dplyr","ggplot2",
  "tidyr","gridExtra","purrr",
  "tibble","writexl", "tidyverse",
  "viridis", "stringr", "viridisLite",
  "readxl", "patchwork", "scales"
)
lapply(pkgs, library, character.only = TRUE)

library(pak) # To install package from GitHub repo

pak::pkg_install("benmarwick/signatselect") # Install SST
library(signatselect)

set.seed(1234)

##########################################
############## FIGURE 1 ##################
##########################################

# Call the unbiased transmission model into a function

neutral_snapshot <- function(N, mu, timesteps, n_runs) {
  runs <- vector("list", length = n_runs)
  
  for (run in 1:n_runs) {
    # Initialize population: 33% start with variant 1, others have unique variants
    initial_variant_count <- round(N * 0.33)
    pop <- c(rep(1, initial_variant_count), 2:(N - initial_variant_count + 1))
    
    traitmatrix <- matrix(NA, nrow = timesteps, ncol = N)
    maxtrait <- max(pop)
    
    for (i in 1:timesteps) {
      pop <- sample(pop, replace = TRUE)  # Neutral drift
      innovate <- which(runif(N) < mu)
      if (length(innovate) > 0) {
        new_variants <- (maxtrait + 1):(maxtrait + length(innovate))
        pop[innovate] <- new_variants
        maxtrait <- max(pop)
      }
      traitmatrix[i, ] <- pop
    }
    
    # Track ONLY variant 1 (initial frequency = 0.33)
    freq_variant1 <- rowMeans(traitmatrix == 1, na.rm = TRUE)
    
    runs[[run]] <- data.frame(
      time = 1:timesteps,
      freq = freq_variant1,
      run = run,
      N = N,
      mu = mu,
      variant = "Variant 1 (Initial freq = 0.33)"
    )
  }
  bind_rows(runs)
}

# Output
n_snap_N_params <- list(
  list(N=10, mu=0,timesteps=200, n_runs=4),
  list(N=50, mu=0,timesteps=200, n_runs=4),
  list(N=100, mu=0,timesteps=200, n_runs=4),
  list(N=250, mu=0,timesteps=200, n_runs=4),
  list(N=500, mu=0,timesteps=200, n_runs=4),
  list(N=1000, mu=0,timesteps=200, n_runs=4)
)

# Run all simulations and bind into one big tibble
N_all_results <- map_dfr(n_snap_N_params, ~
                         neutral_snapshot(.x$N, .x$mu, .x$timesteps, 
                                          .x$n_runs))

# change the label so that each panel shows "N = "
N_all_results <- N_all_results %>%
  mutate(N = factor(paste("N =", N), 
                    levels = paste("N =", sort(unique(N)))))

head(N_all_results)

# And plot:
ggplot(N_all_results, aes(x = time, y = freq, color = factor(run))) +
  geom_line(size = 0.8, alpha = 0.8) +
  geom_hline(yintercept = 0.33, linetype = "dashed", color = "gray30", size = 0.8) +
  facet_wrap(~ N, scales = "fixed", ncol = 3) +
  scale_color_brewer(palette = "Set1", name = "Run") +
  labs(
    x = "Time Step",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 20) +  # Base font size increased to 20
  theme(
    axis.title.x = element_text(size = 22, margin = margin(t = 10)), # Larger X-axis label
    axis.title.y = element_text(size = 22, margin = margin(r = 10)),  # Larger Y-axis label
    axis.text = element_text(size = 18),  # Larger tick labels
    strip.text = element_text(size = 20),  # Larger facet labels
    legend.title = element_text(size = 20),  # Larger legend title
    legend.text = element_text(size = 18),  # Larger legend labels
    legend.position = "bottom",
    panel.spacing = unit(1.5, "lines")  # Extra space between facets
  )


##########################################
############## FIGURE 2 ##################
##########################################


# Now call the model with a higher initial frequency, but with varying innovation rates

neutral_snapshot_mu <- function(N, mu, timesteps, n_runs) {
  runs <- vector("list", length = n_runs)
  
  for (run in 1:n_runs) {
    # Initialize population: 75% start with variant 1, others have unique variants
    initial_variant_count <- round(N * 0.75)
    pop <- c(rep(1, initial_variant_count), 2:(N - initial_variant_count + 1))
    
    traitmatrix <- matrix(NA, nrow = timesteps, ncol = N)
    maxtrait <- max(pop)
    
    for (i in 1:timesteps) {
      pop <- sample(pop, replace = TRUE)  # Neutral drift
      innovate <- which(runif(N) < mu)
      if (length(innovate) > 0) {
        new_variants <- (maxtrait + 1):(maxtrait + length(innovate))
        pop[innovate] <- new_variants
        maxtrait <- max(pop)
      }
      traitmatrix[i, ] <- pop
    }
    
    # Track ONLY variant 1 (initial frequency = 0.75)
    freq_variant1 <- rowMeans(traitmatrix == 1, na.rm = TRUE)
    
    runs[[run]] <- data.frame(
      time = 1:timesteps,
      freq = freq_variant1,
      run = run,
      N = N,
      mu = mu,
      variant = "Variant 1 (Initial freq = 0.75)"
    )
  }
  bind_rows(runs)
}


# Output
n_snap_mu_params <- list(
  list(N=1000, mu=0.05,timesteps=200, n_runs=4),
  list(N=1000, mu=0.1,timesteps=200, n_runs=4),
  list(N=1000, mu=0.25,timesteps=200, n_runs=4),
  list(N=1000, mu=0.5,timesteps=200, n_runs=4),
  list(N=1000, mu=0.75,timesteps=200, n_runs=4),
  list(N=1000, mu=1,timesteps=200, n_runs=4)
)

# Run all simulations and bind into one big tibble
all_results <- map_dfr(n_snap_mu_params, ~
                         neutral_snapshot_mu(.x$N, .x$mu, .x$timesteps, 
                                          .x$n_runs))
# change the label so that R detects the mathematical expression "mu"
all_results$mu <- paste("mu ==", all_results$mu) 

head(all_results)

# And plot:
ggplot(all_results, aes(x = time, y = freq, color = factor(run))) +
  geom_line(size = 0.8, alpha = 0.8) +
  geom_hline(yintercept = 0.75, linetype = "dashed", color = "gray30", size = 0.8) +
  facet_wrap(~ mu, scales = "fixed", ncol = 3, labeller = label_parsed) + # Facet by innovation rate
  scale_color_brewer(palette = "Set1", name = "Run") +
  labs(
    x = "Time Step",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 20) +  # Base font size increased to 20
  theme(
    axis.title.x = element_text(size = 22, margin = margin(t = 10)),  # Larger X-axis label
    axis.title.y = element_text(size = 22, margin = margin(r = 10)),  # Larger Y-axis label
    axis.text = element_text(size = 18),  # Larger tick labels
    strip.text = element_text(size = 20, face = "bold"),  # Larger facet labels
    legend.title = element_text(size = 20),  # Larger legend title
    legend.text = element_text(size = 18),  # Larger legend labels
    legend.position = "bottom",
    panel.spacing = unit(1.5, "lines")  # Extra space between facets
  )


##########################################
############## FIGURE 3a #################
##########################################

# Build a data.frame of x, y and all b‐values
# Content adoption curves
df <- tibble(x = 1:100) %>%
  mutate(x_scaled = x/100, y = 100 - x) %>%
  crossing(b = seq(0.2, 1.2, by = 0.2)) %>%
  mutate(p = x*(1 + b) / (x*(1 + b) + y))

df_neutral <- tibble(x_scaled = seq(0, 1, length.out = 100), 
                     p = seq(0, 1, length.out = 100))

content_plot <- ggplot(df, aes(x = x_scaled, y = p, color = as.factor(b))) +
  geom_line(linewidth = 1) +
  geom_line(data = df_neutral,
            aes(x = x_scaled, y = p),
            linetype = "dashed",
            color = "black",
            linewidth = 1) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_color_viridis_d(name = "b") +
  labs(x = "Frequency of Variant",
       y = "Adoption probability") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top",
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        panel.grid.minor = element_blank())

##########################################
############## FIGURE 3b #################
##########################################

# Conformist adoption curves
dff <- tibble(x = 1:100) %>%
  mutate(x_scaled = x/100, y = 100 - x) %>%
  crossing(b = seq(0.2, 1.2, by = 0.2)) %>%
  mutate(p = x^(1 + b) / ( x^(1 + b) + y ))

# Separate neutral for dashed line
df_neutral <- tibble(x_scaled = seq(0, 1, length.out = 100), 
                     p = seq(0, 1, length.out = 100))
# Plot
conformist_plot <- ggplot(dff, aes(x = x_scaled, y = p, color = as.factor(b))) +
  # biased curves
  geom_line(size = 1) +
  # neutral curve
  geom_line(data = df_neutral, aes(x = x_scaled, y = p),
            linetype = "dashed", color = "black", size = 1) +
  # scales & labels
  scale_color_viridis_d(name = "c",
                        option = "D") +
  labs(
    x = "Frequency of variant",
    y = "Adoption probability") +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.position  = "top"
  )


# Arrange both plots into a single one

library(patchwork)

prob_adopt <- content_plot + conformist_plot +
  plot_layout(axes = "collect", # remove duplicate axis tick labels
              axis_titles = "collect") # merge identical axis titles into one
prob_adopt


##########################################
############## FIGURE 4 ##################
##########################################

neutral_ta_fig <- function(N, mu, burnin, timesteps, n_variants = 2, n_runs,
                           time_window = 20) {
  
  all_runs <- vector("list", n_runs)
  all_runs_raw <- vector("list", n_runs)  # storage for raw trajectories
  
  for (run in 1:n_runs) {
    ini <- 1:n_variants # initial variants
    traitmatrix <- matrix(NA, nrow = timesteps, ncol = N)
    raw_rows <- vector("list", timesteps)  # storage for raw timestep rows
    
    # Create initial population with EXACT equal frequencies
    pop <- rep(ini, each = N/n_variants)[1:N] # Perfect 0.5 each
    
    # Burn-in stage - no recording
    for(i in 1:burnin) {
      # Neutral transmission with frequency correction
      current_freqs <- tabulate(pop, nbins = n_variants)/N
      weights <- (1/current_freqs)[pop] # Inverse frequency weighting
      pop <- sample(pop, size = N, replace = TRUE, prob = weights)
      
      # Innovation with equal probability for all variants
      innovate <- which(runif(N) < mu)
      if(length(innovate) > 0) {
        pop[innovate] <- sample(ini, length(innovate), 
                                replace = TRUE, 
                                prob = rep(0.5, n_variants))
      }
    }
    
    # Observation period after equilibrium
    for (t in 2:timesteps) {
      # Frequency-corrected sampling
      current_freqs <- tabulate(pop, nbins = n_variants)/N
      weights <- (1/current_freqs)[pop]
      pop <- sample(pop, size = N, replace = TRUE, prob = weights)
      
      # Balanced innovation
      innovate <- which(runif(N) < mu)
      if(length(innovate) > 0) {
        pop[innovate] <- sample(ini, length(innovate), 
                                replace = TRUE,
                                prob = rep(0.5, n_variants))
      }
      
      traitmatrix[t,] <- pop # record the variants
      
      # Record raw frequency at each timestep
      tab <- tabulate(pop, nbins = n_variants) / N
      raw_rows[[t]] <- data.frame(
        timestep = t,
        run      = run,
        variant  = as.character(ini),
        frequency = tab
      )
    }
    
    # Collapse raw rows into a single data frame for this run
    all_runs_raw[[run]] <- bind_rows(raw_rows)
    
    # Time averaging step
    n_bins <- floor(timesteps / time_window)
    bin_labels <- 1:n_bins  # Integer bin numbers
    
    averaged_samples <- lapply(seq_len(n_bins), function(j) {
      rows <- ((j - 1) * time_window + 1):(j * time_window)
      as.vector(traitmatrix[rows, ])
    })
    
    unique_variants <- sort(unique(unlist(averaged_samples)))
    freq_mat <- t(sapply(averaged_samples, function(x) {
      tab <- table(factor(x, levels = ini))  # Track only original variants
      as.numeric(tab) / (N * time_window)
    }))
    colnames(freq_mat) <- as.character(ini)
    
    # Convert to long format with integer bins
    df_long <- as.data.frame(freq_mat) %>%
      mutate(bin = bin_labels,  # Use integer sequence
             run = run) %>%
      pivot_longer(cols = -c(bin, run),
                   names_to = "variant",
                   values_to = "frequency")
    
    all_runs[[run]] <- df_long
  }
  
  # Return a named list with both data frames
  list(
    binned = bind_rows(all_runs),
    raw    = bind_rows(all_runs_raw)
  )
}

neutral_ta_traj <- neutral_ta_fig(N = 100, mu = 0.05, burnin = 200,
                                  timesteps = 200, 
                                  n_runs = 1, time_window = 20)

time_averaging <- ggplot() +
  # Grey/white bin rectangles behind the lines
  geom_rect(data = data.frame(
    xmin = seq(1, floor(max(neutral_ta_traj$raw$timestep) / 20) * 20, by = 20),
    xmax = seq(20, floor(max(neutral_ta_traj$raw$timestep) / 20) * 20, by = 20)
  ) %>% 
    mutate(fill = rep(c("grey90", "white"), length.out = n())),
    aes(xmin = xmin, xmax = xmax, ymin = 0.3, ymax = 0.7, fill = fill),
    inherit.aes = FALSE) +
  scale_fill_identity() +
  # Raw timestep trajectories
  geom_line(data = neutral_ta_traj$raw,
            aes(x = timestep, y = frequency, group = interaction(variant, run),
                colour = "Snapshot"),
            linewidth = 0.6) +
  # Smoothed binned trajectories overlaid
  geom_line(data = neutral_ta_traj$binned %>%
              mutate(timestep = (bin - 0.5) * 20),  # map bin to midpoint
            aes(x = timestep, y = frequency, group = interaction(variant, run),
                colour = "Time averaged"),
            linewidth = 1.4) +  # thicker 
  scale_colour_manual(values = c("Snapshot" = "black", 
                                 "Time averaged" = "steelblue"),
                      name = NULL) + 
  scale_x_continuous(breaks = scales::pretty_breaks(),
                     labels = scales::number_format(accuracy = 1)) +
  ylim(c(0.3, 0.7)) +
  theme_minimal() +
  theme(axis.title = element_text(size = 18),
        strip.text = element_text(size = 16),
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.position = "top") +
  labs(x = "Time steps (t)", y = "Frequency") + 
  facet_wrap(~ variant, ncol = 2)

time_averaging

##########################################
############## FIGURE 5 ##################
##########################################



