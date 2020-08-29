# Discrete logistic growth
library(purrr)
library(dplyr)
library(ggplot2)
library(cowplot)
# Set plotting defaults to something less ugly
theme_set(theme_cowplot()) 

# A single time step of logistic growth
logistic_step = function(N, r = 0.2, K = 30, ...) {
  N + N *r * (1 - N/K)
}
exponential_step = function(N, lambda = 1.2, ...) {
  N * lambda
}
# Run one of these step functions for a recursive sequence; 
# define parameters with ...
recursive_sequence = function(.func, gens = 75, N_0 = 2, ...) {

  # Wrap .func in a helper function that works with purrr::accumulate
  helper_func = function(N_prev, t) {
    .func(N_prev,...)
  }
  
  times = seq_len(gens)
  # accumulate() takes iterates over a function, taking the output 
    # as the first argument of the next iteration
  Ns = accumulate(times, # iterate over time
                  .f = helper_func, # the function
                  .init = N_0) # Starting value
  # Put output in a data frame
  tibble(N = Ns, 
         t = c(0, times), # times doesn't include a spot for the initial value of 0
         ...)
}

lambda_range = seq(.3, 2, by = .1)

# run logistic sequence for each value of lambda_range, 
# return results as data frame
exp_lambda_results = map_dfr(lambda_range, 
        ~recursive_sequence(.func = exponential_step, lambda = .x, , gens = 25))


# range of r values to test
r_range = seq(-1, 2, by = .1)
logistic_r_results = map_dfr(r_range,
                             ~recursive_sequence(.func = logistic_step,
                                                 r = .x))
K_range = 10^seq(0.5, 3.5, by = .25)
logistic_k_results = map_dfr(K_range,
                             ~recursive_sequence(.func = logistic_step,
                                                 K = .x))

# Plots
lambda_plot = ggplot(exp_lambda_results) + 
  # N as a function of t, grouped and colored by lambda
  aes(x = t, y = N, group = lambda, color = lambda) + 
  # line plots
  geom_line() + 
  # with pretty colors
  scale_color_viridis_c() + 
  scale_y_log10()

K_plot = ggplot(logistic_k_results) + 
  aes(x = t, y = N, group = K, color = K) + 
  geom_line() + 
  scale_color_viridis_c(trans = "log10") + 
  scale_y_log10()
r_plot = ggplot(logistic_r_results) + 
  aes(x = t, y = N, group = r, color = r) + 
  geom_line() + 
  scale_color_viridis_c()
  
