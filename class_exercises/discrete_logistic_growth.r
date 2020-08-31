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
  
  init = tibble::tibble(N = N_0, t = 0, ...)
  # Wrap .func in a helper function that works with purrr::accumulate
  helper_func = function(prev, time) {
    prev$t = time
    prev$N = do.call(.func, prev)
    prev
  }
  
  times = seq_len(gens)
  # accumulate() takes iterates over a function, taking the output 
    # as the first argument of the next iteration
  out = accumulate(times, # iterate over time
                  .f = helper_func, # the function
                  .init = init) # Starting value
  # out is a list of data frames
  dplyr::bind_rows(out)
}

lambda_range = seq(.3, 2, by = .1)

# run logistic sequence for each value of lambda_range, 
# return results as data frame
exp_lambda_results = recursive_sequence(.func = exponential_step,
                                        lambda = lambda_range , gens = 25)
# Create logistic sequence function
logistic_seq = partial(recursive_sequence, .func = logistic_step)
# range of r & K values to test
r_range = c(0, 0.2, 0.5, 1, 1.5, 2, 2.2, 2.6, 3.2)
K_range = 10^seq(0.5, 3.5, by = .25)
logistic_r_results = logistic_seq(r = r_range)
logistic_K_results = logistic_seq(K = K_range)

# Plots
lambda_plot = ggplot(exp_lambda_results) + 
  # N as a function of t, grouped and colored by lambda
  aes(x = t, y = N, group = lambda, color = lambda) + 
  # line plots
  geom_line() + 
  # with pretty colors
  scale_color_viridis_c() + 
  scale_y_log10()

K_plot = ggplot(logistic_K_results) + 
  aes(x = t, y = N, group = K, color = K) + 
  geom_line() + 
  scale_color_viridis_c(trans = "log10") + 
  scale_y_log10()
r_plot = 
  logistic_r_results %>% 
  # r = 3.2 causes problems (e.g., infinite & negative N values)
  filter(!is.infinite(N), N>0) %>% 
  ggplot()+
  aes(x = t, y = N, group = r, color = r) +
  geom_line() +
  scale_color_viridis_c()

lambda_plot
K_plot
r_plot
