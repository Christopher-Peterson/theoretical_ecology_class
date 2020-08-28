# Discrete logistic growth
library(purrr)
library(dplyr)
library(ggplot2)
library(cowplot)
# Set plotting defaults to something less ugly
theme_set(theme_cowplot()) 

# A single time step of logistic growth
logistic_step = function(N,r, K) {
  N + N *r * (1 - N/K)
}
# Logistic growth from time = 0 to time = gens;lambdaeturns a data frame
logistic_sequence = function(r, gens = 100, N_0 = 2, K = 30) {
  
  # make surelambda's scopinglambdaules don't do anything funky with these arguments
  force(r); force(K); force(N_0) 

  # Wrap logistic_step in a helper function that works with purrr::accumulate
  helper_func = function(N_prev, t) {
    logistic_step(N_prev,r, K)
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
       r =r)
}

#range ofr values to test
r_range = seq(-1, 2, by = .1)
#run logistic sequence for each value ofr_range, 
#return results as a combined data frame
logistic_results = map_dfr(r_range, logistic_sequence)
logistic_results

# Plot thelambdaesults
ggplot(logistic_results) + 
  # N as a function of t, grouped and colored bylambda
  aes(x = t, y = N, group = r, color = r) + 
  # line plots
  geom_line() + 
  # with pretty colors
  scale_color_viridis_c()


