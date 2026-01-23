library(tidyverse)
library(furrr)
library(linkbrl)
source("sim-helpers.R")

reps <- 10
beta <- c(0.01)
p <- 5
n <- c(500, seq(1000, 7000, by = 1000))
prop <- 0.5
L <- 10

set.seed(20)
data <- map(n,
            \(ni) map(1:reps,
                     \(i) sim_graph_brl(n1 = ni,
                                        n2 = ni,
                                        n12 = floor(prop * ni),
                                        p = p,
                                        L = rep(L, p),
                                        params = list("beta" = rep(beta, p),
                                                      "theta" = matrix(rep(1/L, L), nrow = p, ncol = L, byrow = TRUE)))))

# In {linkbrl} functions a warning is issued when a coreference matrix with no links is obtained.
# For timing purposes, warnings are treated as errors and trigger a rerun.
# The reported time therefore corresponds to the final non-trivial run.
plan(multisession(workers = 5))
out <- future_map(list_flatten(data), \(x){
  out_graph <- graph_retry_timed(X1 = x$X1, X2 = x$X2, a_prop = 2, b_prop = 2, a_beta = 1, b_beta = 1, theta_method = "empirical")
  out_fs <- fs_retry_timed(X1 = x$X1, X2 = x$X2, a_prop = 2, b_prop = 2)
  time <- c("graph" = out_graph$time,
  "fs" = out_fs$time)
  metrics <- data.frame("model" = c("graph", "fs"),
                        rbind(binary_metrics(Delta = x$Delta, Delta_hat = out_graph$out$Delta),
                              binary_metrics(Delta = x$Delta, Delta_hat = out_fs$out$Delta)))
  return(list("time" = time,
              "metrics" = metrics))
  },
  .options = furrr_options(seed = 123))


out_time <- split(out, rep(1:length(n), each = reps)) |> 
  map_depth(2, "time") |> 
  map_depth(2, as_tibble_row) |> 
  map(list_rbind) |> 
  map2(.y = n, \(x, y) mutate(x, "n" = y)) |> 
  list_rbind() |> 
  pivot_longer(cols = -n, names_to = "model", values_to = "time") |> 
  mutate(model = factor(model, levels = c("graph", "fs"), labels = c("Graph", "Comp"))) |> 
  group_by(n, model) |> 
  summarise(time_mean = mean(time), time_min = min(time), time_max = max(time), .groups = "drop")

(p_time <- out_time |> 
  ggplot(aes(x = n, y = time_mean, col = model, lty = model))+
  geom_linerange(aes(ymin = time_min, ymax = time_max), alpha = 0.7)+
  geom_line()+
  geom_point()+
  labs(x = expression(n[1]~","~n[2]), y = "Time (seconds)", col = NULL, lty = NULL)+
  theme_bw())

ggsave(plot = p_time, filename = "output/runtime-brl.pdf", width = 6, height = 4)


split(out, rep(1:length(n), each = reps)) |> 
  map_depth(2, "metrics") |> 
  map(list_rbind)
