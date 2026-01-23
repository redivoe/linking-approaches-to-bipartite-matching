library(tidyverse)
library(furrr)
library(linkbrl)
source("sim-helpers.R")

reps <- 100
beta <- c(0.01, 0.05, 0.1)
p <- c(5, 7)
n <- list(c(1000, 1000))
prop <- c(0.1, 0.5, 0.9)
L <- 10
settings <- cross(list("n" = n, "p" = p, "beta" = beta, "prop" = prop, "L" = L))

set.seed(1)
data <- map(settings,
            \(x) map(1:reps,
                     \(i) sim_graph_brl(n1 = x$n[1],
                                        n2 = x$n[2],
                                        n12 = floor(x$prop * x$n[2]),
                                        p = x$p,
                                        L = rep(x$L, x$p),
                                        params = list("beta" = rep(x$beta, x$p),
                                                      "theta" = matrix(rep(1/x$L, x$L), nrow = x$p, ncol = x$L, byrow = TRUE)))))

plan(multisession(workers = 5))
out <- future_map(list_flatten(data), \(x){
  out_graph <- timed_fun(brl_cem)(X1 = x$X1, X2 = x$X2, model = "graph", reps = 5, a_prop = 2, b_prop = 2, a_beta = 1, b_beta = 1, theta_method = "empirical")
  out_fs <- timed_fun(brl_cem)(X1 = x$X1, X2 = x$X2, model = "fs", reps = 5, a_prop = 2, b_prop = 2)
  time <- c("graph" = out_graph$time,
            "fs" = out_fs$time)
  metrics <- data.frame("model" = c("graph", "fs"),
                        rbind(binary_metrics(Delta = x$Delta, Delta_hat = out_graph$out$Delta),
                              binary_metrics(Delta = x$Delta, Delta_hat = out_fs$out$Delta)))
  return(list("time" = time,
              "metrics" = metrics))},
  .options = furrr_options(seed = 1))

out_metrics <- split(out, rep(1:length(settings), each = reps)) |> 
  map_depth(2, "metrics") |> 
  map(list_rbind)

out_df <- transpose(settings)[c("prop", "beta", "p")] |> 
  map(list_simplify) |> 
  as_tibble() |> 
  mutate("metrics" = out_metrics) |> 
  unnest(metrics) |> 
  pivot_longer(cols = -c(prop, beta, model, p), names_to = "metric", values_to = "value") |> 
  mutate(model = factor(model, levels = c("graph", "fs"), labels = c("Graph", "Comp")))

(p_metrics <- map(split(out_df, f = out_df$p),
                 \(x) ggplot(x, aes(x = metric, y = value, col = model))+
                   geom_boxplot(width = 0.75, staplewidth = 0.75, outlier.shape = 1, alpha = 0.9)+
                   facet_grid(rows = vars(prop),
                              col = vars(beta),
                              labeller = label_bquote(cols = beta==.(beta),
                                                      rows = Overlap==.(100*prop)~"%"))+
                   labs(x = NULL, y = NULL, col = NULL)+
                   theme_bw()+
                   theme(strip.background = element_rect(fill = NA))))

map2(.x = p_metrics, .y = c("p5", "p7"),
     .f = \(x, y) ggsave(filename = paste0("output/metrics-sim-", y, ".pdf"),
                         plot = x,
                         width = 6,
                         height = 5))


save(list = c("data", "settings", "out", "out_metrics"), file = "output/out-metrics-sim.RData")

# map_depth(out, 2, "time") |> 
#   map_depth(2, as_tibble_row) |> 
#   map(list_rbind) |> 
#   pivot_longer(cols = everything(), names_to = "model", values_to = "time") |> 
#   ggplot(aes(x = model, y = time))+
#   geom_boxplot()
