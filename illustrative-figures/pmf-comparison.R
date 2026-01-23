library(tidyverse)

L <- 5
beta <- 0.15
sigma <- -1 / log(beta / (L - beta * (L - 1)))
y <- 2
theta <- dbinom(x = 0:(L-1), size = L-1, prob = 0.5)

p_pmf <- tibble("category" = 1:L,
       "f_hamming" = exp(-(1 - as.integer(1:L == y)) / sigma - log(1 + (L - 1) * exp(-1 / sigma))),
       "f_graph" = (1 - beta) * as.integer(1:L == y) + beta * theta) |> 
  pivot_longer(cols = -category, names_prefix = "f_", names_to = "distr", values_to = "fx") |> 
  ggplot(aes(x = category, y = fx))+
  facet_wrap(facets = vars(distr), labeller = as_labeller(c("graph" = "Graphical RL model", "hamming" = "Hamming distribution")))+
  geom_col(width = 0.6, alpha = 0.9, fill = "royalblue")+
  labs(x = "x", y = "P(X = x)")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA))
ggsave(filename = "pmf-comparison.pdf", plot = p_pmf, width = 6, height = 3)
