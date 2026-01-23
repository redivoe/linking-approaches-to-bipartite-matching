library(tidyverse)
library(matrixStats)

get_prior_n12 <- function(n1, n2){
  prior_n12 <- sapply(0:n2, \(n12) lchoose(n2, n12) + lfactorial(n1) - lfactorial(n1 - n12))
  prior_n12 <- exp(prior_n12 - matrixStats::logSumExp(prior_n12))
  return(prior_n12)
}

n1 <- 50
n2 <- c(30, 40, 50)

priors <- map(n2, \(n2) tibble("n12" = 0:n2, "p" = get_prior_n12(n1, n2)))
p_n12prior <- tibble("n2" = n2, "out" = priors) |> 
  unnest(out) |> 
  ggplot(mapping = aes(x = n12))+
  geom_segment(aes(y = 0, yend = p))+
  facet_grid(cols = vars(n2),
             scales = "free",
             labeller = label_bquote(cols = n[2]==.(n2)))+
  labs(y = expression(f(n[12])), x = expression(n[12]))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA))

ggsave(filename = "n12-prior-uniform-delta-brl.pdf", plot = p_n12prior, width = 7, height = 3)
