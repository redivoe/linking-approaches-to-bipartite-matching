library(tidyverse)
library(linkbrl)

connected <- expand_grid("beta" = seq(0, 0.35, len = 100),
            "L" = 2:10) |> 
  rowwise() |> 
  mutate("comp_params" = list(linkbrl:::muparams_graph_er(beta = beta, theta = array(dim = c(1, L), rep(1/L, L))))) |> 
  ungroup() |> 
  mutate("m" = map_dbl(comp_params, "m"),
         "u" = map_dbl(comp_params, "u"))

p_connection <- connected |> 
  ggplot(aes(x = beta, y = m, col = factor(L, ordered = TRUE)))+
  geom_line(alpha = 0.8)+
  labs(x = expression(beta), col = "# categories")+
  scale_color_viridis_d(option = "B", direction = -1, begin = 0.1, end = 0.8)+
  theme_bw()

ggsave(filename = "connected-parameters.pdf", plot = p_connection, width = 4, height = 3)
