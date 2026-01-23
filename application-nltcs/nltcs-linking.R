library(tidyverse)
library(Matrix)
library(linkbrl)

load("input/nltcs-sep.RData")
nltcs_treated <- map(list(nltcs82, nltcs89),
                     \(x) x |>
                       mutate(across(c(sex, regional_office), as.integer)) |> 
                       select(c(id, sex, birth_day, birth_month, birth_year, regional_office, state)) |> 
                       drop_na())
id <- map(nltcs_treated, \(x) pull(x, id))
X <- map(nltcs_treated, \(x) as.matrix(select(x, -id)))
rm(list = setdiff(ls(), c("X", "id", "nltcs_treated")))


set.seed(1)
out_comp <- brl_cem(X1 = X[[1]], X2 = X[[2]],
                    model = "fs",
                    a_prop = 2, b_prop = 2,
                    reps = 10)

set.seed(1)
out_graph <- brl_cem(X1 = X[[1]], X2 = X[[2]],
                     model = "graph",
                     theta_method = "uniform",
                     a_beta = 1, b_beta = 1,
                     a_prop = 2, b_prop = 2,
                     reps = 10)

Delta <- +outer(id[[1]], id[[2]], FUN = "==") |> 
  as(Class = "TsparseMatrix")

binary_metrics(Delta = Delta, Delta_hat = out_comp$Delta)
# recall    precision        F1 
# 0.9034668 0.9812773 0.9407659 

binary_metrics(Delta = Delta, Delta_hat = out_graph$Delta)
# recall    precision        F1 
# 0.9121816 0.7861505 0.8444898 


nltcs_unique <- nltcs_treated |> 
  list_rbind() |> 
  group_by(id) |> 
  summarise(across(everything(), first), .groups = "drop") |> 
  select(-id)
nltcs_unique$state <- match(nltcs_unique$state, unique(nltcs_unique$state))
nltcs_unique$regional_office <- match(nltcs_unique$regional_office, unique(nltcs_unique$regional_office))

var_labs <- c("Sex", "Day of birth", "Month of birth", "Year of birth", "Regional office", "State")
names(var_labs) <- colnames(nltcs_unique)

(p_marginals <- nltcs_unique |> 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") |> 
  mutate(value = factor(value)) |> 
  ggplot(aes(x = value))+
  geom_bar(aes(y = after_stat(prop), group = variable), width = 0.6, alpha = 0.9, fill = "royalblue")+
  facet_wrap(facets = vars(variable), scales = "free", labeller = as_labeller(var_labs))+
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
  scale_y_continuous(breaks = scales::breaks_pretty())+
  labs(x = NULL, y = NULL)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA)))
ggsave(filename = "output/nltcs-marginals.pdf", plot = p_marginals, width = 7, height = 5)

save.image(file = "output/linking-nltcs-output.RData")
