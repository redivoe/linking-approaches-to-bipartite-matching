library(tidyverse)
library(furrr)
library(gt)

library(Matrix)
library(linkbrl)

load("data/shiw-source.RData")


shiw_blocked <- map(list(shiw08, shiw10), \(x) x |> 
  nest_by(IREG) |> 
  mutate(ID = list(pull(data, ID)),
         X = list(select(data, -c(ID)) |> as.matrix())))

Delta <- map2(.x = shiw_blocked[[1]]$ID, .y = shiw_blocked[[2]]$ID,
              \(x, y) +outer(x, y, FUN = "=="))
switch_order <- map_lgl(Delta, \(x) ncol(x) > nrow(x))
Delta <- map2(Delta, switch_order, \(x, y) if(y){ t(x) }else{ x }) |> 
  map(\(x) as(x, Class = "TsparseMatrix"))

L <- L[!(names(L) %in% c("ANNO", "IREG", "ID"))]

plan(multisession(workers = 5))

out_comp <- future_map2(.x = shiw_blocked[[1]]$X,
                        .y = shiw_blocked[[2]]$X,
                        \(X1, X2){
                          brl_cem(X1 = X1, X2 = X2, model = "fs", reps = 10)
                        },
                        .options = furrr_options(seed = 123))

out_graph <- future_map2(.x = shiw_blocked[[1]]$X,
                         .y = shiw_blocked[[2]]$X,
                         \(X1, X2){
                           brl_cem(X1 = X1, X2 = X2, model = "graph", reps = 10)
                           },
                         .options = furrr_options(seed = 123))


metrics_comp <- map2(Delta, map(out_comp, "Delta"), binary_metrics) |> 
  map(as_tibble_row) |> 
  list_rbind()

metrics_graph <- map2(Delta, map(out_graph, "Delta"), binary_metrics) |> 
  map(as_tibble_row) |> 
  list_rbind()

(out_gt <- bind_cols("region" = shiw_blocked[[1]]$IREG,
          metrics_comp |> `colnames<-`(paste0(colnames(metrics_comp), "_comp")),
          metrics_graph |> `colnames<-`(paste0(colnames(metrics_graph), "_graph")),
          "block1" = map_dbl(shiw_blocked[[1]]$data, nrow),
          "block2" = map_dbl(shiw_blocked[[2]]$data, nrow)) |> 
  gt() |> 
  cols_merge(columns = c(block1, block2),
             pattern = "{1} \\(\\times\\) {2}") |> 
  cols_align(columns = 1, align = "left") |> 
  fmt_number(columns = -c(region, block1, block2), scale_by = 100, decimals = 1) |> 
  tab_spanner(label = "Graphical model", columns = ends_with("graph")) |> 
  tab_spanner(label = "Comparison model", columns = ends_with("comp")) |> 
  cols_label("region" ~ "Region of residence",
             starts_with("precision") ~ "Precision",
             starts_with("recall") ~ "Recall",
             starts_with("F1") ~ "F1",
             "block1" ~ "Block size"))

out_gt |> 
  as_latex() |>
  as.character() |>
  writeLines()

mean(metrics_comp$F1); mean(metrics_graph$F1)

save(list = c("metrics_graph", "metrics_comp"), file = "output/out-shiw-blocking.RData")
