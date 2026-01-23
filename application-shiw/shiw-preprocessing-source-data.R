library(tidyverse)
library(gt)

comp <- read_delim(file = "data/comp.csv")
region_labs <- read_delim("data/codice-regioni.txt", col_names = FALSE, delim = "\t") |> 
  `colnames<-`(c("region", "code")) |> 
  mutate(region = str_trim(region))


selected_vars <- tribble(
  ~ name, ~ original_name, ~ description,
  "wave" , "ANNO", NA,
  "sex" , "SESSO", "Sex", 
  "year_birth" , "ANASC", "Year of birth", 
  "region_birth" , "NASCREG", "Region of birth", 
  "region_residence" , "IREG", "Region of residence",
  "pop_municipality_residence" , "ACOM4C", "Popoulation of municipality of residence",
  "education" , "STUDIO", "Educational qualification",
  "employment_status" , "NONOC", "Employment status",
  "work_status" , "QUALP7N", "Professional status",
  "economic_sector" , "SETTP7", "Economic sector of main occupation",
  "foreign_country", "ENASC2", "Foreign area of birth"
)

shiw <- comp |> 
  filter(ANNO %in% c(2008, 2010)) |> 
  mutate(ID = paste0(NQUEST, NORD)) |> 
  # rename_with(.fn = \(x) selected_vars$name[selected_vars$original_name == x],
  #             .cols = selected_vars$original_name) |> 
  # select(all_of(c(selected_vars$name, "ID")))
  select(all_of(c(selected_vars$original_name, "ID"))) |> 
  filter(!is.na(SETTP7)) |> 
  mutate(across(-c(ANNO, ID, IREG), \(x) as.integer(as.factor(x))),
         IREG = factor(IREG, levels = region_labs$code, labels = region_labs$region),
         # ENASC2 = ifelse(is.na(ENASC2), 2, 1),
         across(c(NASCREG, ENASC2), \(x) as.integer(forcats::fct_na_value_to_level(factor(x)))))

shiw_split <- split(select(shiw, -ANNO), shiw$ANNO)

shiw08 <- shiw_split$`2008`
shiw10 <- shiw_split$`2010`

table(shiw$NASCREG)
colSums(is.na(shiw))
shiw$ID |> unique() |> length()

# shiw |> 
#   group_by(region_residence) |> 
#   summarise(length(unique(ID)))

L <- apply(shiw, 2, \(x) length(unique(x[!is.na(x)])))


save(file = "data/shiw-source.RData", list = c("shiw08", "shiw10", "L"))


selected_vars <- left_join(selected_vars,
          y = tibble("original_name" = names(L), "levels" = L),
          by = "original_name")

selected_vars |> 
  filter(name != "wave") |> 
  select(-name) |> 
  gt() |> 
  cols_label(
    original_name = "Variable",
    description = "Description",
    levels = "Number of unique values"
  ) |> 
  as_latex() |> 
  as.character() |> 
  writeLines()
  

