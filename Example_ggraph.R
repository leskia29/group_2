library(readr)
library(dplyr)
library(ggraph)

test_data <- read_csv(paste0("https://raw.githubusercontent.com/KatieKey/input_output_shiny_group/",
                             "master/CSV_Files/efficacy_summary.csv"))

test_data_dend <- test_data %>% 
  select(PLA:SLE) %>% 
  mutate_all(funs(scale(.))) %>% 
  as.matrix() %>% 
  t() %>% 
  dist() %>% 
  hclust() %>% 
  as.dendrogram()

ggraph(test_data_dend, layout = "dendrogram") + 
  geom_edge_diagonal()


