library(readr)
library(dplyr)
library(ggraph)
library(dendextend)
library(factoextra)


test_data <- read_csv(paste0("https://raw.githubusercontent.com/KatieKey/input_output_shiny_group/",
                             "master/CSV_Files/efficacy_summary.csv"))

test_data_dend <- test_data %>% 
  select(PLA:SLE,cLogP:MacUptake) %>% 
  mutate_all(funs(scale(.))) %>% 
  as.matrix() %>% 
  t() %>% 
  dist() %>% 
  hclust() %>% 
  as.dendrogram(horiz = TRUE, hang = .3)



fviz_dend(test_data_dend, k = 6, # Cut in four groups
          cex = 0.5,
          xlab = "",
          ylab = "",
          main = "Cluster by Measurements",
          horiz = TRUE,
          axes = FALSE,
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", "#F09428",
                       "#18E154"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE, # Add rectangle around groups
          rect_border = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", "#F09428",
                          "#18E154"),
          rect_fill = TRUE)