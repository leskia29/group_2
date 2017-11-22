library(readr)
library(dplyr)
library(ggraph)
library(dendextend)
library(factoextra)
install.packages("factoextra")

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


plot(test_data_dend, k = 4,
     k_colors = "jco",
     xlab = "",
     ylab = "",
     main = "Cluster by Measurements",
     horiz = TRUE,
     axes = FALSE)
par(cex = .6, mar=c(9, 11, 7, 7))
par(cex = 0.6)

