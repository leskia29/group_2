library(readr)
library(dplyr)
library(ggraph)
library(dendextend)



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

test_data_dend<- rotate(dendogram, 1:21)
  



(test_data_dend) %>% 
plot(xlab = "",
     ylab = "",
     main = "Cluster Example")

  
  


?plot.hclust
?dendextend
