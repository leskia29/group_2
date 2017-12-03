library(readr)
library(dplyr)
library(tidyr) #unite
library(ggraph)
library(tibble)
library(dendextend)

# RIM = rim
# OCS - outer caeseum
# ICS -inner caseesum
# uninvolved lung ULU
# ULU -> rim -> OCS --> ICS  
# pla plasma
# sle - standard lesion (entire)
# standard lung SLU 
# mic - minimum inhibiroy concenration 
# 

test_data <- read_csv(paste0("https://raw.githubusercontent.com/KatieKey/input_output_shiny_group/",
                             "master/CSV_Files/efficacy_summary.csv"))

test_data_dend <- test_data %>% 
  tidyr::unite(drugz, drug:level, sep = "_") %>% #combine identifying data into one column, 
  remove_rownames %>% 
  column_to_rownames (var = "drugz") %>%  #make drugz row name 
  select(PLA:MacUptake, -ELU, -ESP) %>%  #remove efficacy 
  mutate_each_(funs(scale(.) %>% as.vector),
               vars = c("PLA", "ULU", "RIM", "OCS", "ICS", "SLU", "SLE", "cLogP", "huPPB","muPPB",
                        "MIC_Erdman", "MICserumErd", "MIC_Rv", "Caseum_binding", "MacUptake")) %>%
  rename(plasma = PLA,
         uninvolved_lung = ULU,
         outer_caseum = OCS,
         inner_caseum = ICS,
         standard_lung = SLU,
         standard_lung_lesion = SLE) %>% 
        
  as.matrix() %>% 
  t() %>% #transpose 
  dist() %>% 
  hclust() %>% 
  as.dendrogram(horiz = TRUE, hang = .1) %>% 
  plot(xlab = "",
       ylab = "",
       main = "Across Drug Measurements",
       horiz = TRUE,
       axes = FALSE)
par(cex = 0.5, mar=c(10,12,9,9)) %>% #cex magnifies text; mar does axis
  par(cex = 0.6)

