##Functions
library(readr)
library(dplyr)
library(ggraph)
library(dendextend)
library(factoextra)
library(ggthemes)
library(tibble)
library(dendextend)

##############################by measurement###############################################
#superfxn <- function(category, variables =c("measurement"))
#superfxn, by_test or by_category 
superfxn <- function(category) {
   #1. read data in
   category<- read_csv(paste0("https://raw.githubusercontent.com/KatieKey/input_output_shiny_group/",
                             "master/CSV_Files/efficacy_summary.csv")) 
   #how do we change the inputted data file? (raw data)
   #1.5 choose what you want to do, then clean the data accordngly
   if(category <= "by_test"){
         by_test <- category %>% 
            select(PLA:SLE,cLogP:MacUptake) %>% 
           mutate_all(funs(scale(.))) %>%
           rename(plasma = PLA,
                  uninvolved_lung = ULU,
                  outer_caseum = OCS,
                  inner_caseum = ICS,
                  standard_lung = SLU,
                  standard_lesion = SLE,
                  macrophage_uptake = MacUptake,
                  human_binding_plasma =huPPB,
                  mouse_binding_plasma = muPPB)
            as.matrix() %>% 
             t() %>% 
             dist() %>% 
              hclust() %>% 
              as.dendrogram(horiz = TRUE, hang = .3)
         ggdendrogram(test_data_dend, 
                         segments = TRUE,
                         rotate = TRUE, 
                         labels = TRUE,
                         leaf_labels = TRUE,
                         size = 2,
                         theme_dendro = TRUE)  +
              labs(title = "Comparison by test")
         
  ##cluster shown in colors
  # fviz_dend(measurement, k = 6, # Cut in four groups
  #         cex = 0.5,
  #         xlab = "",
  #         ylab = "",
  #         main = "Cluster by Measurements",
  #         horiz = TRUE,
  #         k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", "#F09428",
  #                      "#18E154"),
  #         color_labels_by_k = FALSE, # color labels by groups
  #         rect = TRUE, # Add rectangle around groups
  #         rect_border = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", "#F09428",
  #                         "#18E154"),
  #         rect_fill = TRUE)+
  # theme_void()  
 } else {
  drug <- category %>% 
    tidyr::unite(drugdetail, drug:level, sep = "_") %>% #combine identifying data into one column, 
    mutate_each_(funs(scale(.) %>% as.vector),
                 vars = c("PLA", "ULU", "RIM", "OCS", "ICS", "SLU", "SLE", "cLogP", "huPPB","muPPB",
                          "MIC_Erdman", "MICserumErd", "MIC_Rv", "Caseum_binding", "MacUptake")) %>% #scales
    select(drugdetail, PLA:MacUptake, -ELU, -ESP) %>%  #remove efficacy 
    tibble::column_to_rownames (var = "drugdetail") %>%  #make drugdetail leaf name!  
    dist() %>% 
    hclust() %>%  #can change method 
    as.dendrogram(horiz = TRUE, hang = .1) 
  
  ggdendrogram(test_data_dend2, 
               segments = TRUE,
               rotate = TRUE, 
               labels = TRUE,
               #leaf_labels = TRUE,
               size = 2,
               theme_dendro = TRUE)  +
    labs(title = "Comparison by drug, dose, dose-int, and level") +
    theme(axis.title.x = element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
    
    
#     
#     ###add labels to name full names?
#       as.matrix() %>% 
#     t() %>% 
#     dist() %>% 
#     hclust() %>% 
#     as.dendrogram(horiz = TRUE, hang = .1)
#   plot (drug,
#       xlab = "",
#       ylab = "",
#       main = "by drug combo",
#       horiz = TRUE,
#       axes = FALSE)
# #par(cex = 0.6, mar=c(9,11,7,7)) #cex magnifies text; mar does axis 
# #par(cex = 0.6))
} }

superfxn(category = "by_test")
