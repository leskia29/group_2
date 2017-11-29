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
#superfxn <- function(category, variables =c("measurement", "drug"))
superfxn <- function(category) {
   #1. read data in
   category<- read_csv(paste0("https://raw.githubusercontent.com/KatieKey/input_output_shiny_group/",
                             "master/CSV_Files/efficacy_summary.csv")) 
   #how do we change the inputted data file? (raw data)
   #1.5 choose what you want to do, then clean the data accordngly
   if(category <= "measurement"){
         measurement <- category %>% 
            select(PLA:SLE,cLogP:MacUptake) %>% 
           mutate_all(funs(scale(.))) %>% 
            as.matrix() %>% 
             t() %>% 
             dist() %>% 
              hclust() %>% 
              as.dendrogram(horiz = TRUE, hang = .3)
  ##cluster shown in colors
  fviz_dend(measurement, k = 6, # Cut in four groups
          cex = 0.5,
          xlab = "",
          ylab = "",
          main = "Cluster by Measurements",
          horiz = TRUE,
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", "#F09428",
                       "#18E154"),
          color_labels_by_k = FALSE, # color labels by groups
          rect = TRUE, # Add rectangle around groups
          rect_border = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", "#F09428",
                          "#18E154"),
          rect_fill = TRUE)+
  theme_void()  
 } else {
  drug <- category %>% 
    tidyr::unite(drug, drug:level, sep = "_") %>% #combine identifying data into one column, 
    remove_rownames %>% 
    column_to_rownames (var = "drugz") %>%  #make drugz row name 
    select(PLA:MacUptake, -ELU, -ESP) %>%
     mutate_each_(funs(scale(.) %>% as.vector),
               vars = c("PLA", "ULU", "RIM", "OCS", "ICS", "SLU", "SLE", "cLogP", "huPPB","muPPB",
                        "MIC_Erdman", "MICserumErd", "Caseum_binding", "MacUptake")) %>% 
      as.matrix() %>% 
    t() %>% 
    dist() %>% 
    hclust() %>% 
    as.dendrogram(horiz = TRUE, hang = .1)
  plot (drug,
      xlab = "",
      ylab = "",
      main = "Across Drug Measurements",
      horiz = TRUE,
      axes = FALSE)
#par(cex = 0.6, mar=c(9,11,7,7)) #cex magnifies text; mar does axis 
#par(cex = 0.6))
} }

superfxn(category = "drug")
