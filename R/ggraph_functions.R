##Functions
library(readr)
library(dplyr)
library(ggraph)
library(dendextend)
#library(factoextra)
library(ggthemes)
library(tibble)
library(dendextend)

##############################by measurement###############################################
#superfxn <- function(category, variables =c("measurement"))
#superfxn, by_test or by_category  
input_data<- read_csv(paste0("https://raw.githubusercontent.com/KatieKey/input_output_shiny_group/",
                             "master/CSV_Files/efficacy_summary.csv")) 
#superfux(df= input_data, category = "by_test" OR "drug")
superfxn <- function(df, category) {
  test_type <- ifelse(colnames(input_data) %in% 
                         c("cLogP", "huPPB","muPPB", "MIC_Erdman", 
                           "MICserumErd", "MIC_Rv","Caseum_binding", "MacUptake"), 1, 2) 
                  #makes vecotr #could assign color  1 is invivo  
   #how do we change the inputted data file? (raw data)
   #1.5 choose what you want to do, then clean the data accordngly
   if(category == "by_test"){
         by_test <- input_data %>% 
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
                  mouse_binding_plasma = muPPB) %>% 
            as.matrix() %>% 
            t() %>% 
            dist() %>% 
            hclust() %>% 
            as.dendrogram(horiz = TRUE, hang = .3) #%>% 
         #new function within function to plot colors red = invivo; blue = in vitro 
         labelCol <- function(by_test) {
           if (is.leaf(by_test)) {
             ## fetch label
             label <- attr(by_test, "label") 
             ## set label color to red for A and B, to blue otherwise
             attr(by_test, "nodePar") <- list(lab.col=ifelse(label %in% 
                c("cLogP", "huPPB","muPPB", "MIC_Erdman", "MICserumErd", 
                  "MIC_Rv","Caseum_binding", "MacUptake"), "red", "blue"))
           }
             return(by_test)
         }
         d <- dendrapply(as.dendrogram(by_test), labelCol)
         plot_horiz.dendrogram(d, side = TRUE, main = "comparison by test")
         #plot(d, horiz = TRUE, main = "by test", sub="color coded by test type", xlab = "")
         cols <- c("red","blue")
         legend("topright", legend = c("invivo","invitro"),
                fill = cols, border = cols, bty = "n")
        # ggdendrogram(d, 
        #                  segments = TRUE,
        #                  rotate = TRUE, 
        #                  labels = TRUE,
        #                  leaf_labels = TRUE,
        #                  size = 2,
        #                  theme_dendro = TRUE)  +
        #       labs(title = "Comparison by test")
      } else {
  by_drug <- input_data %>% 
    tidyr::unite(drugdetail, drug:level, sep = "_") %>% #combine identifying data into one column, 
    mutate_each_(funs(scale(.) %>% as.vector),
                 vars = c("PLA", "ULU", "RIM", "OCS", "ICS", "SLU", "SLE", "cLogP", "huPPB","muPPB",
                          "MIC_Erdman", "MICserumErd", "MIC_Rv", "Caseum_binding", "MacUptake")) %>% #scales
    select(drugdetail, PLA:MacUptake, -ELU, -ESP) %>%  #remove efficacy 
    tibble::column_to_rownames (var = "drugdetail") %>%  #make drugdetail leaf name!  
    dist() %>% 
    hclust() %>%  #can change method 
    as.dendrogram(horiz = TRUE, hang = .1) 
  ggdendrogram(by_drug, 
               segments = TRUE,
               rotate = TRUE, 
               labels = TRUE,
               size = 2,
               theme_dendro = TRUE)  +
    labs(title = "Comparison by drug, dose, dose-int, and level") +
    theme(axis.title.x = element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
    
   } }

superfxn(df = input_data, category = "by_test")
superfxn(df = input_data, category = "by_drug")
