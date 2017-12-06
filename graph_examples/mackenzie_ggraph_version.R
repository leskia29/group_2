library(readr)
library(dplyr)
library(tidyr) #unite
library(ggraph)
library(tibble)
library(dendextend)
library(ggdendro)

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
         standard_lesion = SLE,
         macrophage_uptake = MacUptake,
         human_binding_plasma =huPPB,
         mouse_binding_plasma = muPPB) %>% 
  as.matrix() %>% 
  t() %>% #transpose 
  dist() %>% 
  hclust() %>% 
  as.dendrogram(horiz = TRUE, hang = .1) 

ggdendrogram(test_data_dend, 
             segments = TRUE,
             rotate = TRUE, 
             labels = TRUE,
             leaf_labels = TRUE,
             size = 2,
             theme_dendro = TRUE)  +
  labs(title = "Comparison by test")

test_data_dend2 <- test_data %>% 
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
  labs(title = "Comparison by Drug") +
  theme(axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())






test_data <- read_csv(paste0("https://raw.githubusercontent.com/KatieKey/input_output_shiny_group/",
                             "master/CSV_Files/efficacy_summary.csv"))

sortr <- test_data %>% 
  tidyr::unite(drugdetail, drug:level, sep = "_") %>% #combine identifying data into one column, 
  remove_rownames %>% 
  tibble::column_to_rownames (var = "drugdetail") %>%  #make drugdetail row name , but doesn' showup 
  select(PLA:MacUptake, -ELU, -ESP) %>%  #remove efficacy 
  mutate_each_(funs(scale(.) %>% as.vector),
               vars = c("PLA", "ULU", "RIM", "OCS", "ICS", "SLU", "SLE", "cLogP", "huPPB","muPPB",
                        "MIC_Erdman", "MICserumErd", "MIC_Rv", "Caseum_binding", "MacUptake")) %>% 
  #scales 
  as.matrix() %>% 
  t() %>%  
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "drugtype") #makes drugtype new rowname, but dont see 

sortr$type <- ifelse(sortr$drugtype %in% c("cLogP", "huPPB","muPPB", "MIC_Erdman", "MICserumErd", "MIC_Rv","Caseum_binding", "MacUptake"), "invivo", "invitro") #made new column 
df<- sortr %>% 
  # select(drugtype,type, DRUG1_50_BID_Cmax:DRUG11_100_QD_Trough) %>% #if include type 
  select(drugtype,type, V1:V21) %>% #lost column names 
  dist() %>% 
  hclust() %>% 
  as.dendrogram(horiz = TRUE, hang = .1) %>% 
  plot(xlab = "",
       ylab = "",
       main = "Across Drug Measurements",
       horiz = TRUE,
       axes = FALSE)
par(cex = 0.6, mar=c(9,11,7,7)) %>% #cex magnifies text; mar does axis
  par(cex = 0.6) 



########################function part 1##############################
##Functions
library(readr)
library(dplyr)
library(ggraph)
#library(factoextra)
library(ggthemes)
library(tibble)
library(dendextend)
library(ggplot2)
library(ggdendro)

##############################by measurement###############################################
#superfxn <- function(category, variables =c("measurement"))
#superfxn, by_test or by_category  
input_data<- read_csv(paste0("https://raw.githubusercontent.com/KatieKey/input_output_shiny_group/",
                             "master/CSV_Files/efficacy_summary.csv")) 
#superfux(df= input_data, category = "by_test" OR "drug")
df = input_data
category= "by_test"
superfxn <- function(df, category) {
  #test_type <- ifelse(colnames(input_data) %in% 
  #c("cLogP", "huPPB","muPPB", "MIC_Erdman", 
  #"MICserumErd", "MIC_Rv","Caseum_binding", "MacUptake"), 1, 2) 
  #makes vecotr #could assign color  1 is invivo  
  #how do we change the inputted data file? (raw data)
  #1.5 choose what you want to do, then clean the data accordngly
  if(category == "by_test"){
    by_test <- df %>% 
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
                                                          c("macrophage_uptake","cLogP", "MIC_Erdman", "MICserumErd", 
                                                            "MIC_Rv","Caseum_binding",
                                                           "human_binding_plasma",
                                                          "mouse_binding_plasma"), "red", "blue"))  # red is invitro
      }
      return(by_test)
    }
    d <- dendrapply(as.dendrogram(by_test), labelCol)
    plot_horiz.dendrogram(d, side = TRUE, main = "Comparison by Test")
    #plot(d, horiz = TRUE, main = "by test", sub="color coded by test type", xlab = "")
    cols <- c("red","blue")
    legend("topright", legend = c("invitro","invivo"),
           fill = cols, border = cols, bty = "n")
    par(cex = 0.6, mar=c(9,11,10,10)) %>% #cex magnifies text; mar does axis
      par(cex = 0.6) 
  }}

superfxn(input_data, category = "by_test")
    #base plot oldpar<- par(mar xxxx, oma xxx)  run at start
    # par(oldpar) rest    at begining and end of function ; side effect of function
    # try ggplot or ggdend with colors 