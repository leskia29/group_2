#read in xlsx to github
library(readxl)
library(tidyverse)
library(stringr)

#just file name because already working in group_2 directory 
testdata <- read_excel("DRUG_CLASS_I_Mean_Cmax_Trough_Efficacy_R_DATA_ANALYSIS.xlsx", 
                       skip = 1, n_max = 12, sheet = )
summary(testdata)
