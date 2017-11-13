#using ggbeeswarm with sample data 

library(dplyr)
library(readr)
library(ggplot2)
library(ggbeeswarm)
library(ggthemes)

test_data <- read_csv(paste0("https://raw.githubusercontent.com/KatieKey/input_output_shiny_group/",
                             "master/CSV_Files/efficacy_summary.csv"))
#testdata <- read_excel("DRUG_CLASS_I_Mean_Cmax_Trough_Efficacy_R_DATA_ANALYSIS.xlsx", skip = 1, n_max = 12, sheet = )
summary(test_data)
head(test_data)


bio_sample_test <- test_data %>%
  select(drug, dosage, PLA, ULU, RIM, OCS, ICS, SLU, SLE)
  
head(bio_sample_test)

test_plot <- bio_sample_test %>% 
  ggplot(aes(dosage, PLA, color = drug))+
  geom_beeswarm(alpha=.5,size=1.5)+
  scale_x_continuous(breaks=c(50,100))+
  ggtitle('geom_beeswarm') + labs(x='Dose')+
  theme_few()






