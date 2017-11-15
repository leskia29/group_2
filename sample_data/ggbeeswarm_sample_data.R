#using ggbeeswarm with sample data 

library(dplyr)
library(readr)
library(ggplot2)
library(ggbeeswarm)
library(ggthemes)
library(tidyr)
library(scales)


test_data <- read_csv(paste0("https://raw.githubusercontent.com/KatieKey/input_output_shiny_group/",
                             "master/CSV_Files/efficacy_summary.csv"))
#testdata <- read_excel("DRUG_CLASS_I_Mean_Cmax_Trough_Efficacy_R_DATA_ANALYSIS.xlsx", skip = 1, n_max = 12, sheet = )
summary(test_data)
head(test_data)


#select for in-vivo marker variables
bio_sample_test <- test_data %>%
  select(drug, dosage, PLA, ULU, RIM, OCS, ICS, SLU, SLE) 
  
head(bio_sample_test)


#single variable plot for in-vivo marker
test_plot <- bio_sample_test %>% 
  ggplot(aes(dosage, PLA, color = drug))+
  geom_beeswarm(alpha=.5,size=1.5)+
  scale_x_continuous(breaks=c(50,100))+
  ggtitle('Variable = PLA') + labs(x='Dose')+
  theme_few()

test_plot

#gather for small multiples in-vivo markers
bio_sample_forSM <- bio_sample_test %>% 
  gather(key = variable, value = value, -drug, -dosage)

head(bio_sample_forSM)


#plot small multiples in-vivo markers 
bio_sample_SMplot <- bio_sample_forSM %>% 
  ggplot(aes(x = dosage, y = value, color = drug))+
  geom_beeswarm(alpha = 0.5, size = 1.5)+
  scale_x_continuous(breaks = c(50,100))+
  coord_trans(y = "log10")+
  labs(x = 'Dose', y = 'Value')+
  ggtitle('In-Vivo Distribution of TB Drugs')+
  theme_few()+
  facet_wrap(~ variable, ncol = 4)

bio_sample_SMplot

#####################################################################

#select for in-vitro marker variables
chem_sample_test <- test_data %>%
  select(drug, dosage, cLogP, huPPB, muPPB, 
         MIC_Erdman, MICserumErd, MIC_Rv, Caseum_binding, MacUptake) 

head(chem_sample_test)

#single variable plot for in-vitro marker
test_plot_2 <- chem_sample_test %>% 
  ggplot(aes(dosage, cLogP, color = drug))+
  geom_beeswarm(alpha=.5,size=1.5)+
  scale_x_continuous(breaks=c(50,100))+
  ggtitle('Variable = cLogP') + labs(x='Dose')+
  theme_few()

test_plot_2

#gather for small multiples in-vitro markers
chem_sample_forSM <- chem_sample_test %>% 
  gather(key = variable, value = value, -drug, -dosage)

head(chem_sample_forSM)

#plot small multiples in-vitro markers 
chem_sample_SMplot <- chem_sample_forSM %>% 
  ggplot(aes(x = dosage, y = value, color = drug))+
  geom_beeswarm(alpha = 0.5, size = 1.5)+
  scale_x_continuous(breaks = c(50,100))+
  labs(x = 'Dose', y = 'Value')+
  ggtitle('In-Vitro Distribution of TB Drugs')+
  theme_few()+
  facet_wrap(~ variable, ncol = 4, scale="free")

chem_sample_SMplot

