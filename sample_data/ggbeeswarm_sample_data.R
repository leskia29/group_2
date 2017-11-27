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
vivo_sample_test <- test_data %>%
  select(drug, dosage, PLA, ULU, RIM, OCS, ICS, SLU, SLE) %>% 
  mutate(dosage = factor(dosage))
  
head(vivo_sample_test)


#single variable plot for in-vivo marker
test_plot <- vivo_sample_test %>% 
  ggplot(aes(dosage, PLA, color = drug))+
  geom_beeswarm(alpha=.5,size=1.5)+
  ggtitle('Variable = PLA') + labs(x='Dose')+
  theme_few()

test_plot

#gather for small multiples in-vivo markers
vivo_sample_forSM <- vivo_sample_test %>% 
  gather(key = variable, value = value, -drug, -dosage)

head(vivo_sample_forSM)


#plot small multiples in-vivo markers 
vivo_sample_SMplot <- vivo_sample_forSM %>% 
  ggplot(aes(x = dosage, y = value, color = drug))+
  geom_beeswarm(alpha = 0.5, size = 1.5)+
  scale_y_log10()+
  labs(x = 'Dose', y = 'Value')+
  ggtitle('In-Vivo Distribution of TB Drugs')+
  theme_few()+
  facet_wrap(~ variable, ncol = 4)

vivo_sample_SMplot

function()

#####################################################################

#select for in-vitro marker variables
vitro_sample_test <- test_data %>%
  select(drug, dosage, cLogP, huPPB, muPPB, 
         MIC_Erdman, MICserumErd, MIC_Rv, Caseum_binding, MacUptake) %>% 
  mutate(dosage = factor(dosage))

head(vitro_sample_test)

#single variable plot for in-vitro marker
test_plot_2 <- vitro_sample_test %>% 
  ggplot(aes(dosage, cLogP, color = drug))+
  geom_beeswarm(alpha=.5,size=1.5)+
  ggtitle('Variable = cLogP') + labs(x='Dose')+
  theme_few()

test_plot_2

#gather for small multiples in-vitro markers
vitro_sample_forSM <- vitro_sample_test %>% 
  gather(key = variable, value = value, -drug, -dosage) %>% 
  mutate(variable = factor(variable, levels = c("Caseum_binding", "cLogP", "huPPB", "muPPB", "MIC_Erdman", "MICserumErd", "MIC_Rv", "MacUptake"),
                           labels = c("Ex-Vivo \nCaseum \nBinding", "In-Vitro \nLipophilic \nBinding", 
                                      "Human \nPlasma \nBinding", "Mouse \nPlasma \nBinding", 
                                      "MIC Erdman \nStrain", "MIC Erdman \nStrain \nwith Serum", "MIC Rv Strain",
                                      "Macrophage \nUptake (Ratio)"))) 

head(vitro_sample_forSM) 

#plot small multiples in-vitro markers 
vitro_sample_SMplot <- vitro_sample_forSM %>% 
  ggplot(aes(x = dosage, y = value, color = drug))+
  geom_beeswarm(alpha = 0.5, size = 1.5)+
  labs(x = 'Dose', y = 'Value')+
  ggtitle('In-Vitro Distribution of TB Drugs')+
  theme_few()+
  facet_wrap(~ variable, ncol = 4, scale="free")

vitro_sample_SMplot

#create function that allows to select DRUG and VARIBALE


