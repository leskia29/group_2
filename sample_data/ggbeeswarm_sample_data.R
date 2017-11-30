#using ggbeeswarm with sample data 

library(dplyr)
library(readr)
library(ggplot2)
library(ggbeeswarm)
library(ggthemes)
library(tidyr)
library(scales)
library(tidyr)
library(crosstalk)
library(d3scatter)


test_data <- read_csv(paste0("https://raw.githubusercontent.com/KatieKey/input_output_shiny_group/",
                             "master/CSV_Files/efficacy_summary.csv"))
#testdata <- read_excel("DRUG_CLASS_I_Mean_Cmax_Trough_Efficacy_R_DATA_ANALYSIS.xlsx", skip = 1, n_max = 12, sheet = )
summary(test_data)
head(test_data)


#select for in-vivo marker variables
vivo_sample_test <- test_data %>%
  select(drug, dosage, dose_int, PLA, ULU, RIM, OCS, ICS, SLU, SLE) %>% 
  rename(Drugs = "drug") %>% 
  mutate(dosage = factor(dosage)) %>% 
  unite(dosage_interval, dosage:dose_int, sep = "")
  
head(vivo_sample_test)


#single variable plot for in-vivo marker
test_plot <- vivo_sample_test %>% 
  ggplot(aes(dosage_interval, PLA, color = Drugs))+
  geom_beeswarm(alpha=.5,size=1.5)+
  ggtitle('Variable = PLA') + labs(x='Dose')+
  theme_few()

test_plot

#gather for small multiples in-vivo markers
vivo_sample_forSM <- vivo_sample_test %>% 
  gather(key = variable, value = value, -Drugs, -dosage_interval) %>% 
  mutate(variable = factor(variable, levels = c("RIM", "OCS","ICS","ULU","SLU","SLE","PLA"),
                           labels = c("Rim (of lesion)","Outer Caseum","Inner Caseum","Uninvolved Lung",
                                      "Standard Lung", "Standard Lesion", "Plasma"))) %>% 
  mutate(dosage_interval = factor(dosage_interval, levels = c("50BID","100QD")))


head(vivo_sample_forSM)


#plot small multiples in-vivo markers 
vivo_sample_SMplot <- vivo_sample_forSM %>% 
  ggplot(aes(x = dosage_interval, y = value, color = Drugs))+
  geom_beeswarm(alpha = 0.5, size = 1.5)+
  scale_y_log10()+
  labs(x = 'Dose', y = 'Value')+
  ggtitle('In-Vivo Distribution of TB Drugs')+
  theme_few()+
  facet_wrap(~ variable, ncol = 4)

vivo_sample_SMplot

#####################################################################

#select for in-vitro marker variables
vitro_sample_test <- test_data %>%
  select(drug, dosage, dose_int, cLogP, huPPB, muPPB, 
         MIC_Erdman, MICserumErd, MIC_Rv, Caseum_binding, MacUptake) %>%
  rename(Drugs = "drug") %>% 
  mutate(dosage = factor(dosage)) %>% 
  unite(dosage_interval, dosage:dose_int, sep = "")

head(vitro_sample_test)

#single variable plot for in-vitro marker
test_plot_2 <- vitro_sample_test %>% 
  ggplot(aes(dosage_interval, cLogP, color = Drugs))+
  geom_beeswarm(alpha=.5,size=1.5)+
  ggtitle('Variable = cLogP') + labs(x='Dose')+
  theme_few()

test_plot_2

test_plot_3 <- vitro_sample_test %>% 
  ggplot(aes(dosage_interval, MIC_Erdman, color = Drugs))+
  geom_beeswarm(alpha=.5,size=1.5)+
  ggtitle('Variable = MIC_Erdman') + labs(x='Dosage-Interval')+
  theme_few()

test_plot_3


#gather for small multiples in-vitro markers
vitro_sample_forSM <- vitro_sample_test %>% 
  gather(key = variable, value = value, -Drugs, -dosage_interval) %>% 
  mutate(variable = factor(variable, levels = c("Caseum_binding", "cLogP", "huPPB", "muPPB", "MIC_Erdman", "MICserumErd", "MIC_Rv", "MacUptake"),
                           labels = c("Caseum \nBinding", "cLogP", 
                                      "Human \nPlasma \nBinding", "Mouse \nPlasma \nBinding", 
                                      "MIC Erdman \nStrain", "MIC Erdman \nStrain \nwith Serum", "MIC Rv Strain",
                                      "Macrophage \nUptake (Ratio)"))) %>% 
  mutate(dosage_interval = factor(dosage_interval, levels = c("50BID", "100QD")))

head(vitro_sample_forSM) 

#plot small multiples in-vitro markers 
vitro_sample_SMplot <- vitro_sample_forSM %>% 
  ggplot(aes(x = dosage_interval, y = value, color = Drugs))+
  geom_beeswarm(alpha = 0.5, size = 1.5)+
  labs(x = 'Dosage-Interval', y = 'Value')+
  ggtitle('In-Vitro Distribution of TB Drugs')+
  theme_few()+
  facet_wrap(~ variable, ncol = 4, scale="free")

vitro_sample_SMplot



