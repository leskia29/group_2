#final code for function 

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


efficacy_summary <- read_csv(paste0("https://raw.githubusercontent.com/KatieKey/input_output_shiny_group/",
                             "master/CSV_Files/efficacy_summary.csv"))
#testdata <- read_excel("DRUG_CLASS_I_Mean_Cmax_Trough_Efficacy_R_DATA_ANALYSIS.xlsx", skip = 1, n_max = 12, sheet = )
summary(efficacy_summary)
head(efficacy_summary)


#select for in-vivo marker variables
in_vivo <- efficacy_summary %>%
  select(drug, dosage, dose_int, PLA, ULU, RIM, OCS, ICS, SLU, SLE) %>% 
  rename(Drugs = "drug") %>% 
  mutate(dosage = factor(dosage)) %>% 
  unite(dosage_interval, dosage:dose_int, sep = "")

head(in_vivo)

#gather for small multiples in-vivo markers
in_vivo_SM <- in_vivo %>% 
  gather(key = variable, value = value, -Drugs, -dosage_interval) %>% 
  mutate(variable = factor(variable, levels = c("RIM", "OCS","ICS","ULU","SLU","SLE","PLA"),
                           labels = c("Rim (of lesion)","Outer Caseum","Inner Caseum","Uninvolved Lung",
                                      "Standard Lung", "Standard Lesion", "Plasma"))) %>% 
  mutate(dosage_interval = factor(dosage_interval, levels = c("50BID","100QD")))


head(in_vivo_SM)

#plot small multiples in-vivo markers 
in_vivo_SMplot <- in_vivo_SM %>% 
  ggplot(aes(x = dosage_interval, y = value, color = Drugs))+
  geom_beeswarm(alpha = 0.5, size = 1.5)+
  scale_y_log10()+
  labs(x = 'Dose', y = 'Value')+
  ggtitle('In-Vivo Distribution of TB Drugs')+
  theme_few()+
  facet_wrap(~ variable, ncol = 4)

in_vivo_SMplot

#####################################################################

#select for in-vitro marker variables
in_vitro <- efficacy_summary %>%
  select(drug, dosage, dose_int, cLogP, huPPB, muPPB, 
         MIC_Erdman, MICserumErd, MIC_Rv, Caseum_binding, MacUptake) %>%
  rename(Drugs = "drug") %>% 
  mutate(dosage = factor(dosage)) %>% 
  unite(dosage_interval, dosage:dose_int, sep = "")

head(in_vitro)

#gather for small multiples in-vitro markers
in_vitro_SM <- in_vitro %>% 
  gather(key = variable, value = value, -Drugs, -dosage_interval) %>% 
  mutate(variable = factor(variable, levels = c("Caseum_binding", "cLogP", "huPPB", "muPPB", "MIC_Erdman", "MICserumErd", "MIC_Rv", "MacUptake"),
                           labels = c("Caseum \nBinding", "cLogP", 
                                      "Human \nPlasma \nBinding", "Mouse \nPlasma \nBinding", 
                                      "MIC Erdman \nStrain", "MIC Erdman \nStrain \nwith Serum", "MIC Rv Strain",
                                      "Macrophage \nUptake (Ratio)"))) %>% 
  mutate(dosage_interval = factor(dosage_interval, levels = c("50BID", "100QD")))

head(in_vitro_SM) 

#plot small multiples in-vitro markers 
in_vitro_SMplot <- in_vitro_SM %>% 
  ggplot(aes(x = dosage_interval, y = value, color = Drugs))+
  geom_beeswarm(alpha = 0.5, size = 1.5)+
  labs(x = 'Dosage-Interval', y = 'Value')+
  ggtitle('In-Vitro Distribution of TB Drugs')+
  theme_few()+
  facet_wrap(~ variable, ncol = 4, scale="free")

in_vitro_SMplot
