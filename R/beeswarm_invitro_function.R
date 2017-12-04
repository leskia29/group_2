#in vitro function 

invitro_beeswarm_function <- function(efficacy_summary, variables = c(), drugs = c()){
  
  efficacy_summary <- read_csv(paste0("https://raw.githubusercontent.com/KatieKey/input_output_shiny_group/",
                                      "master/CSV_Files/efficacy_summary.csv"))
  in_vitro <- efficacy_summary %>%
    select(drug, dosage, dose_int, cLogP, huPPB, muPPB, 
           MIC_Erdman, MICserumErd, MIC_Rv, Caseum_binding, MacUptake) %>%
    rename(Drugs = "drug") %>% 
    unite(dosage_interval, dosage:dose_int, sep = "")
  
  in_vitro_SM <- in_vitro %>% 
    gather(key = variable, value = value, -Drugs, -dosage_interval) %>% 
    mutate(variable = factor(variable, levels = c("Caseum_binding", "cLogP", "huPPB", "muPPB", "MIC_Erdman", "MICserumErd", "MIC_Rv", "MacUptake"),
                             labels = c("Caseum \nBinding", "cLogP", 
                                        "Human \nPlasma \nBinding", "Mouse \nPlasma \nBinding", 
                                        "MIC Erdman \nStrain", "MIC Erdman \nStrain \nwith Serum", "MIC Rv Strain",
                                        "Macrophage \nUptake (Ratio)"))) %>% 
    mutate(dosage_interval = factor(dosage_interval, levels = c("50BID", "100QD")))
  
  invitro_variable_filtered <- in_vitro_SM %>% 
    dplyr::filter(variable %in% variables)
  
  invitro_drug_filtered <- in_vitro_SM %>% 
    dplyr::filter(Drugs %in% drugs)
  
  
  in_vitro_SMplot <- in_vitro_SM %>% 
    ggplot(aes(x = dosage_interval, y = value, color = Drugs))+
    geom_beeswarm(alpha = 0.5, size = 1.5)+
    labs(x = 'Dosage-Interval', y = 'Value')+
    ggtitle('In-Vitro Distribution of TB Drugs')+
    theme_few()+
    facet_wrap(~ variable, ncol = 4, scale="free")
  
  return(in_vitro_SMplot)
  
}

invitro_beeswarm_function()

#figure out why it's not selecting 
#should I source the csv file in the function? 

