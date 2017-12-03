#invivo function

invivo_function <- function(efficacy_summary, variables = c(), drugs = c()) {
  
  efficacy_summary <- read_csv(paste0("https://raw.githubusercontent.com/KatieKey/input_output_shiny_group/",
                                      "master/CSV_Files/efficacy_summary.csv"))
  in_vivo <- efficacy_summary %>%
    select(drug, dosage, dose_int, PLA, ULU, RIM, OCS, ICS, SLU, SLE) %>% 
    rename(Drugs = "drug") %>% 
    mutate(dosage = factor(dosage)) %>% 
    unite(dosage_interval, dosage:dose_int, sep = "")
  
  in_vivo_SM <- in_vivo %>% 
    gather(key = variable, value = value, -Drugs, -dosage_interval) %>% 
    mutate(variable = factor(variable, levels = c("RIM", "OCS","ICS","ULU","SLU","SLE","PLA"),
                             labels = c("Rim (of lesion)","Outer Caseum","Inner Caseum","Uninvolved Lung",
                                        "Standard Lung", "Standard Lesion", "Plasma"))) %>% 
    mutate(dosage_interval = factor(dosage_interval, levels = c("50BID","100QD")))
  
  invivo_variable_filtered <- in_vivo_SM %>% 
    dplyr::filter(variable %in% variables)
  
  invivo_drug_filtered <- in_vivo_SM %>% 
    dplyr::filter(Drugs %in% drugs)
  
  in_vivo_SMplot <- in_vivo_SM %>% 
    ggplot(aes(x = dosage_interval, y = value, color = Drugs))+
    geom_beeswarm(alpha = 0.5, size = 1.5)+
    scale_y_log10()+
    labs(x = 'Dosage-Interval', y = 'Value')+
    ggtitle('In-Vivo Distribution of TB Drugs')+
    theme_few()+
    facet_wrap(~ variable, ncol = 4)
  
  return(in_vivo_SMplot)
  
}

invivo_function(efficacy_summary, variables = c("Rim (of lesion)"), drugs = ("DRUG1"))

invivo_function()

#figure out why it's not selecting 
#should I source the csv in the function...? 


