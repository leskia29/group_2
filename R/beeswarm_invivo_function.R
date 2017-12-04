#invivo function

efficacy_summary <- paste0("https://raw.githubusercontent.com/KatieKey/input_output_shiny_group/",
                                    "master/CSV_Files/efficacy_summary.csv")
#IMPORTANT NOTES:

#OPTIONS FOR VARIABLES STATEMENT
#"RIM", "OCS","ICS","ULU","SLU","SLE","PLA"

#OPTIONS FOR DRUGS STATEMENT
#"DRUG1", "DRUG2", "DRUG3", "DRUG4", "DRUG5", "DRUG6", 
#"DRUG7", "DRUG8", "DRUG9", "DRUG10", DRUG11" 

#if default is NULL (i.e. no input for variabels and drugs statements) 
#then it will plot ALL variables and drugs! 


invivo_beeswarm_function <- function(efficacy_summary, variables = NULL, drugs = NULL) {
  
  efficacy_summary <- read_csv(efficacy_summary) 
  
  in_vivo <- efficacy_summary %>%
    select(drug, dosage, dose_int, PLA, ULU, RIM, OCS, ICS, SLU, SLE) %>% 
    rename(Drugs = "drug") %>% 
    unite(dosage_interval, dosage:dose_int, sep = "")
  
  in_vivo_SM <- in_vivo %>% 
    gather(key = variable, value = value, -Drugs, -dosage_interval) %>%
    mutate(variable_filtered = variable) %>% 
    mutate(variable = factor(variable, levels = c("RIM", "OCS","ICS","ULU","SLU","SLE","PLA"),
                             labels = c("Rim (of lesion)","Outer Caseum","Inner Caseum","Uninvolved Lung",
                                        "Standard Lung", "Standard Lesion", "Plasma"))) %>% 
    mutate(dosage_interval = factor(dosage_interval, levels = c("50BID","100QD")))
  
  
  if(!is.null(variables)) {
    in_vivo_SM <- in_vivo_SM %>% 
      dplyr::filter(variable_filtered %in% variables)
  }
  
  if(!is.null(drugs)) {
    in_vivo_SM <- in_vivo_SM %>%
      dplyr::filter(Drugs %in% drugs)
  }
  
  in_vivo_SMplot <- in_vivo_SM %>% 
    ggplot(aes(x = dosage_interval, y = value, color = Drugs))+
    geom_beeswarm(alpha = 0.5, size = 1.5)+
    scale_y_log10()+
    labs(x = 'Dosage-Interval', y = 'Value')+
    ggtitle('In-Vivo Distribution of TB Drugs')+
    theme_few()+
    facet_wrap(~ variable, ncol = 4)
  
  ggplotly(in_vivo_SMplot)
  
  return(ggplotly(in_vivo_SMplot))
  
}

invivo_beeswarm_function(efficacy_summary)

#example selection
invivo_beeswarm_function(efficacy_summary, variables = c("RIM", "OCS", "ULU", "PLA"),
                         drugs = c("DRUG1", "DRUG2"))



