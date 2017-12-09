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
  
  return(in_vivo_SMplot)
  
}

invivo_beeswarm_function(efficacy_summary)

#example plot
invivo_beeswarm_function(efficacy_summary, variables = c("RIM", "OCS", "ICS", "PLA"),
                         drugs = c("DRUG1", "DRUG2","DRUG11", "DRUG9"))

#############################################################################################

#in vitro function 

efficacy_summary <- paste0("https://raw.githubusercontent.com/KatieKey/input_output_shiny_group/",
                           "master/CSV_Files/efficacy_summary.csv")

#IMPORTANT NOTES:

#OPTIONS FOR VARIABLES STATEMENT
#"Caseum_binding", "cLogP", "huPPB", "muPPB",
#"MIC_Erdman", "MICserumErd", "MIC_Rv", "MacUptake"

#OPTIONS FOR DRUGS STATEMENT
#"DRUG1", "DRUG2", "DRUG3", "DRUG4", "DRUG5", "DRUG6", 
#"DRUG7", "DRUG8", "DRUG9", "DRUG10", DRUG11" 

#if default is NULL (i.e. no input for variabels and drugs statements) 
#then it will plot ALL variables and drugs! 

invitro_beeswarm_function <- function(efficacy_summary, variables = NULL, drugs = NULL) {
  
  efficacy_summary <- read_csv(efficacy_summary) 
  
  in_vitro <- efficacy_summary %>%
    select(drug, dosage, dose_int, cLogP, huPPB, muPPB, 
           MIC_Erdman, MICserumErd, MIC_Rv, Caseum_binding, MacUptake) %>%
    rename(Drugs = "drug") %>% 
    unite(dosage_interval, dosage:dose_int, sep = "")
  
  in_vitro_SM <- in_vitro %>% 
    gather(key = variable, value = value, -Drugs, -dosage_interval) %>% 
    mutate(variable_filtered = variable) %>% 
    mutate(variable = factor(variable, levels = c("Caseum_binding", "cLogP", "huPPB", "muPPB", "MIC_Erdman",
                                                  "MICserumErd", "MIC_Rv", "MacUptake"),
                             labels = c("Caseum \nBinding", "cLogP", 
                                        "Human \nPlasma \nBinding", "Mouse \nPlasma \nBinding", 
                                        "MIC Erdman \nStrain", "MIC Erdman \nStrain \nwith Serum", "MIC Rv Strain",
                                        "Macrophage \nUptake (Ratio)"))) %>% 
    mutate(dosage_interval = factor(dosage_interval, levels = c("50BID", "100QD")))
  
  
  if(!is.null(variables)) {
    in_vitro_SM <- in_vitro_SM %>% 
      dplyr::filter(variable_filtered %in% variables)
  }
  
  if(!is.null(drugs)) {
    in_vitro_SM <- in_vitro_SM %>%
      dplyr::filter(Drugs %in% drugs)
  }
  
  
  in_vitro_SMplot <- in_vitro_SM %>% 
    ggplot(aes(x = dosage_interval, y = value, color = Drugs))+
    geom_beeswarm(alpha = 0.5, size = 1.5)+
    labs(x = 'Dosage-Interval', y = 'Value')+
    ggtitle('In-Vitro Distribution of TB Drugs')+
    theme_few()+
    facet_wrap(~ variable, ncol = 4, scale="free")
  
  return(in_vitro_SMplot)
  
}

invitro_beeswarm_function(efficacy_summary)

#example selection 
invitro_beeswarm_function(efficacy_summary, variables = c("cLogP", "Caseum_binding"),
                          drugs = c("DRUG1", "DRUG5"))