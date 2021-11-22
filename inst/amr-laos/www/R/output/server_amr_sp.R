# "Streptococcus pneumoniae" ------------------------------------------------

output$organism_isolates_sp <- renderText({
  req(data_available())
  organism <- "Streptococcus pneumoniae"
  
  df <- amr_filt() %>% 
    filter(org_name == organism) 
  
  paste(h5(paste0("There are a total of ", n_distinct(df$spec_id), " distinct specimens from ", n_distinct(df$patient_id), " patients", " for ", organism, ".")))
})


# SIR Status ----------------------------------------------------------------------------------------------------------------

output$organism_sir_sp <- renderHighchart({
  req(data_available())
  
  highchart_sir(data = amr_filt(), organism = "Streptococcus pneumoniae")
})


# Penicilin Status ---------------------------------------------------------------------------------------------------------------

output$penicilin_sp <- renderHighchart({
  req(data_available())
  
  highchart_sir_evolution(data = amr_filt(), organism = "Streptococcus pneumoniae", 
                          antibiotic_vec = "Penicillin G", levels = c("Penicillin-susceptible", "Penicillin-intermediate", "Penicillin-resistant", "Not Tested")) 
})