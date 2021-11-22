
# Neisseria gonorrhoeae -----------------------------------------------------------------------------------------------------

output$organism_isolates_ng <- renderText({
  req(data_available())
  organism <- "Neisseria gonorrhoeae"
  
  df <- amr_filt() %>% 
    filter(org_name == organism) 
  
  paste(h5(paste0("There are a total of ", n_distinct(df$spec_id), " distinct specimens from ", n_distinct(df$patient_id), " patients", " for ", organism, ".")))
})


# SIR Status ----------------------------------------------------------------------------------------------------------------

output$organism_sir_ng <- renderHighchart({
  req(data_available())
  
  highchart_sir(data = amr_filt(), organism = "Neisseria gonorrhoeae")
})


# Ceftriaxone Status ---------------------------------------------------------------------------------------------------------------

output$ceftriaxone_gon <- renderHighchart({
  req(data_available())
  
  organism <- "Neisseria gonorrhoeae"
  
  highchart_sir_evolution(data = amr_filt(), organism = organism, 
                          antibiotic_vec = "Ceftriaxone",
                          levels = c("Ceftriaxone-susceptible", "Ceftriaxone-intermediate", "Ceftriaxone-resistant", "Not Tested"))
})