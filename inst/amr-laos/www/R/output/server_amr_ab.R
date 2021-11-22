# "Acinetobacter species" ------------------------------------------------

output$organism_isolates_ab <- renderText({
  req(data_available())

  df <- amr_filt() %>% 
    filter(org_name |> startsWith("Acinetobacter")) 
  
  paste(h5(paste0("There are a total of ", n_distinct(df$spec_id), " distinct specimens from ", n_distinct(df$patient_id), " patients", " for Acinetobacter species.")))
})


# SIR Status ----------------------------------------------------------------------------------------------------------------

output$organism_sir_ab <- renderHighchart({
  req(data_available())
  
  selection_org <- amr_filt() |> 
    filter(org_name |> startsWith("Acinetobacter")) |> 
    pull(org_name) |> 
    unique()
  
  highchart_sir(data = amr_filt(), organism = selection_org)
})


# Carbapenem Status ---------------------------------------------------------------------------------------------------------------

output$carbapenem_ab <- renderHighchart({
  req(data_available())
  
  selection_org <- amr_filt() |> 
    filter(org_name |> startsWith("Acinetobacter")) |> 
    pull(org_name) |> 
    unique()
  
  highchart_sir_evolution(data = amr_filt(), organism = selection_org, 
                          antibiotic_vec = c("Imipenem", "Meropenem"),
                          levels = c("Carbapenem-susceptible", "Carbapenem-intermediate", "Carbapenem-resistant", "Not Tested"))
})

