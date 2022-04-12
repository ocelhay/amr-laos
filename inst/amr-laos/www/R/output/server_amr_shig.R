# "Shigella species" ------------------------------------------------

output$organism_isolates_shig <- renderText({
  
  
  df <- amr_filt() %>% 
    filter(org_name |> startsWith("Shig")) 
  
  paste(h5(paste0("There are a total of ", n_distinct(df$spec_id), " distinct specimens from ", n_distinct(df$patient_id), " patients", " for Shigella species.")))
})


# SIR Status ----------------------------------------------------------------------------------------------------------------

output$organism_sir_shig <- renderHighchart({
  
  
  selection_org <- amr_filt() |> 
    filter(org_name |> startsWith("Shig")) |> 
    pull(org_name) |> 
    unique()
  
  highchart_sir(data = amr_filt(), organism = selection_org)
})


# Ciprofloxacin Status ---------------------------------------------------------------------------------------------------------------

output$ciproflaxin_shig <- renderHighchart({
  
  
  selection_org <- amr_filt() |> 
    filter(org_name |> startsWith("Shig")) |> 
    pull(org_name) |> 
    unique()
  
  highchart_sir_evolution(data = amr_filt(), organism = selection_org, 
                          antibiotic_vec = "Ciprofloxacin",
                          levels = c("Ciprofloxacin-susceptible", "Ciprofloxacin-intermediate", "Ciprofloxacin-resistant", "Not Tested"))
})

