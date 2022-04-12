# "All Organisms" ------------------------------------------------

output$organism_isolates_all <- renderText({
  
  organism <- input$organism
  
  df <- amr_filt() %>% 
    filter(org_name == organism) 
  
  paste(h5(paste0("There are a total of ", n_distinct(df$spec_id), " distinct specimens from ", n_distinct(df$patient_id), " patients", " for ", organism, ".")))
})


# SIR Status ----------------------------------------------------------------------------------------------------------------

output$organism_sir_all <- renderHighchart({
  
  
  organism <- input$organism
  
  highchart_sir(data = amr_filt(), organism = organism)
})
