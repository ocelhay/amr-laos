# Escherichia coli ----------------------------------------------------------------------------------------------------------

output$organism_isolates_ec <- renderText({
  
  organism <- "Escherichia coli"
  
  df <- amr_filt() %>% 
    filter(org_name == organism) 
  
  paste(h5(paste0("There are a total of ", n_distinct(df$spec_id), " distinct specimens from ", n_distinct(df$patient_id), " patients", " for ", organism, ".")))
})


# SIR Status ----------------------------------------------------------------------------------------------------------------

output$organism_sir_ec <- renderHighchart({
  
  
  highchart_sir(data = amr_filt(), organism = "Escherichia coli")

})



# ESBL Status ---------------------------------------------------------------------------------------------------------------

output$esbl_ec <- renderHighchart({
  
  
  organism <- "Escherichia coli"
  
  total_tested <- amr_filt() %>% 
    filter(org_name == organism) %>% 
    mutate(spec_quarter = round_date(spec_date, "3 months")) %>%
    group_by(spec_quarter) %>%
    summarise(total2 = n_distinct(spec_id)) %>%
    ungroup()
  
  esbl_results <- amr_filt() %>% 
    filter(org_name == organism) %>% 
    mutate(spec_quarter = round_date(spec_date, "3 months")) %>%
    group_by(spec_quarter, spec_id) %>%
    filter(row_number(spec_id) == 1) %>%
    ungroup() %>%
    group_by(spec_quarter, esbl) %>%
    count() %>%
    ungroup() %>%
    left_join(total_tested, by = "spec_quarter") %>%
    mutate(percent = round(100*n / total2, 1),
           resistance = case_when(
             esbl == "Positive" ~ "ESBL E. coli",
             esbl == "Negative" ~ "Non-ESBL E. coli",
             TRUE ~ "Not Tested")) %>%
    mutate(resistance = factor(resistance, levels = c("Non-ESBL E. coli", "ESBL E. coli", "Not Tested"))) %>%
    complete(resistance, nesting(spec_quarter)) %>%
    mutate(spec_quarter = as.character(quarter(spec_quarter, with_year = TRUE))) %>%
    mutate(spec_quarter = paste0(substr(spec_quarter, 1, 4), ", Quarter ", substr(spec_quarter, 6, 7)))
  
  return(
    hchart(esbl_results, type = "column", hcaes(x = "spec_quarter", y = "percent", group = "resistance")) %>%
      hc_yAxis(title = list(text = "%", rotation = 0), max = 100) %>% hc_xAxis(title = "") %>%
      hc_colors(cols_sir[c(1, 3, 4)]) %>%
      hc_tooltip(headerFormat = "",
                 pointFormat = "<b>{point.spec_quarter}</b><br> {point.resistance}: {point.percent}% <br>({point.n} of {point.total2} tested.)") %>%
      hc_plotOptions(series = list(stacking = 'normal', 
                                   dataLabels = list(enabled = TRUE,
                                                     formatter = JS("function() { return  this.point.n  + ' of ' + this.point.total2; }"))
      )
      )
  )
})