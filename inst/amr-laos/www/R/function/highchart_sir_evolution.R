highchart_sir_evolution <- function(data, organism, antibiotic_vec, levels) {
  
  total_tested <- data %>%
    filter(org_name %in% organism, antibiotic_name %in% antibiotic_vec) %>% 
    mutate(spec_quarter = round_date(spec_date, "3 months")) %>%
    group_by(spec_quarter) %>%
    summarise(total2 = n_distinct(spec_id)) %>%
    ungroup()
  
  evol_results <- data %>%
    filter(org_name %in% organism, antibiotic_name %in% antibiotic_vec) %>% 
    mutate(spec_quarter = round_date(spec_date, "3 months")) %>%
    group_by(spec_quarter, spec_id) %>%
    filter(row_number(spec_id) == 1) %>%
    ungroup() %>%
    group_by(spec_quarter, resistance) %>%
    count() %>%
    ungroup() %>%
    left_join(total_tested, by = "spec_quarter") %>%
    mutate(percent = round(100*n / total2, 1),
           resistance = case_when(
             resistance == "S" ~ levels[1],
             resistance == "I" ~ levels[2],
             resistance == "R" ~ levels[3],
             TRUE ~ levels[4])) %>%
    mutate(resistance = factor(resistance, levels = levels)) %>%
    complete(resistance, nesting(spec_quarter)) %>%
    mutate(spec_quarter = as.character(quarter(spec_quarter, with_year = TRUE))) %>%
    mutate(spec_quarter = paste0(substr(spec_quarter, 1, 4), ", Quarter ", substr(spec_quarter, 6, 7)))
  
  return(
    hchart(evol_results, type = "column", hcaes(x = "spec_quarter", y = "percent", group = "resistance")) %>%
      hc_yAxis(title = list(text = "%", rotation = 0), max = 100) %>% hc_xAxis(title = "") %>%
      hc_colors(cols_sir) %>%
      hc_tooltip(headerFormat = "",
                 pointFormat = "<b>{point.spec_quarter}</b><br> {point.resistance}: {point.percent}% <br>({point.n} of {point.total2} tested.)") %>%
      hc_plotOptions(series = list(stacking = 'normal', 
                                   dataLabels = list(enabled = TRUE,
                                                     formatter = JS("function() { return  this.point.n  + ' of ' + this.point.total2; }"))
      )
      )
  )
}