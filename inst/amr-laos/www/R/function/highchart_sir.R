highchart_sir <- function(data, organism) {
  
  excl_antibio <- bugantibio %>%
    filter(bug %in% organism, (display == "Never" | display == "Maybe")) %>%
    pull(antibio)
  
  total_tested <- data %>% 
    filter(org_name %in% organism, !is.na(antibiotic_name), !antibiotic_name %in% excl_antibio) %>% 
    count(antibiotic_name) %>%
    rename(total_org = n)
  
  sir_results <- data %>% 
    filter(org_name %in% organism, !is.na(antibiotic_name), !antibiotic_name %in% excl_antibio) %>% 
    count(antibiotic_name, resistance) %>%
    left_join(total_tested, by = "antibiotic_name") %>%
    mutate(percent = round(100*n / total_org, 1),
           resistance = case_when(
             resistance == "S" ~ "Susceptible",
             resistance == "I" ~ "Intermediate",
             resistance == "R" ~ "Resistant",
             TRUE ~ "Not Tested")) %>%
    mutate(resistance = factor(resistance, levels = c("Susceptible", "Intermediate", "Resistant", "Not Tested"))) %>%
    complete(resistance, nesting(antibiotic_name))
  
  return(
    hchart(sir_results, type = "bar", hcaes(x = "antibiotic_name", y = "percent", group = "resistance")) %>%
      hc_yAxis(title = list(text = "%"), max = 100) %>% hc_xAxis(title = "") %>%
      hc_colors(cols_sir) %>%
      hc_tooltip(headerFormat = "",
                 pointFormat = "<b>{point.antibiotic_name}</b><br> {point.resistance}: {point.percent}% <br>({point.n} of {point.total_org} tested.)") %>%
      hc_plotOptions(series = list(stacking = 'normal', dataLabels = list(enabled = TRUE, format = "{point.n}", align = "center")))
  )
}