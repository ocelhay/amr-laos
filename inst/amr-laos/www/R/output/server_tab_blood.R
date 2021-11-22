output$filter_text_blood <- renderText({
  req(data_available())
  
  n_patients_start <- n_distinct(amr_blood()$patient_id)
  n_patients_end <- n_distinct(amr_blood_filt()$patient_id)
  n_specimens_start <- n_distinct(amr_blood()$spec_id)
  n_specimens_end <- n_distinct(amr_blood_filt()$spec_id)
  
  ifelse((n_patients_start == n_patients_end) & (n_specimens_start == n_specimens_end),
         paste0(div(class = "info", icon("info-circle", "fa-1x"), strong("Dataset with blood cultures only has not been filtered"), tags$ul( 
           tags$li("There are ", n_patients_start, " patients."),
           tags$li("There are ", n_specimens_start," specimens.")
         ))),
         paste0(div(class = "alert", icon("filter", "fa-1x"), strong("Dataset with blood cultures is filtered"), tags$ul( 
           tags$li("There are ", n_patients_end, " of the ", n_patients_start, " patients."),
           tags$li("There are ", n_specimens_end, " of the ", n_specimens_start, " specimens.")
         )
         ))
  )
})


output$province_specimen_blood <- renderPlot({
  req(nrow(amr_blood_filt()) > 0)
  
  amr_blood_filt() %>% 
    group_by(spec_id) %>% filter(row_number() == 1) %>% ungroup() %>%
    count(province) %>% mutate(province = fct_reorder(province, n, .desc = FALSE)) %>%
    ggplot(aes(x = province, weight = n)) + 
    geom_bar() +
    geom_label(aes(y = 0.95*n, label = n)) +
    labs(x = NULL, y = "Total Specimens", title = "Patients per Province", subtitle = "Blood Culture Only") +
    coord_flip() +
    theme_minimal(base_size = 15)
})

output$hospital_specimen_blood <- renderPlot({
  req(nrow(amr_blood_filt()) > 0)
  
  amr_blood_filt() %>% 
    group_by(spec_id) %>% filter(row_number() == 1) %>% ungroup() %>%
    count(location) %>% mutate(location = fct_reorder(location, n, .desc = FALSE)) %>%
    ggplot(aes(x = location, weight = n)) + 
    geom_bar() +
    geom_label(aes(y = 0.95*n, label = n)) +
    labs(x = NULL, y = "Total Specimens", title = "Patients per Hospital", subtitle = "Blood Culture Only") +
    coord_flip() +
    theme_minimal(base_size = 15)
})


output$growth_blood <- renderPlot({
  req(nrow(amr_blood_filt()) > 0)
  
  amr_blood_filt() %>% 
    group_by(spec_id) %>% filter(row_number() == 1) %>% ungroup() %>%
    mutate(growth = ifelse(org_name == "No growth", "No Growth", "Growth")) %>%
    count(growth) %>%
    ggplot(aes(x = growth, weight = n, group = growth)) + 
    geom_bar() +
    geom_label(aes(y = n, label = n)) +
    labs(x = NULL, y = NULL, title = NULL) +
    theme_minimal(base_size = 16) +
    theme(legend.position = "none", 
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
})



output$count_organisms_blood <- renderPlot({
  req(nrow(amr_blood_filt()) > 0)
  
  amr_blood_filt() %>%
    group_by(spec_id) %>% filter(row_number() == 1) %>% ungroup() %>%
    filter(org_name != "No growth") %>%
    count(org_name) %>%
    arrange(desc(n)) %>%
    head(n = 25) %>%
    mutate(org_name = fct_reorder(org_name, n, .desc = FALSE)) %>%
    ggplot(aes(x = org_name, weight = n)) + 
    geom_bar() +
    geom_label(aes(y = n, label = n)) +
    labs(x = NULL, y = "Organisms") +
    coord_flip() +
    theme_minimal(base_size = 15) +
    theme(axis.text = element_text(face = 'italic'))
})

output$table_organisms_blood <- renderDataTable({
  req(nrow(amr_blood_filt()) > 0)
  
  amr_blood_filt() %>% 
    group_by(spec_id) %>% filter(row_number() == 1) %>% ungroup() %>%
    filter(org_name != "No growth") %>%
    count(org_name) %>% mutate(org_name = fct_reorder(org_name, n, .desc = FALSE)) %>%
    arrange(desc(n)) %>%
    transmute(Organisms = paste0('<em>', org_name, '</em>'), Count = n) %>%
    datatable(rownames = FALSE, filter = "none", escape = FALSE, options = list(pageLength = 100, dom = 'ft'))
})