source("www/R/startup.R", local = TRUE)

# Define UI ----
ui <- page(
  theme = amr_theme,
  includeCSS("www/styles.css"),
  page_navbar(
    title = "Lao AMR Dashboard",
    id = "tabs",
    selected = "welcome",
    window_title = "Lao AMR Dashboard",
    collapsible = TRUE, inverse = FALSE, 
    position = "static-top",
    
    # Header with filters -----------------------------------------------------
    header = div(conditionalPanel(
      condition = "input.tabs != 'welcome'",
      div(id = "header-filter",
          div(class = "border4",
              fluidRow(
                column(3, htmlOutput("data_status_duplicated")),
                column(3, 
                       h3(icon("filter", "fa-1x"), "Filter the Dataset:"),
                       
                       fluidRow(
                         column(4, p("Patients Age Range (Year):")),
                         column(8, sliderInput("age_patients_selection", label = NULL, min = 0, max = oldest_patient, value = c(0, oldest_patient)))
                       ),
                       fluidRow(
                         column(4, p("Patients Province of Residence:")),
                         column(8, pickerInput(inputId = "province_patients_selection", label = NULL, multiple = TRUE,
                                               choices = all_provinces, selected = all_provinces, options = list(
                                                 `actions-box` = TRUE, `deselect-all-text` = "None...",
                                                 `select-all-text` = "Select All", `none-selected-text` = "None Selected")
                         ))
                       )
                ),
                column(3,
                       fluidRow(
                         column(4, p("Specimens Collection Date:")),
                         column(8, selectInput("date_range_selection", label = NULL, choices = c("Filter by Year", "Filter by Date Range")),
                                conditionalPanel("input.date_range_selection == 'Filter by Year'",
                                                 checkboxGroupInput("year_selection", label = NULL, choices = all_spec_year, selected = all_spec_year, inline = TRUE)
                                                 
                                ),
                                conditionalPanel("input.date_range_selection == 'Filter by Date Range'",
                                                 br(), br(),
                                                 dateRangeInput("date_selection", label = NULL, start = min_collection_date, end = max_collection_date)
                                ))
                       )
                ),
                column(3,
                       fluidRow(
                         column(4, p("Specimens Collection Location:")),
                         column(8, pickerInput(inputId = "spec_method_collection", label = NULL, multiple = TRUE,
                                               choices = all_locations, selected = all_locations, options = list(
                                                 `actions-box` = TRUE, `deselect-all-text` = "None...",
                                                 `select-all-text` = "Select All", `none-selected-text` = "None Selected")),
                                conditionalPanel(condition = "input.tabs == 'blood_culture'",
                                                 fluidRow(
                                                   column(4, p("Specimens Collection Method:")),
                                                   column(8, strong("Blood Culture Only"))
                                                 )
                                ),
                                conditionalPanel(condition = "input.tabs == 'patients' | input.tabs == 'specimens' | input.tabs == 'organisms' | input.tabs == 'amr'",
                                                 fluidRow(
                                                   column(4, p("Specimens Collection Method:")),
                                                   column(8, pickerInput(inputId = "spec_method_selection", label = NULL, multiple = TRUE,
                                                                         choices = all_spec_method, selected = all_spec_method, options = list(
                                                                           `actions-box` = TRUE, `deselect-all-text` = "None...",
                                                                           `select-all-text` = "Select All", `none-selected-text` = "None Selected")
                                                   )
                                                   )
                                                 )
                                ),
                                conditionalPanel(condition = "input.tabs == 'patients' | input.tabs == 'specimens' | input.tabs == 'organisms' | input.tabs == 'amr'",
                                                 htmlOutput("filter_text")
                                ),
                                conditionalPanel(condition = "input.tabs == 'blood_culture'",
                                                 htmlOutput("filter_text_blood")
                                )
                         )
                       )
                ))
          ))
    )
    ),
    
    # Start Tabs --------------------------------------------------------------
    nav("Welcome", value = "welcome",
        fluidRow(
          column(4,
                 div(class = "imgsolidborder4", img(src = "img_LOMWRU_Partners.jpg", alt = "LOMWRU")),
                 div(class = "large",
                     selectInput("selected_language", span(icon("language"), "Language"),
                                 choices = c("Lao" = "la", "English" = "en"), selected = "en", width = "150px"),
                 )),
          column(8,
                 htmlOutput("data_status")
          )
        ),
        fluidRow(
          column(4,
                 div(class = "imgsolidborder4", img(src = "img_ecoli_LOMWRU.png", alt = "Antibiotic susceptibility testing of a multi-drug resistant Escherichia coli isolated from the urine of a 51 year old Lao patient with a perinephric abscess. There are no inhibition zones surrounding any of the antibiotic disks, including meropenem (MEM, 12 o’clock position), a ‘last-line’ antibiotic. Whole-genome sequencing confirmed that this isolate was carrying a NDM-5 carbapenemase. Such infections are likely to become more frequent, given the ability of carbapenemases to spread and the increasing availability of meropenem in Laos.")),
                 conditionalPanel("input.selected_language == 'en'",
                                  includeMarkdown("./www/markdown/ecoli_legend_en.md")
                 ),
                 conditionalPanel("input.selected_language == 'la'",
                                  includeMarkdown("./www/markdown/ecoli_legend_la.md")
                 )
          ),
          column(8,
                 
                 conditionalPanel("input.selected_language == 'en'",
                                  includeMarkdown("./www/markdown/about_en.md")),
                 conditionalPanel("input.selected_language == 'la'",
                                  includeMarkdown("./www/markdown/about_la.md"))
          )
        )
    ),
    nav("Patients", value = "patients",
        h2("Total Patients per Place of Collection"),
        plotOutput("patients_nb", height = "600px") %>% withSpinner()
    ),
    nav("Blood Culture", value = "blood_culture",
        fluidRow(
          column(2, h2("Sample Growth")),
          column(10, div(class = "cent", h2("Specimens Origins")))
        ),
        fluidRow(
          column(2,
                 br(),
                 br(),
                 plotOutput("growth_blood", height = "250px") %>% withSpinner()
          ),
          column(5,
                 plotOutput("province_specimen_blood") %>% withSpinner()
          ),
          column(5,
                 plotOutput("hospital_specimen_blood") %>% withSpinner()
          )
        ),
        h2("Organism"),
        fluidRow(
          column(8,
                 p("The graph below displays the 25 most commons organisms. Please see table to right for complete listing."),
                 plotOutput("count_organisms_blood", height = "600px") %>% withSpinner()
          ),
          column(4,
                 p("Table of all organisms:"),
                 DTOutput("table_organisms_blood") %>% withSpinner()
          )
        ),
        br()
    ),
    nav("Specimens", value = "specimens",
        h2("Total Specimens per Specimen Type"),
        p("Use filters located on the the sidebar to select and display, for example, only specimens collected in a specific hospital."),
        plotOutput("specimens_method", height = "600px") %>% withSpinner()
    ),
    nav("Organisms", value = "organisms",
        h2("Total Number of Positive Isolates per Specimen"),
        plotOutput("isolates_method", height = "600px") %>% withSpinner(),
        h2("Total Number of Isolates by Organism"),
        fluidRow(
          column(8, 
                 p("The graph below displays the 25 organisms with the most isolates, report to the table for the complete listing."),
                 plotOutput("isolates_organisms", height = "600px") %>% withSpinner()),
          column(4, dataTableOutput("table_isolates_organisms") %>% withSpinner())
        )
    ),
    nav("AMR", value = "amr", icon = icon("bug"),
        tabsetPanel(
          tabPanel("Acinetobacter species",
                   br(),
                   fluidRow(
                     htmlOutput("organism_isolates_ab"),
                     column(8,
                            h2("Susceptibility Status"),
                            highchartOutput("organism_sir_ab", height = "600px") %>% withSpinner()
                     ),
                     column(4,
                            h3("Carbapenem-resistant Acinetobacter species"),
                            highchartOutput("carbapenem_ab", height = "500px") %>% withSpinner()
                            
                     )
                   )
          ),
          tabPanel("E. coli",
                   br(),
                   fluidRow(
                     htmlOutput("organism_isolates_ec"),
                     column(6,
                            h2("Susceptibility Status"),
                            highchartOutput("organism_sir_ec", height = "600px") %>% withSpinner()
                     ),
                     column(6,
                            h2("ESBL Results per Quarter"),
                            highchartOutput("esbl_ec", height = "600px") %>% withSpinner()
                     )
                   )
          ),
          tabPanel("K. pneumoniae",
                   br(),
                   fluidRow(
                     htmlOutput("organism_isolates_kp"),
                     column(6,
                            h2("Susceptibility Status"),
                            highchartOutput("organism_sir_kp", height = "600px") %>% withSpinner()
                     ),
                     column(6,
                            h2("ESBL Results per Quarter"),
                            highchartOutput("esbl_kp", height = "600px") %>% withSpinner()
                     )
                   )
          ),
          tabPanel("S. aureus",
                   br(),
                   fluidRow(
                     htmlOutput("organism_isolates_sa"),
                     column(6,
                            h2("Susceptibility Status"),
                            highchartOutput("organism_sir_sa", height = "600px") %>% withSpinner()
                     ),
                     column(6,
                            h2("MRSA per Quarter"),
                            highchartOutput("organism_mrsa_sa", height = "600px") %>% withSpinner()
                     )
                   )
          ),
          tabPanel("S. pneumoniae",
                   br(),
                   fluidRow(
                     htmlOutput("organism_isolates_sp"),
                     column(8,
                            h2("Susceptibility Status"),
                            highchartOutput("organism_sir_sp", height = "600px") %>% withSpinner()
                     ),
                     column(4,
                            h3("Penicillin-resistant S. pneumoniae"),
                            highchartOutput("penicilin_sp", height = "500px") %>% withSpinner()
                     )
                   )
          ),
          tabPanel("Salmonella Typhi",
                   br(),
                   fluidRow(
                     htmlOutput("organism_isolates_st"),
                     column(8,
                            h2("Susceptibility Status"),
                            highchartOutput("organism_sir_st", height = "600px") %>% withSpinner()
                     ),
                     column(4,
                            h3("Ciprofloxacin-resistant Salmonella Typhi"),
                            highchartOutput("ciprofloxacin_st", height = "500px") %>% withSpinner()
                     )
                   )
          ),
          tabPanel("Shigella spp.",
                   br(),
                   fluidRow(
                     htmlOutput("organism_isolates_shig"),
                     column(8,
                            h2("Susceptibility Status"),
                            highchartOutput("organism_sir_shig", height = "600px") %>% withSpinner()
                     ),
                     column(4,
                            h3("Ciprofloxacin-resistant Shigella spp."),
                            highchartOutput("ciproflaxin_shig", height = "500px") %>% withSpinner()
                     )
                   )
          ),
          tabPanel("Neisseria gonorrhoeae",
                   br(),
                   fluidRow(
                     htmlOutput("organism_isolates_ng"),
                     column(8,
                            h2("Susceptibility Status"),
                            highchartOutput("organism_sir_ng", height = "600px") %>% withSpinner()
                     ),
                     column(4,
                            h3("Ceftriaxone-resistant N. gonorrhoeae"),
                            highchartOutput("ceftriaxone_gon", height = "500px") %>% withSpinner()
                     )
                   )
          ),
          tabPanel("All Organisms",
                   br(),
                   pickerInput(inputId = "organism", label = NULL, multiple = FALSE,
                               choices = all_org_name, selected = all_org_name[1]),
                   br(),
                   fluidRow(
                     htmlOutput("organism_isolates_all"),
                     column(8,
                            h2("Susceptibility Status"),
                            highchartOutput("organism_sir_all", height = "600px") %>% withSpinner()
                     ),
                     column(4,
                            br()
                     )
                   )
          )
        )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  # Report
  # https://stackoverflow.com/questions/51050306/have-downloadbutton-work-with-observeevent
  feedback_download <- reactiveValues(download_flag = 0)
  
  output$report <- downloadHandler(
    filename = "AMR Report.pdf",
    content = function(file) {
      feedback_download$download_flag <- feedback_download$download_flag + 1
      if(feedback_download$download_flag > 0) {
        showNotification(HTML("Generation of the report typically takes 10 to 30 seconds"), duration = NULL, type = "message", id = "report_generation", session = session)
      }
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("./www/report/report.Rmd", tempReport, overwrite = TRUE)
      rmarkdown::render(tempReport, output_file = file)
      removeNotification(id = "report_generation", session = session)
      showNotification(HTML("Report Generated"), duration = 4, type = "message", id = "report_generated", session = session)
    }
  )
  
  # Initiate reactive values
  data_available <- reactiveVal(TRUE)
  amr <- reactiveVal(lims$amr)
  source_data <- reactiveVal("LOMWRU")
  date_generation <- reactiveVal(lims$meta$generate)
  
  # Hide several tabs at the launch of the app
  # observeEvent(NULL, {
  #   hideTab(inputId = "tabs", target = "blood_culture")
  #   hideTab(inputId = "tabs", target = "patients")
  #   hideTab(inputId = "tabs", target = "specimens")
  #   hideTab(inputId = "tabs", target = "organisms")
  #   hideTab(inputId = "tabs", target = "amr")
  # }, ignoreNULL = FALSE)
  
  # Create a reactive dataframe applying filters to amr
  amr_filt <- reactive({
    
    if(input$date_range_selection == "Filter by Year"){
      return(
        amr() %>%
          filter(
            age_years >= input$age_patients_selection[1] | is.na(age_years),
            age_years <= input$age_patients_selection[2] | is.na(age_years),
            province %in% input$province_patients_selection | is.na(province),
            spec_year %in% input$year_selection | is.na(spec_year),
            spec_method %in% input$spec_method_selection | is.na(spec_method),
            location %in% input$spec_method_collection | is.na(location)
          )
      )
    }
    
    if(input$date_range_selection == "Filter by Date Range"){
      return(
        amr() %>%
          filter(
            age_years >= input$age_patients_selection[1] | is.na(age_years),
            age_years <= input$age_patients_selection[2] | is.na(age_years),
            province %in% input$province_patients_selection | is.na(province),
            spec_date >= input$date_selection[1] | is.na(spec_date),
            spec_date <= input$date_selection[2] | is.na(spec_date),
            spec_method %in% input$spec_method_selection | is.na(spec_method),
            location %in% input$spec_method_collection | is.na(location)
          )
      )
    }
  })
  
  amr_blood <- reactive({
    amr() %>%
      filter(spec_method == "Blood culture")
  })
  
  
  # Create a reactive dataframe applying filters to amr_blood
  amr_blood_filt <- reactive({
    
    if(input$date_range_selection == "Filter by Year"){
      return(
        amr() %>%
          filter(spec_method == "Blood culture") %>%
          filter(
            age_years >= input$age_patients_selection[1] | is.na(age_years),
            age_years <= input$age_patients_selection[2] | is.na(age_years),
            province %in% input$province_patients_selection | is.na(province),
            spec_year %in% input$year_selection | is.na(spec_year),
            location %in% input$spec_method_collection  | is.na(spec_method)
          )
      )
    }
    
    if(input$date_range_selection == "Filter by Date Range"){
      return(
        amr() %>%
          filter(spec_method == "Blood culture") %>%
          filter(
            age_years >= input$age_patients_selection[1] | is.na(age_years),
            age_years <= input$age_patients_selection[2] | is.na(age_years),
            province %in% input$province_patients_selection | is.na(province),
            spec_date >= input$date_selection[1] | is.na(spec_date),
            spec_date <= input$date_selection[2] | is.na(spec_date),
            location %in% input$spec_method_collection  | is.na(spec_method)
          )
      )
    }
  })
  
  
  
  
  # Render Text on number of specimens
  
  # List of information on the status of data
  output$data_status <- renderText({
    
    ifelse(data_available(),
           paste0(div(class = "info2", h4(icon("info-circle", "fa-1x"), "Data uploaded"), tags$ul( 
             tags$li("Dataset: ", source_data()),
             tags$li("Dataset generated on the ", date_generation()),
             tags$li("Dataset contains ", n_distinct(amr()$patient_id), " patients", " and ", n_distinct(amr()$spec_id)," specimens."))
           )),
           paste0(div(class = "alert", h4(icon("exclamation-triangle", "fa-1x"), "There is no data to display"), "Upload a dataset provided by LOMWRU or 'Use Mock Dataset'."))
    )
  })
  
  output$data_status_duplicated <- renderText({
    
    ifelse(data_available(),
           paste0(div(class = "info2", h4(icon("info-circle", "fa-1x"), "Data uploaded"), tags$ul( 
             tags$li("Dataset: ", source_data()),
             tags$li("Dataset generated on the ", date_generation()),
             tags$li("Dataset contains ", n_distinct(amr()$patient_id), " patients", " and ", n_distinct(amr()$spec_id)," specimens."))
           )),
           paste0(div(class = "alert", h4(icon("exclamation-triangle", "fa-1x"), "There is no data to display,"), br(), "Upload a dataset provided by LOMWRU or 'Use Mock Dataset'."))
    )
  })
  
  
  output$filter_text <- renderText({
    req(data_available())
    
    n_patients_start <- n_distinct(amr()$patient_id)
    n_patients_end <- n_distinct(amr_filt()$patient_id)
    n_specimens_start <- n_distinct(amr()$spec_id)
    n_specimens_end <- n_distinct(amr_filt()$spec_id)
    
    if(n_patients_start != n_patients_end | (n_specimens_start != n_specimens_end)){
      return(
        # paste0(div(class = "info", icon("info-circle", "fa-1x"), strong("Dataset has not been filtered"), tags$ul( 
        #   tags$li("There are ", n_patients_start, " patients."),
        #   tags$li("There are ", n_specimens_start," specimens.")
        # )
        paste0(div(class = "alert", icon("filter", "fa-1x"), strong("Dataset is filtered"), tags$ul(
          tags$li("There are ", n_patients_end, " of the ", n_patients_start, " patients."),
          tags$li("There are ", n_specimens_end, " of the ", n_specimens_start, " specimens.")
        )
        ))
      )
    }
    
    return(NULL)
  })
  
  
  source("www/R/output/server_tab_blood.R", local = TRUE)
  
  
  
  # Patient Tab ---------------------------------------------------------------------------------------------------------------
  
  output$patients_nb <- renderPlot({
    req(nrow(amr_filt()) > 0)
    
    amr_filt() %>% 
      group_by(location) %>% summarise(count = n_distinct(patient_id)) %>% ungroup() %>%
      mutate(location = fct_reorder(location, count, .desc = FALSE)) %>%
      ggplot(aes(x = location, weight = count)) + 
      geom_bar() +
      geom_label(aes(y = count, label = count)) +
      coord_flip() +
      labs(x = NULL, y = "Total Patients", x = "Collection Place") +
      theme_minimal(base_size = 16)
  })
  
  
  # Specimens Tab -------------------------------------------------------------------------------------------------------------
  
  output$specimens_method <- renderPlot({
    req(nrow(amr_filt()) > 0)
    
    amr_filt() %>% 
      group_by(spec_id) %>% filter(row_number() == 1) %>% ungroup() %>%
      count(spec_method) %>% mutate(spec_method = fct_reorder(spec_method, n, .desc = FALSE)) %>%
      ggplot(aes(x = spec_method, weight = n)) + 
      geom_bar() +
      geom_label(aes(y = n, label = n)) +
      coord_flip() +
      labs(x = NULL, y = "Total Specimens", x = "Collection Method") +
      theme_minimal(base_size = 16)
  })
  
  
  # Organisms tab -----------------------------------------------------------
  output$isolates_method <- renderPlot({
    req(nrow(amr_filt()) > 0)
    
    amr_filt() %>% 
      filter(org_name != "No growth") %>%
      group_by(spec_method) %>%
      summarise(n = length(unique(spec_id))) %>%
      ungroup() %>%
      mutate(spec_method = fct_reorder(spec_method, n, .desc = FALSE)) %>%
      ggplot(aes(x = spec_method, weight = n)) + 
      geom_bar() +
      geom_label(aes(y = n, label = n)) +
      coord_flip() +
      labs(x = NULL, y = "Total Isolates", x = "Collection Method") +
      theme_minimal(base_size = 16)
  })
  
  
  output$isolates_organisms <- renderPlot({
    req(nrow(amr_filt()) > 0)
    
    amr_filt() %>%
      filter(org_name != "No growth") %>%
      group_by(org_name) %>% 
      summarise(n = length(unique(spec_id))) %>%
      ungroup() %>%
      arrange(desc(n)) %>%
      head(n = 25) %>%
      mutate(org_name = fct_reorder(org_name, n, .desc = FALSE)) %>%
      ggplot(aes(x = org_name, weight = n)) + 
      geom_bar() +
      geom_label(aes(y = n, label = n)) +
      labs(x = NULL, y = "Total Isolates") +
      coord_flip() +
      theme_minimal(base_size = 15) +
      theme(axis.text.y = element_text(face = 'italic'))
  })
  
  output$table_isolates_organisms <- renderDT({
    req(nrow(amr_filt()) > 0)
    
    amr_filt() %>%
      filter(org_name != "No growth") %>%
      group_by(org_name) %>% 
      summarise(n = length(unique(spec_id))) %>%
      ungroup() %>%
      arrange(desc(n)) %>%
      transmute(Organisms = paste0('<em>', org_name, '</em>'), Count = n) %>%
      datatable(rownames = FALSE, filter = "none", escape = FALSE, options = list(pageLength = 100, dom = 'ft'))
  })
  
  # AMR Tab -----------------------------------------------------------------
  
  source("www/R/output/server_amr_ab.R", local = TRUE)
  source("www/R/output/server_amr_ec.R", local = TRUE)
  source("www/R/output/server_amr_kp.R", local = TRUE)
  source("www/R/output/server_amr_sa.R", local = TRUE)
  source("www/R/output/server_amr_sp.R", local = TRUE)
  source("www/R/output/server_amr_st.R", local = TRUE)
  source("www/R/output/server_amr_shig.R", local = TRUE)
  source("www/R/output/server_amr_ng.R", local = TRUE)
  source("www/R/output/server_amr_any.R", local = TRUE)
}

# Return the App ----
shinyApp(ui = ui, server = server)
