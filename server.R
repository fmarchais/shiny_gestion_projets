library(shiny)

server <- function(input, output) { 
  
  # Sidebars
  output$sidebar_etudes <- renderUI({
    tagList(
      conditionalPanel(condition = 'input.tabs=="Résumé des études"',
                       
                       sliderInput("choix_année","Choisissez l'année :", min = min_year, max = max_year, value = c(min_year,max_year)),
                       selectInput("type_projet", "Type Projet", liste_choix_projets, selected = "Academique",  multiple = TRUE),
                       selectInput("choix_intex", "Interne/externe : ", liste_choix_intex, selected = "Interne et externe"),
                       selectInput("choix_etat", "Etat : ", liste_choix_etats, selected = "Tout"),
                       selectInput("choix_by", "Analyse par :", liste_choix_by, selected = "Tout")
      ))
  })
  output$sidebar_filieres <- renderUI({
    tagList(
      conditionalPanel(condition = 'input.tabs=="Filières"',
                       sliderInput("choix_année_plot","Choisissez l'année :", min = min_year, max = max_year, value = c(min_year,max_year)),                 selectInput("type_projet_plot", "Type Projet", liste_choix_projets, multiple = TRUE, selected = "Academique"),
                       selectInput("choix_intex_plot", "Filtrer : ", liste_choix_intex, selected = "Interne et externe")
      ))
  })      
  
  output$sidebar_swimmer <- renderUI({
    tagList(
      conditionalPanel(condition = 'input.tabs=="Gestion des projets"',
                       sliderInput("swim_year","Période :", min = min_year_swim, max = max_year_swim, value = c(min_year_swim,max_year_swim)),
                       selectInput("swim_type_projet", "Type Projet", liste_choix_projets, multiple = TRUE, selected = "Academique"),
                       selectInput("swim_choix_intex", "Filtrer : ", liste_choix_intex, selected = "Interne et externe"),
                       selectInput("swim_choix_etat", "Etat : ", liste_choix_etats, selected = "Tout")
      ))
  })
  
  
  output$sidebar_calendar <- renderUI({
    tagList(
      conditionalPanel(condition = 'input.tabs=="Calendrier"',
                       sliderInput("calendar_year","Année :", min = min_year_calendar, max = max_year_calendar, value = current_year),
                       selectInput("calendar_type_projet", "Type Projet", liste_choix_projets, multiple = TRUE,
                                   selected = c("Academique", "Autre")),
                       selectInput("calendar_choix_intex", "Filtrer : ", liste_choix_intex, selected = "Interne et externe")
      ))
  })
  
  
  
  
  # Controlbars
  output$control_download_resume <- renderUI({
    tagList(
      conditionalPanel(condition = 'input.tabs=="Résumé des études"',
                       downloadButton("download", label = "Télécharger le rapport", class = NULL)
      ))
  })
  
  
  
  
  output$control_download_swimmer <- renderUI({
    tagList(
      conditionalPanel(condition = 'input.tabs=="Gestion des projets"',
                       downloadButton('download_swimmer','Télécharger le graphe')
      ))
  })  
  
  
  ### Server
  
  ### Résumé ---------------------------------------------------------------
  # INPUTS
  
  ## filtre interne/externe
  filtre_choix_intex <- reactive({
    if(input$choix_intex =="Interne et externe"){
      df$Origine %>% unique() %>% sort()
    } else {
      input$choix_intex
    }
  })
  
  ## filtre type de projet
  filtre_choix_projet <- reactive(input$type_projet)
  ## filtre état de projet
  filtre_choix_etat <- reactive({
    if(input$choix_etat == "Tout"){
      df$`Etat Prise en charge` %>% as.character() %>% unique() %>% sort()
    } else{
      input$choix_etat 
    }
  })
  
  
  # Créer la table filtrée et recodée
  tib_filtered <- reactive({
    df %>%
      filter(`Type Projet` %in% filtre_choix_projet()) %>%
      filter(`année_contrat` %in% input$choix_année[1]:input$choix_année[2]) %>%
      filter(Origine %in% filtre_choix_intex()) %>%
      filter(`Etat Prise en charge` %in% filtre_choix_etat()) %>%
      select(- c(`année_contrat`,
                 Organe,
                 `Date signature contrat/convention`,
                 `Date fin de contrat/projet`,
                 if(input$choix_intex == "Interne")  {
                   c("delai_signature",
                     "Date signature contrat/convention")
                 }
      )) %>%
      mutate_if(is.character, ~ case_when(
        is.na(.) ~ "<manquant>",
        TRUE ~ .
      )) %>%
      mutate_at(c("Avis Comité (date)", "Date de début de saisie", "Date de fin de saisie"), ~ ymd(.)) %>%
      # supprimer les colonnes qui seraient entièrement vides
      select(-  colnames(.)[colSums(is.na(.)) == nrow(.)]) %>%
      select(- c("Nom_projet"))
  })
  
  
  # gtsummary table
  my_gt_table <- reactive({
    tbl_summary(
      tib_filtered(),
      if(input$choix_by != "Tout"){
        by = input$choix_by
      },
      label = `Etat Prise en charge` ~ "Etat de prise en charge",
      sort = all_categorical() ~ "frequency",
      missing = "ifany",
      missing_text = "<manquant>",
      statistic = list(
        all_continuous() ~ "{mean} [{min};{max}]"
      )
    )  %>%
      as_gt() %>%
      tab_header(md(
        str_glue("Données de {input$choix_année[1]} à {input$choix_année[2]} \n 
        {input$choix_intex} \n 
        {paste0(input$type_projet, collapse = ',')} \n
        {if(input$choix_etat != 'Tout'){input$choix_etat}else{''}}"
        )
      )) 
  })
  
  # Output gtsummary
  output$my_gt_table <- render_gt({
    req(input$choix_année)
    my_gt_table()
  })
  

  
  
  
  
  
  ### SWIMMERPLOT -------------------------------------------------------------
  
  # FILTRES
  ## filtre année
  filtre_swim_year <- reactive(input$swim_year)
  
  ## filtre Interne/externe
  filtre_swim_intex  <- reactive({
    if(input$swim_choix_intex =="Interne et externe"){
      c("Externe", "Interne")
    } else {
      input$swim_choix_intex
    }
  })
  # Filtre projet
  filtre_swim_projet <- reactive(input$swim_type_projet)
  # filtre état
  filtre_swim_etat <- reactive({
    if(input$swim_choix_etat == "Tout"){
      df$`Etat Prise en charge` %>% as.character() %>% unique() %>% sort()
    } else{
      input$swim_choix_etat 
    }
  })
  
  # Data filtrées
  df_swim_filtered_reactive <- reactive({
    df_swim %>% 
      ## Filtres dynamiques
      filter(year(`Avis Comité (date)`) >= filtre_swim_year()[1] & 
               year(`Date fin de contrat/projet`) <= filtre_swim_year()[2]
      ) %>%
      filter(`Type Projet` %in% filtre_swim_projet() ) %>%
      filter(`Etat Prise en charge`%in% filtre_swim_etat()) %>%
      filter(Origine %in% filtre_swim_intex())
  })
  
  
  # Data pour lanes du swimmerplot
  df_swim_lanes_reactive <- reactive({
    bind_rows(
      df_swim_filtered_reactive() %>% 
        select(Nom_projet, `Etat Prise en charge`, deb_reglementaire, finreg_contrat) %>%
        rename(Debut = deb_reglementaire,
               Fin = finreg_contrat) %>%
        mutate(periode = "Règlementaire"),
      df_swim_filtered_reactive() %>% 
        select(Nom_projet, `Etat Prise en charge`, deb_ges_contrat, fin_ges_saisie) %>%
        rename(Debut = deb_ges_contrat,
               Fin = fin_ges_saisie) %>%
        mutate(periode = "Gestion des données"),
      df_swim_filtered_reactive() %>% 
        select(Nom_projet, `Etat Prise en charge`, deb_ana_saisie, fin_ana_proj) %>%
        rename(Debut = deb_ana_saisie,
               Fin = fin_ana_proj) %>%
        mutate(periode = "Analyse & valorisation")
    ) %>%
      arrange(Nom_projet)
  })
  
  
  
  # Data évènements uniques swimmerplot
  df_swim_events_reactive <- reactive({
    df_swim_filtered_reactive() %>%
      select(Nom_projet, contains('date'), contains('Date')) %>%
      pivot_longer(cols = str_subset(colnames(df_swim), "[Dd]ate"),
                   names_to = "event",
                   values_to = "date"
      ) %>%
      na.omit()
  })
  
  
  
  
  # Swimmerplot
  swimmerplot <- reactive({
    
    df_swim_lanes_reactive() %>% ggplot() +
      geom_swim_lane(
        mapping = aes(
          x = Debut, 
          y = Nom_projet, 
          xend = Fin,
          colour = periode
        )
      ) +
      geom_swim_marker(
        data = df_swim_events_reactive(),
        mapping = aes(x = date, y = Nom_projet, marker = event),
        size = 7
      ) +
      scale_colour_manual(
        values = c("Règlementaire" = "#E31A1C",
                   "Analyse & valorisation" = "#1FB538",
                   "Gestion des données" = "#1F78B4"), 
        name = "Période"
      )  +
      scale_marker_discrete(
        glyphs = c( "▶"  ,   "\U25C6"  ,  "\u25E2"  ,   "\u25BC"),
        colours = c("#25bbe8", "#FFD900", "black", "red"),
        limits = c("Avis Comité (date)", "Date signature contrat/convention", 
                   "Date de fin de saisie", "Date fin de contrat/projet"
        ),
        name = "Study Events"
      )+
      geom_vline(xintercept= as.POSIXct(today())) +
      xlab("Date") +
      ylab("Projets") +
      scale_y_discrete(limits=rev) +
      scale_x_datetime(labels = date_format("%Y-%b"),
                       breaks = waiver(),
                       date_breaks = "4 month", 
                       minor_breaks = "2 month",
                       sec.axis = dup_axis()) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      ggtitle(
        str_glue("Suivi des projets - ",
             if(input$swim_year[1] == input$swim_year[2]){
               '{input$swim_year[1]}'
             } else {
               '{input$swim_year[1]} à {input$swim_year[2]}'
             },
             '\n 
                     {input$swim_type_projet} - {input$swim_choix_etat}'
        )
      )
  })
  
  
  output$swimmerplot <- renderPlot(
    {
      swimmerplot()
    },
    height = 1000,
    units="px"
  )
  
  output$nrow_swimmer <- renderText({
    str_glue("Nombre de projets : {nrow(df_swim_filtered_reactive())}")
  })
  
  
  # Télécharger rapport
  output$download_swimmer <- downloadHandler(
    filename = function() {
      paste0('Gestion_projets_',Sys.Date(), collapse = '')
    },
    
    content = function(file) {
      ggsave(file, plot = swimmerplot(), device = "jpeg")
    }
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Filières -----------------------------------------------------------------
  
  ## Filtres
  
  ### Filtre Année
  year_plot <- reactive(input$choix_année_plot)
  ### Filtre type de projet
  filter_projet_plot <- reactive(input$type_projet_plot)
  
  ### Filtre Interne/externe
  my_filtre_plot <- reactive({
      if(input$choix_intex_plot =="Interne et externe"){
          df$Origine %>% unique() %>% sort()
      } else {
          input$choix_intex_plot
      }
  })
  
  
  
  
  ## GRAPHE
  ### Data du graphe
  df_plot <- reactive({
      df  %>%
          filter(`Type Projet` %in% filter_projet_plot()) %>%
          filter(année_contrat %in% year_plot()[1]:year_plot()[2]) %>%
          filter(Origine %in% my_filtre_plot()) %>%
          mutate(Filière = case_when(
              is.na(Filière) ~ "<manquant>",
              TRUE ~ Filière))
  })
  
  
  ### Titre du graphe
  output$plot_titre <- renderText({
      if(input$choix_année_plot[1] == input$choix_année_plot[2]) {
          annees <- str_glue("Données de {input$choix_année_plot[1]}")
      } else {
          annees <- str_glue("Données de {input$choix_année_plot[1]} à {input$choix_année_plot[2]}")
      } 
      
      str_glue("{annees} \n
          {input$choix_intex_plot} \n
          {paste0(input$type_projet_plot, collapse = ',')}
        ")
  })
  
  ### Output graphe        
  output$plot <- renderPlotly({
    
    ggplot(df_plot(), 
           aes(
             fill = as.factor(année_contrat),
             x = Filière)
    ) +
      geom_bar(position = "dodge", stat = "count") +
      scale_fill_brewer(palette="Spectral", name = "Année") +
      ylab("Nombre de projets")
    
  })
  
  ### nombre de projets
  n_row_plot <- reactive({
      nrow(df_plot())
  })
  ### output nombre de projets
  output$n_row_pie <- renderText({
      str_glue("Total de projets : {n_row_plot()}")
  })
  
  # Télécharger rapport
  output$download <- downloadHandler(
      filename = function() {
          paste0('Rapport-',Sys.Date(),".docx", collapse = '')
      },
      
      content = function(file) {
          gtsave(my_gt_table(), file)
      }
  )
  
  
  
  

  ## Filtres
  year_calendar <- reactive(input$calendar_year)
  ### Filtre type de projet
  filter_projet_calendar <- reactive(input$calendar_type_projet)
  
  ### Filtre Interne/externe
  filter_intex_calendar <- reactive({
    if(input$calendar_choix_intex =="Interne et externe"){
      df_calendar$Origine %>% unique() %>% sort()
    } else {
      input$calendar_choix_intex
    }
  })
  
  ## DATA
  reactive_df_calendar <-  reactive({
    
    # récupérer les dates des projets
    df_calendar_filtered <- df_calendar %>%
      filter(Origine %in% filter_intex_calendar()) %>%
      filter(`Type Projet` %in% filter_projet_calendar())
    
    # créer une table contenant les jours de l'année 
    # Pour chaque jour, compter son nombre d'apparition dans la table filtrée
    selected_year_calendar <- tibble(
      days = seq.Date(
        as.Date(str_glue("{year_calendar()}-01-01")),
        as.Date(str_glue("{year_calendar()}-12-31")),
        by = "day"
      )
    ) %>% 
      group_by(days) %>% 
      mutate(n_overlaps = sum(days %within% df_calendar_filtered$interval_projet) )
    
    
    return(selected_year_calendar)

  })
  

  ## GRAPHE
  output$calendar_plot <- renderPlot( {

    
    calendR(year = year_calendar(), # Affichage de l'année en haut
            special.days = reactive_df_calendar()$n_overlaps, # Valeurs pour chaque case
            gradient = TRUE, # Activer le gradient
            mbg.col = 4,   # background pour les noms des mois
            months.col = "white", # couleur du nom des mois
            special.col = rgb(1, 0.1, 0.1, alpha = 0.9), # couleur des gradient
            low.col = ifelse(
              min(reactive_df_calendar()$n_overlaps, na.rm = TRUE) == 0,
              "white",
              rgb(1, 0.9, 0.7, alpha = 0.9)
            ),
            legend.pos = "right",
            legend.title = "n projets",
            start = "M", # démarrer semaine au lundi
            title.size = 40,   # Title size
            orientation = "p", # Vertical orientation
            bg.img = "images/background.jpg"
    )
  })
  
  
  
}