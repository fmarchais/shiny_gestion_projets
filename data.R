# transformations de données
library(readr)
library(lubridate)
library(dplyr)
library(tibble)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)
library(scales)
library(ggswim)
library(gt)
library(gtsummary)
library(plotly)
library(shinydashboard)
library(shinydashboardPlus)
library(emojifont)
library(calendR)
library(cleaner)


# Charger le jeu de données df
df <- read_csv2("data.csv")

# Vérifier formats dates
dates_a_corriger <- df %>%
  select(contains("date") & !where(is.Date)) %>%
  colnames()

df <- df %>%
  mutate_at(dates_a_corriger, ~ cleaner::clean_Date(., guess_each = TRUE, max_date = Inf))

# Variables uniques pour UI ----------------------------------------------
current_year <- year(today())

liste_années <- df$année_contrat  %>% as.character() %>% unique() %>% sort()

min_year <- as.numeric(min(liste_années,na.rm = TRUE))

max_year <- as.numeric(max(liste_années, na.rm = TRUE))

liste_choix_etats <- c("Tout", unique(df$`Etat Prise en charge`)  )

liste_choix_projets <- unique(df$`Type Projet`) 

liste_choix_années <- append(list("global" = liste_années), as.list(liste_années) %>% setNames(liste_années))

liste_choix_intex <- c("Interne et externe","Externe","Interne")

liste_choix_by <- c("Tout", "Origine","Type données")










# SWIMMERPLOT -------------------------------------------------------

# Data pour swimmerplot
df_swim <- df %>%
  # Retirer les projets pas démarrés ou terminés
  filter(!`Etat Prise en charge` %in% c("Cadrage","Archivé")) %>% 
  mutate_at(
    c("Date signature contrat/convention",
      "Date fin de contrat/projet"
    ), ~ as.POSIXct(.)
  ) %>%
  mutate("deb_reglementaire" = `Avis Comité (date)`,
         "finreg_contrat" =`Date signature contrat/convention`,
         "deb_ges_contrat" = `Date signature contrat/convention`,
         "fin_ges_saisie" = `Date de fin de saisie`,
         "deb_ana_saisie" = `Date de fin de saisie`,
         "fin_ana_proj" = `Date fin de contrat/projet`
  ) %>%
  mutate(Nom_projet = as.factor(Nom_projet))%>%
  arrange(Nom_projet)


min_year_swim <- min(year(df_swim$`Avis Comité (date)`), na.rm = TRUE)
max_year_swim <- max(year(df_swim$`Date fin de contrat/projet`), na.rm = TRUE)




# CALENDRIER ----------------------------
df_calendar <- df %>%
  select(`Date signature contrat/convention`, `Date fin de contrat/projet`, Origine, `Type Projet`) %>%
  mutate(interval_projet = interval(`Date signature contrat/convention`, `Date fin de contrat/projet`))


## valeurs pour filtres

min_year_calendar <- min(year(df_calendar$`Date signature contrat/convention`), na.rm = TRUE)
max_year_calendar <- max(year(df_calendar$`Date fin de contrat/projet`), na.rm = TRUE)
