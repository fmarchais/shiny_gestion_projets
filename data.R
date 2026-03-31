# transformations de données
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
df <- as_tibble(read.csv2("data.csv"))

# Vérifier formats dates
dates_a_corriger <- df %>%
  select(contains("date") & !where(is.Date)) %>%
  colnames()

df <- df %>%
  mutate_at(
    dates_a_corriger,
    ~ cleaner::clean_Date(., guess_each = TRUE, max_date = Inf)
  )

# Variables uniques pour UI ----------------------------------------------
current_year <- year(today())

liste_années <- df$annee_contrat %>% as.character() %>% unique() %>% sort()

min_year <- as.numeric(min(liste_années, na.rm = TRUE))

max_year <- as.numeric(max(liste_années, na.rm = TRUE))

liste_choix_etats <- c("Tout", unique(df$etat_prise_en_charge))

liste_choix_projets <- unique(df$type_projet)

liste_choix_années <- append(
  list("global" = liste_années),
  as.list(liste_années) %>% setNames(liste_années)
)

liste_choix_intex <- c("Interne et externe", "Externe", "Interne")

liste_choix_by <- c(
  "Tout" = "Tout",
  "Origine" = "origine",
  "Type de Données" = "type_donnees"
)


# SWIMMERPLOT -------------------------------------------------------

# Data pour swimmerplot
df_swim <- df %>%
  # Retirer les projets pas démarrés ou terminés
  filter(!etat_prise_en_charge %in% c("Cadrage", "Archivé")) %>%
  mutate_at(
    c("date_signature_contrat_convention", "date_fin_contrat"),
    ~ as.POSIXct(.)
  ) %>%
  mutate(
    "deb_reglementaire" = avis_comite_date,
    "finreg_contrat" = date_signature_contrat_convention,
    "deb_ges_contrat" = date_signature_contrat_convention,
    "fin_ges_saisie" = date_fin_saisie,
    "deb_ana_saisie" = date_fin_saisie,
    "fin_ana_proj" = date_fin_contrat
  ) %>%
  mutate(nom_projet = as.factor(nom_projet)) %>%
  arrange(nom_projet)


min_year_swim <- min(year(df_swim$avis_comite_date), na.rm = TRUE)
max_year_swim <- max(year(df_swim$date_fin_contrat), na.rm = TRUE)


# CALENDRIER ----------------------------
df_calendar <- df %>%
  select(
    date_signature_contrat_convention,
    date_fin_contrat,
    origine,
    type_projet
  ) %>%
  mutate(
    interval_projet = interval(
      date_signature_contrat_convention,
      date_fin_contrat
    )
  )


## valeurs pour filtres

min_year_calendar <- min(
  year(df_calendar$date_signature_contrat_convention),
  na.rm = TRUE
)
max_year_calendar <- max(
  year(df_calendar$date_fin_contrat),
  na.rm = TRUE
)
