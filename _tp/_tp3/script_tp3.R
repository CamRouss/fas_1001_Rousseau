# 0.1 - Libraries ----
#install.packages("crayon")
library(crayon) # couleur du code
#install.packages("quanteda")
library(quanteda) # Package pour faire des analyses textuelles
#devtools::install_github("clessn/clessnverse")
library(clessnverse) # package avec la fonction pour rouler des dictionnaires
#install.packages("tidyverse")
suppressMessages(library(tidyverse)) # Data wrangling
library(readr)
library(dplyr)


# 0.2 - Données ----
chemin_annee_2015 <- "C:/Users/Camilien/OneDrive - Universite de Montreal/fas_1001_Rousseau/_tp/data/lipadcsv-1.1.0/lipad/2015"

dossiers_mois_2015 <- list.dirs(chemin_annee_2015, full.names = TRUE, recursive = 
                                  FALSE)
# Initialisez une liste vide pour stocker les fichiers CSV de chaque mois
fichiers_mois_2015 <- list()

# Parcourez chaque dossier mois et obtenez la liste des fichiers CSV
fichiers_tous_mois <- list.files(chemin_annee_2015, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)



# Importer les données:

# Importez tous les fichiers CSV dans une seule base de données
data_2015 <- do.call(rbind, lapply(fichiers_tous_mois, read.csv))

#View(data_2015)

# Dictionnaire 

lexicoder <- dictionary(file = "C:/Users/Camilien/OneDrive - Universite de Montreal/fas_1001_Rousseau/dictionnary/policy_agendas_english.lcd", format = "yoshikoder")


# 2 - Nettoyage des données ----

green_party_speeches <- data_2015 %>%
  filter(speakerparty == "Green", speakername == "Elizabeth May") %>%
  select(speechdate, speakername, maintopic, subtopic, speechtext) %>%
  mutate(
    speechtext = tolower(speechtext)
  ) %>% 
  na.omit()


# Analyse du dictionnaire par sujet 

ana_speech_green <- run_dictionary(data = green_party_speeches,
                                   text = speechtext,
                                   dictionary = lexicoder) %>% 
  bind_cols(green_party_speeches) %>% 
  select(-c(doc_id, speechtext)) %>% 
  pivot_longer(cols = -c(speechdate, maintopic, subtopic, speakername), names_to = "categorie", values_to = "n") %>% 
  ungroup() %>% 
  na.omit() %>% 
  group_by(speechdate, subtopic, categorie) %>% 
  summarise(n = sum(n)) %>% 
  mutate(prop = round((n/sum(n)) * 100, 4)) %>% 
  filter(categorie %in% c("macroeconomics","healthcare", "environnement", "agriculture", "energy"))
# 4 - Visualisation ----
## Graphique 1: 
library(lubridate)
library(ggplot2)

ana_speech_green$speechdate <- ymd(ana_speech_green$speechdate)

ggplot(ana_speech_green, aes(x = speechdate, y = prop, fill = categorie)) +
  geom_bar(stat = "identity") +
  labs(title = "Évolution des catégories de mots populaires au fil du temps",
       x = "Date du discours",
       y = "Proportion (%)",
       fill = "Sujets") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(title = "Catégorie de mots")) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "1 month")

## Graphique 2: 
# Trier les données par fréquence décroissante
ana_speech_green_sorted <- ana_speech_green %>%
  arrange(desc(n))

# Créer le graphique en barres horizontales
ggplot(ana_speech_green_sorted, aes(x = n, y = reorder(categorie, n))) +
  geom_bar(stat = "identity", fill = "#69b3a2") +
  labs(title = "Fréquence de mention des sujets",
       x = "Fréquence",
       y = "Sujets") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8)) 