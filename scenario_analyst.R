library(plotly)
library(wordcloud2)
library(igraph)
library(ggraph)
library(tidygraph)
library(tidyverse)
library(visNetwork)
########plotly####################### Analyse des scenarios ############################

############## Scenario en anglais
path_data_en_AFB23 <- file.path(path_data,"en_info_data_AFB2023.json")
# read data2
info_data_en_AFB2023 <- fromJSON(path_data_en_AFB23)
# info_data_en_AFB2023$Start_time_discusion <- ymd_hms(info_data_en_AFB2023$Start_time_discusion)
# info_data_en_AFB2023$End_time_discusion <- ymd_hms(info_data_en_AFB2023$End_time_discusion)
# info_data_en_AFB2023$Start_time_chatbot <- ymd_hms(info_data_en_AFB2023$Start_time_chatbot)
# info_data_en_AFB2023$End_time_chatbot <- ymd_hms(info_data_en_AFB2023$End_time_chatbot)
# info_data_en_AFB2023$Start_time_agent <- ymd_hms(info_data_en_AFB2023$Start_time_agent)
# info_data_en_AFB2023$End_time_agent <- ymd_hms(info_data_en_AFB2023$End_time_agent)

data_en <- info_data_en_AFB2023


# Nettoyage des données en supprimant les valeurs manquantes
data_clean <- data_en$Scenario_Chatbot[!is.na(data_en$Scenario_Chatbot)]

# Compter les occurrences de chaque scénario
data_scenario_counts <- as.data.frame(table(unlist(data_clean)))
scenario_counts <- table(unlist(data_clean))
# Créer un dataframe à partir des résultats
scenario_df <- data.frame(Scenario = names(scenario_counts), Count = as.numeric(scenario_counts))
# Réinitialiser les index du dataframe
rownames(scenario_df) <- NULL


# Retirer les chiffres, les deux points et le mot "for" au début de chaque scénario
scenario_df$Scenario <- gsub("^(\\d{1,}:)", "", scenario_df$Scenario)
# Retirer tous les "for " de la chaîne
scenario_df$Scenario <- gsub("for ", "", scenario_df$Scenario)



# Trouver les indices des scénarios contenant "Press" et "return"
indices <- grep("Press|return", scenario_df$Scenario)


# Supprimer les scénarios correspondants du dataframe
scenario_df <- scenario_df[-indices, ]

# Réinitialiser les index du dataframe
rownames(scenario_df) <- NULL


# Trier les données par nombre d'occurrences
scenario_df <- scenario_df[order(scenario_df$Count, decreasing = TRUE), ]



# Créer un graphique interactif avec Plotly
plot_ly(scenario_df, x = ~Count, y = ~Scenario, type = 'bar', orientation = 'h') %>%
  layout(title = "Fréquence des scénarios de chatbot",
         xaxis = list(title = "Nombre d'occurrences"),
         yaxis = list(title = "Scénario"))


# Wordcloud sur les scenario
# Affichage du nuage de mots interactif avec wordcloud2
wordcloud2(scenario_df,  size = 2)




# Créer une liste de paires de relations (arêtes)
edges <- list()
nodes <- unique(unlist(data_clean)) %>% na.omit()
nodes_df <- data.frame(name = nodes, value = 1, size = 10, category = "Option", symbol = "circle")

for (conversation in data_clean) {
  previous_option <- NULL
  for (option in conversation) {
    if (!is.na(option)) {
      if (!is.null(previous_option)) {
        edges <- append(edges, list(c(previous_option, option)))
      }
      previous_option <- option
    }
  }
}

# Convertir la liste en data frame
edges_df <- do.call(rbind, edges) %>%
  as.data.frame() %>%
  rename(source = V1, target = V2)

# Création et visualisation du graphe interactif
e_charts() %>%
  e_graph(layout = "force") %>%
  e_graph_nodes(nodes_df, name, value, size, category, symbol) %>%
  e_graph_edges(edges_df, source, target) %>%
  e_tooltip() %>%
  e_title("Graphe de Connaissance des Scénarios Chatbot des Clients")








############## Scenario en francais
path_data_fr_AFB23 <- file.path(path_data,"fr_info_data_AFB2023.json")
# read data2
info_data_fr_AFB2023 <- fromJSON(path_data_fr_AFB23)
# info_data_fr_AFB2023$Start_time_discusion <- ymd_hms(info_data_en_AFB2023$Start_time_discusion)
# info_data_fr_AFB2023$End_time_discusion <- ymd_hms(info_data_en_AFB2023$End_time_discusion)
# info_data_fr_AFB2023$Start_time_chatbot <- ymd_hms(info_data_en_AFB2023$Start_time_chatbot)
# info_data_en_AFB2023$End_time_chatbot <- ymd_hms(info_data_en_AFB2023$End_time_chatbot)
# info_data_en_AFB2023$Start_time_agent <- ymd_hms(info_data_en_AFB2023$Start_time_agent)
# info_data_en_AFB2023$End_time_agent <- ymd_hms(info_data_en_AFB2023$End_time_agent)

data_fr <- info_data_fr_AFB2023


# Nettoyage des données en supprimant les valeurs manquantes
data_clean_fr <- data_fr$Scenario_Chatbot[!is.na(data_fr$Scenario_Chatbot)]

# Compter les occurrences de chaque scénario
data_scenario_counts <- as.data.frame(table(unlist(data_clean_fr)))
scenario_counts <- table(unlist(data_clean_fr))
# Créer un dataframe à partir des résultats
scenario_df <- data.frame(Scenario = names(scenario_counts), Count = as.numeric(scenario_counts))

# Filtrer les scénarios qui commencent par un chiffre suivi de deux points
scenario_df <- scenario_df[grep("^\\d{1,}: ", scenario_df$Scenario), ]
# Réinitialiser les index du dataframe
rownames(scenario_df) <- NULL

# Retirer les chiffres, les deux points et le mot "pour" au début de chaque scénario
scenario_df$Scenario <- gsub("^(\\d{1,}:)", "", scenario_df$Scenario)
# Retirer tous les "pour " de la chaîne
scenario_df$Scenario <- gsub("pour ", "", scenario_df$Scenario)



# Trouver les indices des scénarios contenant le terme "Tapez" et "pour"
indices <- grep("Tapez|précédent", scenario_df$Scenario)

# Supprimer les scénarios correspondants du dataframe
scenario_df <- scenario_df[-indices, ]

# Réinitialiser les index du dataframe
rownames(scenario_df) <- NULL

# Trier les données par nombre d'occurrences
scenario_df <- scenario_df[order(scenario_df$Count, decreasing = TRUE), ]


# Wordcloud sur les scenario
wordcloud2(scenario_df,  size = 2)




