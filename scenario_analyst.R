library(plotly)
library(wordcloud2)
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
