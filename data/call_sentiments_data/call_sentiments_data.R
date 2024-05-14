library(lubridate)
library(dplyr)
library(jsonlite)
library(tidyr)
require(quanteda)
require(quanteda.sentiment)
library(syuzhet)
library(tm)
library(SnowballC)
library(textstem)
library(stringr)
library(writexl)
library(readxl)
library(purrr)
library(cld3)
library(textcat)
library(fastText)
library(topicmodels)
library(tidytext)
library(LDAvis)
library(Rmpfr)



####################### Data to display scenarios ############################

############## Scenario en anglais
path_data_en_AFB23 <- file.path(path_data,"en_info_data_AFB2023.json")
# read data2
info_data_en_AFB2023 <- fromJSON(path_data_en_AFB23)

data_en <- info_data_en_AFB2023

# Nettoyage des données en supprimant les valeurs manquantes
data_clean <- data_en$Scenario_Chatbot[!is.na(data_en$Scenario_Chatbot)]

# Compter les occurrences de chaque scénario
data_scenario_counts <- as.data.frame(table(unlist(data_clean)))
scenario_counts <- table(unlist(data_clean))
# Créer un dataframe à partir des résultats
scenario_df <- data.frame(Scenario = names(scenario_counts), Count = as.numeric(scenario_counts))

# Filtrer les scénarios qui commencent par un chiffre suivi de deux points
scenario_df <- scenario_df[grep("^\\d{1,}: ", scenario_df$Scenario), ]

# Réinitialiser les index du dataframe
rownames(scenario_df) <- NULL


# Retirer les chiffres, les deux points et le mot "pour" au début de chaque scénario
scenario_df$Scenario <- gsub("^(\\d{1,}: |for )", "", scenario_df$Scenario)



# Trouver les indices des scénarios contenant le terme "Tapez"
indices <- grep("Press", scenario_df$Scenario)

# Supprimer les scénarios correspondants du dataframe
scenario_df <- scenario_df[-indices, ]

# Réinitialiser les index du dataframe
rownames(scenario_df) <- NULL

# Convertir le dataframe en JSON
scenario_data <- toJSON(scenario_df)
# Spécifier le chemin et le nom de fichier pour l'exportation
output_file <- file.path(path_data,"call_sentiments_data","scenario_data.json")
# Exporter le JSON
writeLines(scenario_data, output_file)



############################ to display opinion mining #########
wordcloud_data_file <- file.path(path_data,"customer_interaction_data","wordcloud_data.json")
sentiment_data <- fromJSON(wordcloud_data_file)

# Obtention des sentiments
sentiment_scores <- get_nrc_sentiment(sentiment_data$value, lang="english")
sentiment_data <- cbind(sentiment_data, sentiment_scores)

# Convertir le dataframe en JSON
sentiment_data <- toJSON(sentiment_data)
# Spécifier le chemin et le nom de fichier pour l'exportation
output_file <- file.path(path_data,"call_sentiments_data","sentiment_data.json")
# Exporter le JSON
writeLines(sentiment_data, output_file)
