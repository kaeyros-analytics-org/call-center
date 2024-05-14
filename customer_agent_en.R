library(jsonlite)
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
library(quanteda)
library(Rmpfr)
library(servr)
library(slam)
library(wordcloud2)
library(rJava)
library(shiny.fluent)
library(shinymanager)
library(reactable)
library(sf)
library(shinyWidgets)
library(markdown)
library(stringr)
library(leaflet)
library(plotly)
library(DT)
library(shinycssloaders)
library(pool)
library(readxl)
library(shinyjs)
library(openxlsx)
library(glue)
library(rintrojs)
library(shinyjs)
library(dplyr)
library(lubridate)
library(flextable)
library(keyring)
library(shiny)
library(shinydashboard)
library(daterangepicker)
require(quanteda)
require(quanteda.sentiment)
library(syuzhet)



root <- getwd()
path_data <- file.path(root,"data")
path_data_en_AFB23 <- file.path(path_data,"en_info_data_AFB2023.json")
# read data2
info_data_en_AFB2023 <- fromJSON(path_data_en_AFB23)
data_en <- info_data_en_AFB2023

df <- data_en$customer_agent_discussion[!sapply(data_en$customer_agent_discussion, function(x) length(x) == 0)]


# reunion de tous les sous dataframes en un
df_total <- data.frame(key = character(), value = character(), stringsAsFactors = FALSE)
for (i in 1:length(df)){
  df_total <- rbind(df_total, df[[i]])
}

# Separation des contenus en anglais de ceux en francais
file_pretrained = system.file("language_identification/lid.176.ftz", package = "fastText")
dtbl_out <- language_identification(df_total$value, file_pretrained)
indexes <- which(dtbl_out$iso_lang_1 == "fr")

df_total_fr <- df_total[indexes, ]
df_total_en <- df_total[!(seq_along(df_total$key) %in% indexes), ]


route <- file.path(path_data, "df_total_en_clean_client.csv")
# Vérifier si le dossier existe déjà
if (!dir.exists(route)) {
  # Création d'un dataframe vide pour stocker les données nettoyées
  df_total_en_clean <- data.frame(key = character(), value = character(), stringsAsFactors = FALSE)
  # traitement du texte anglais
  for (i in 1:nrow(df_total_en)){
    
    # Convertir le texte en minuscules
    df_total_en$value[i] <- tolower(df_total_en$value[i])
    
    # Supprimer la ponctuation
    df_total_en$value[i] <- gsub("[[:punct:]]", " ", df_total_en$value[i])
    
    # Supprimer les chiffres
    df_total_en$value[i] <- gsub("\\d+", "", df_total_en$value[i])
    
    # Supprimer les mots vides
    stopwords_list <- stopwords::stopwords('en', source='stopwords-iso' )
    
    # Mots à ajouter à la liste des stopwords
    mots_a_ajouter <- c("fichier", "jpg", "hello", "ok", "good morning","morning",
                        "okay", "hi", "sir", "allo", "goodmorning","good evening",
                        "evening","helloo")
    
    # Concaténer les mots à ajouter avec la liste existante des stopwords
    stopwords_list <- c(stopwords_list, mots_a_ajouter)
    
    df_total_en$value[i] <- removeWords(df_total_en$value[i], stopwords_list)
    
    #retrait des espaces en trop
    df_total_en$value[i] <- gsub("\\s+"," ",df_total_en$value[i])
    
    # Ajouter la ligne nettoyée au dataframe de données nettoyées
    df_total_en_clean <- rbind(df_total_en_clean, df_total_en[i,])
    
  }
  
  # selection des messages en anglais ou les clients interviennent
  df_total_en_clean_client <- df_total_en_clean[which(df_total_en_clean$key=="client"),]
  write.csv(df_total_en_clean_client, file.path(path_data, "df_total_en_clean_client.csv"), row.names = FALSE)
  
} else {
  # Si le dossier existe déjà, afficher un message ou effectuer une autre action
  df_total_en_clean_client <- read.csv(route)
}



##################################### Analyse du texte ######################
route <- file.path(path_data, "word_freq_df.csv")
# Vérifier si le dossier existe déjà
if (!dir.exists(route)) {
  corpus <- Corpus(VectorSource(df_total_en_clean_client$value))
  # print(corpus)
  
  #retirer les espaces en trop (s'il en reste encore)
  corpus <- tm_map(corpus,stripWhitespace)
  
  #Création de la matrice documents-termes
  mdt <- DocumentTermMatrix(corpus,control=list(weighting=weightBin))
  
  m <- as.matrix(mdt)
  #frequence des mots
  freqMots <- colSums(m)
  
  # conserver que les termes apparaissant plus de 2 fois dans la matrice
  mClean <- m[,colSums(m) > 2]
  
  #wordcloud des mot restant
  word_freq_df <- data.frame(word = colnames(mClean), freq = colSums(mClean))
  write.csv(word_freq_df, file.path(path_data, "word_freq_df.csv"), row.names = FALSE)
} else {
  # Si le dossier existe déjà, afficher un message ou effectuer une autre action
  word_freq_df <- read.csv(route)
}



######################### Topic model ############################""
route <- paste(path_data,"/Topic_modelling", sep="")

# Vérifier si le dossier existe déjà
if (!dir.exists(route)) {
  # Si le dossier n'existe pas, exécuter serVis
  
  # topic model des message des clients en anglais
  sotu_corpus <- corpus(df_total_en_clean_client$value)
  corp = corpus_reshape(sotu_corpus, to = "sentences")
  #corp = corpus_reshape(data_corpus_inaugural, to = "paragraphs")
  dfm = dfm( quanteda::tokens(corp))
  dfm = dfm_trim(dfm, min_docfreq = 5)
  
  dtm = convert(dfm, to = "topicmodels")
  set.seed(1)
  
  harmonicMean <- function(logLikelihoods, precision = 2000L) {
    llMed <- median(logLikelihoods)
    as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                         prec = precision) + llMed))))
  }
  
  m = LDA(dtm, method = "Gibbs", k = 10,  control = list(alpha = 0.1))
  
  dtm = dtm[slam::row_sums(dtm) > 0, ]
  phi = as.matrix(posterior(m)$terms)
  theta <- as.matrix(posterior(m)$topics)
  vocab <- colnames(phi)
  doc.length = slam::row_sums(dtm)
  term.freq = slam::col_sums(dtm)[match(vocab, colnames(dtm))]
  
  json = createJSON(phi = phi, theta = theta, vocab = vocab,
                    doc.length = doc.length, term.frequency = term.freq)
  serVis(json, out.dir = route, open.browser = FALSE)
} else {
  # Si le dossier existe déjà, afficher un message ou effectuer une autre action
  print("Le dossier Topic Modeling existe déjà, aucune action supplémentaire nécessaire.")
}


################ ANALYSE DES SENTIMENTS ###################################"
# # Créer un vecteur pour stocker les valeurs des clients et les temps de début de discussion correspondants
# client_values <- c()
# start_times <- c()
# # Parcourir les colonnes de 10 à 50
# for (i in 10:50) {
#   # Extraire les valeurs de la colonne i pour les discussions client
#   values <- data_en[["customer_agent_discussion"]][[i]][data_en[["customer_agent_discussion"]][[i]]$key == "client", "value"]
# 
#   
#   # Extraire les temps de début de discussion correspondants
#   times <- rep(data_en$Start_time_discusion[i], length(values))
#   
#   # Ajouter les valeurs et les temps extraits aux vecteurs correspondants
#   client_values <- c(client_values, values)
#   start_times <- c(start_times, times)
# }
# 
# # Créer un dataframe avec les valeurs et les temps extraits
# df_client_discussions <- data.frame(Start_time_discusion = start_times, client_values = client_values)
# 
# 
# # Convertir le texte en minuscules
# client_values <- tolower(client_values)
# 
# # Supprimer la ponctuation
# client_values <- gsub("[[:punct:]]", " ", client_values)
# 
# # Supprimer les chiffres
# client_values <- gsub("\\d+", "", client_values)
# 
# 
# file_pretrained = system.file("language_identification/lid.176.ftz", package = "fastText")
# dtbl_out <- language_identification(client_values, file_pretrained)
# indexes <- which(dtbl_out$iso_lang_1 == "en")
# 
# client_values <- client_values[indexes]
# # Obtention des sentiments
# sentiment_scores <- get_nrc_sentiment(client_values, lang="english")
# 
# emotions <- colSums(prop.table(sentiment_scores[, 1:8]))
# pourcentage_positive  <- colSums(prop.table(sentiment_scores[, 9:10]))
# 
# plot_ly(labels = names(emotions), values = emotions, type = "pie", hole = 0.5) %>%
#   add_annotations(
#     text = paste0(round(pourcentage_positive[2]*100, 2),  "% positive"),
#     x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 20, color = "gray")) %>%
#   add_trace(
#     textinfo = "percent",
#     hoverinfo = "text+percent",
#     text = ~paste(names(emotions)))



# # topic model des message des clients en anglais
# sotu_corpus <- corpus(df_total_en_clean_client$value)
# corp = corpus_reshape(sotu_corpus, to = "sentences")
# #corp = corpus_reshape(data_corpus_inaugural, to = "paragraphs")
# dfm = dfm(tokens(corp))
# dfm = dfm_trim(dfm, min_docfreq = 5)
# 
# dtm = convert(dfm, to = "topicmodels")
# set.seed(1)
# 
# harmonicMean <- function(logLikelihoods, precision = 2000L) {
#   llMed <- median(logLikelihoods)
#   as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
#                                        prec = precision) + llMed))))
# }
# 
# m = LDA(dtm, method = "Gibbs", k = 10,  control = list(alpha = 0.1))
# m
# 
# terms(m, 5)
# 
# dtm = dtm[slam::row_sums(dtm) > 0, ]
# phi = as.matrix(posterior(m)$terms)
# theta <- as.matrix(posterior(m)$topics)
# vocab <- colnames(phi)
# doc.length = slam::row_sums(dtm)
# term.freq = slam::col_sums(dtm)[match(vocab, colnames(dtm))]
# 
# json = createJSON(phi = phi, theta = theta, vocab = vocab,
#                   doc.length = doc.length, term.frequency = term.freq)
# serVis(json, open.browser = TRUE)




# ######################################################################
# df_total_fr_clean <- data.frame(key = character(), value = character(), stringsAsFactors = FALSE)
# # traitement du texte francais
# for (i in 1:nrow(df_total_fr)){
#
#   # Convertir le texte en minuscules
#   df_total_fr$value[i] <- tolower(df_total_fr$value[i])
#
#   # Supprimer la ponctuation
#   df_total_fr$value[i] <- gsub("[[:punct:]]", " ", df_total_fr$value[i])
#
#   # Supprimer les chiffres
#   df_total_fr$value[i] <- gsub("\\d+", "", df_total_fr$value[i])
#
#   # Supprimer les mots vides
#   stopwords_list <- stopwords("fr")
#
#   # Mots à ajouter à la liste des stopwords
#   mots_a_ajouter <- c("fichier", "jpg")
#
#   # Concaténer les mots à ajouter avec la liste existante des stopwords
#   stopwords_list <- c(stopwords_list, mots_a_ajouter)
#
#   df_total_fr$value[i] <- removeWords(df_total_fr$value[i], stopwords_list)
#
#
#   # Ajouter la ligne nettoyée au dataframe de données nettoyées
#   df_total_fr_clean <- rbind(df_total_fr_clean, df_total_fr[i,])
# }
#
# # selection des message en francais des clients
# df_total_fr_clean_client <- df_total_fr_clean[which(df_total_fr_clean$key=="client"),]
# nrow(df_total_fr_clean_client)
#


