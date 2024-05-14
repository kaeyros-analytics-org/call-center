library(lubridate)
library(dplyr)
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
require(quanteda)
require(quanteda.sentiment)
library(syuzhet)


#import data
root <- getwd()
path_data <- file.path(root,"data")
path_data_AFB23 <- file.path(path_data,"info_data_AFB2023.json")
# read data2
info_data_AFB2023 <- fromJSON(path_data_AFB23)
info_data_AFB2023$Start_time_discusion <- ymd_hms(info_data_AFB2023$Start_time_discusion)
info_data_AFB2023$End_time_discusion <- ymd_hms(info_data_AFB2023$End_time_discusion)
info_data_AFB2023$Start_time_chatbot <- ymd_hms(info_data_AFB2023$Start_time_chatbot)
info_data_AFB2023$End_time_chatbot <- ymd_hms(info_data_AFB2023$End_time_chatbot)
info_data_AFB2023$Start_time_agent <- ymd_hms(info_data_AFB2023$Start_time_agent)
info_data_AFB2023$End_time_agent <- ymd_hms(info_data_AFB2023$End_time_agent)

data <- info_data_AFB2023


path_data_en_AFB23 <- file.path(path_data,"en_info_data_AFB2023.json")
info_data_en_AFB2023 <- fromJSON(path_data_en_AFB23)
data_en <- info_data_en_AFB2023

################# Data to display average time call by agent ###############
# Filter out rows with "auto" or empty Agent values and extract the last word of Agent
data_average_call_agent <- data %>%
  filter(Agent != "auto" & Agent != "") %>%
  mutate(Agent = tolower(word(Agent, -1)))

# Select relevant columns
data_average_call_agent <- data_average_call_agent[c("Agent", "Start_time_discusion", "Start_time_agent", "End_time_agent")]

# Calculate the call duration in minutes
data_average_call_agent$call_duration <- as.numeric(difftime(data_average_call_agent$End_time_agent, 
                                                             data_average_call_agent$Start_time_agent, 
                                                             units = "mins"))

data_average_call_agent <- data_average_call_agent[c("Agent", "Start_time_discusion", "call_duration")]
# Convertir le dataframe en JSON
data_average_call_agent <- toJSON(data_average_call_agent)
# Spécifier le chemin et le nom de fichier pour l'exportation
output_file <- file.path(path_data,"customer_interaction_data","data_average_call_agent.json")
# Exporter le JSON
writeLines(data_average_call_agent, output_file)




#################  data to display wordcloud #################
start_times <- c()
keys <- c()
values <- c()

# Parcourir chaque élément de la colonne "customer_agent_discussion"
for (i in seq_along(data_en$customer_agent_discussion)) {
  # Récupérer la longueur de la valeur actuelle
  value_length <- length(data_en$customer_agent_discussion[[i]]$key)
  # Répéter la valeur de Start_time_discusion autant de fois que nécessaire
  start_times <- c(start_times, rep(data_en$Start_time_discusion[i], value_length))
  # Ajouter les valeurs de key et value à leurs vecteurs respectifs
  keys <- c(keys, data_en$customer_agent_discussion[[i]]$key)
  values <- c(values, data_en$customer_agent_discussion[[i]]$value)
}

# Créer le nouveau dataframe
df_total <- data.frame(Start_time_discusion = start_times,
                     key = keys,
                     value = values,
                     stringsAsFactors = FALSE)



# Separation des contenus en anglais de ceux en francais
file_pretrained = system.file("language_identification/lid.176.ftz", package = "fastText")
dtbl_out <- language_identification(df_total$value, file_pretrained)
indexes <- which(dtbl_out$iso_lang_1 == "fr")

df_total_fr <- df_total[indexes, ]
df_total_en <- df_total[!(seq_along(df_total$key) %in% indexes), ]


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
  # 
  # # Ajouter la ligne nettoyée au dataframe de données nettoyées
  # df_total_en_clean <- rbind(df_total_en_clean, df_total_en[i,])
  
}

# selection des messages en anglais ou les clients interviennent
df_total_en_client <- df_total_en[which(df_total_en$key=="client"),]
df_total_en_client <- df_total_en_client[c("Start_time_discusion", "value")]

# Convertir le dataframe en JSON
wordcloud_data <- toJSON(df_total_en_client)
# Spécifier le chemin et le nom de fichier pour l'exportation
output_file <- file.path(path_data,"customer_interaction_data","wordcloud_data.json")
# Exporter le JSON
writeLines(wordcloud_data, output_file)



######################### Data to display Topic model ############################""
route <- paste(file.path(path_data,"customer_interaction_data"),"/Topic_modelling", sep="")

# Vérifier si le dossier existe déjà
if (!dir.exists(route)) {
  # Si le dossier n'existe pas, exécuter serVis
  
  # topic model des message des clients en anglais
  sotu_corpus <- corpus(df_total_en_client$value)
  corp = corpus_reshape(sotu_corpus, to = "sentences")
  #corp = corpus_reshape(data_corpus_inaugural, to = "paragraphs")
  dfm = dfm( quanteda::tokens(corp))
  dfm = dfm_trim(dfm, min_docfreq = 5)
  
  dtm = convert(dfm, to = "topicmodels")
  set.seed(1)
  
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

