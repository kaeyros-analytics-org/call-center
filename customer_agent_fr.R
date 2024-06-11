library(jsonlite)
library(tm)
library(SnowballC)
library(textstem)
library(stringr)
library(writexl)
library(readxl)
library(dplyr)
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
library(spacyr)
library(wordcloud2)
library(arulesViz)


root <- getwd()
path_data <- file.path(root, "data")
path_data_fr_AFB23 <- file.path(path_data,"fr_info_data_AFB2023.json")
# read data2
info_data_fr_AFB2023 <- fromJSON(path_data_fr_AFB23)
data_fr <- info_data_fr_AFB2023

df <- data_fr$customer_agent_discussion[!sapply(data_fr$customer_agent_discussion, function(x) length(x) == 0)]

# reunion de tous les sous dataframes en un
df_total <- data.frame(key = character(), value = character(), stringsAsFactors = FALSE)
for (i in 1:length(df)){
  df_total <- rbind(df_total, df[[i]])
}

# Separation des contenus en anglais de ceux en francais
file_pretrained = system.file("language_identification/lid.176.ftz", package = "fastText")
dtbl_out <- language_identification(df_total$value, file_pretrained)
indexes <- which(dtbl_out$iso_lang_1 == "en")

df_total_en <- df_total[indexes, ]
df_total_fr <- df_total[!(seq_along(df_total$key) %in% indexes), ]
# Création d'un dataframe vide pour stocker les données nettoyées
df_total_fr_clean <- data.frame(key = character(), value = character(), stringsAsFactors = FALSE)

# traitement du texte anglais
for (i in 1:nrow(df_total_fr)){

  # Convertir le texte en minuscules
  df_total_fr$value[i] <- tolower(df_total_fr$value[i])

  # Supprimer la ponctuation
  df_total_fr$value[i] <- gsub("[[:punct:]]", " ", df_total_fr$value[i])

  # Supprimer les chiffres
  df_total_fr$value[i] <- gsub("\\d+", "", df_total_fr$value[i])

  # Supprimer les mots vides
  stopwords_list <- stopwords("fr")

  # Mots à ajouter à la liste des stopwords
  mots_a_ajouter <- c("fichier", "jpg","hello", "ok", "okay", "pdf", "bonjour", "bsr", "bjr",
                      "bonsoir", "allô", "toc", "svp")

  # Concaténer les mots à ajouter avec la liste existante des stopwords
  stopwords_list <- c(stopwords_list, mots_a_ajouter)

  df_total_fr$value[i] <- removeWords(df_total_fr$value[i], stopwords_list)


  # Ajouter la ligne nettoyée au dataframe de données nettoyées
  df_total_fr_clean <- rbind(df_total_fr_clean, df_total_fr[i,])
}


# selection des message des clients
df_total_fr_clean_client <- df_total_fr_clean[which(df_total_fr_clean$key=="client"),]
#write.csv(df_total_fr_clean_client, file.path(path_data, "df_total_fr_clean_client.csv"), row.names = FALSE, fileEncoding = "UTF-8")



##################################### Analyse du texte ######################
corpus <- Corpus(VectorSource(df_total_fr_clean_client$value))
print(corpus)

#retirer les espaces en trop (s'il en reste encore)
corpus <- tm_map(corpus,stripWhitespace)

#Création de la matrice documents-termes
mdt <- DocumentTermMatrix(corpus,control=list(weighting=weightBin))
print(mdt)

m <- as.matrix(mdt)
#frequence des mots
freqMots <- colSums(m)


# Trier les occurrences des termes par ordre décroissant
term_occurrences_sorted <- sort(term_occurrences, decreasing = TRUE)

#tri par ordre décroissant
freqMots <- sort(freqMots,decreasing=TRUE)

#affichage des 10 mots les plus fréquents
print(freqMots[1:50])


#termes n'apparaissant qu'au plus deux fois
terms_occuring_less_than_or_equal_to_2_times <- colnames(m)[which(freqMots<=2)]

# conserver que les termes apparaissant plus de 2 fois dans la matrice
mClean <- m[,colSums(m) > 2]
print(dim(mClean))

#wordcloud des mot restant
word_freq_df <- data.frame(word = colnames(mClean), freq = colSums(mClean))
word_freq_df <- word_freq_df[order(word_freq_df$freq, decreasing = TRUE), ] # Trier le dataframe par fréquence décroissante
wordcloud2(word_freq_df[1:50,], size = 2)


# les termes qui reviennent souvent conjointement dans les messages des clients
#paramètres de l'extraction des itemsets
parametres <- list(supp=0.01,minlen=2, maxlen=3,target="frequent itemsets")
#extraction des itemsets
itemsets <- apriori(mClean,parameter=parametres)

# Affichage des itemsets fréquents
as(itemsets, "data.frame")

plot(itemsets, method = "graph", limit = 25)






#######################################################################################
# topic model des message des clients

sotu_corpus <- corpus(df_total_fr_clean_client$value)
corp = corpus_reshape(sotu_corpus, to = "sentences")
#corp = corpus_reshape(data_corpus_inaugural, to = "paragraphs")
dfm = dfm(corp)
dfm = dfm_trim(dfm, min_docfreq = 5)

dtm = convert(dfm, to = "topicmodels")
set.seed(1)

harmonicMean <- function(logLikelihoods, precision = 2000L) {
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}

m = LDA(dtm, method = "Gibbs", k = 10,  control = list(alpha = 0.1))
m

terms(m, 5)

dtm = dtm[slam::row_sums(dtm) > 0, ]
phi = as.matrix(posterior(m)$terms)
theta <- as.matrix(posterior(m)$topics)
vocab <- colnames(phi)
doc.length = slam::row_sums(dtm)
term.freq = slam::col_sums(dtm)[match(vocab, colnames(dtm))]

json = createJSON(phi = phi, theta = theta, vocab = vocab,
                  doc.length = doc.length, term.frequency = term.freq)
serVis(json, open.browser = TRUE)



