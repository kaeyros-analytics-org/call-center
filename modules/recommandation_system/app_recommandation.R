# Charger les packages nécessaires
library(shiny)
library(quanteda)
library(topicmodels)
library(stopwords)
library(dplyr)
library(ldatuning)
library(tm)
library(text)
library(lsa)
library(dplyr)
library(stringr)
library(tidytext)
library(stopwords)
library(topicmodels)
library(jsonlite)
library(LDAvis)
library(SnowballC)
library(textstem)
library(proxy)
library(tidytext)
library(shinyjs)

# Définir la liste des stopwords et mots à ajouter
stopwords_list <- stopwords::stopwords('fr', source='stopwords-iso')
mots_a_ajouter <- c("fichier", "jpg","hello", "ok", "okay", "pdf", "bonjour", "bsr","bjr", "banque", "bonsoir",
                    "allô", "toc", "svp", "bank", "afriland", "frs", "fr", "fcfa", "cfa", "svp", "bonsoir", "weeeehhh",
                    "hi", "tssssuip", "bonsoi", "aló", "héllo", "first", "afrilandfirstbank", "firstbank","okkk","xaf")
stopwords_list <- c(stopwords_list, mots_a_ajouter)

# Fonction de prétraitement pour une chaîne de caractères
preprocess_text <- function(text) {
  # Convertir le texte en minuscules
  text <- tolower(text)

  # Remplacer la ponctuation par des espaces
  text <- gsub("[[:punct:]]", " ", text)

  # Retirer les chiffres
  text <- gsub("\\d+", "", text)

  # Tokenisation et suppression des mots vides
  tokens <- unlist(strsplit(text, "\\s+"))
  tokens <- tokens[!tokens %in% stopwords_list]

  # Appliquer la lemmatisation (vous devez avoir une fonction de lemmatisation)
  tokens <- lemmatize_words(tokens)

  # Recombiner les tokens en une seule chaîne de caractères
  text <- paste(tokens, collapse = " ")

  return(text)
}

# Calcul de la similarité cosinus
cosine_similarity <- function(A, B) {
  # Calcul des produits scalaires
  AB <- A %*% t(B)
  
  # Calcul des normes
  A_norm <- sqrt(rowSums(A^2))
  B_norm <- sqrt(rowSums(B^2))
  
  # Normalisation pour obtenir la similarité cosinus
  similarity <- AB / (A_norm %*% t(B_norm))
  return(similarity)
}

# Fonction de recommandation des produits
recommender_system <- function(discussion, topic_model, grouped_train_data) {
  # Prétraiter le texte de la discussion
  discussion <- sapply(discussion , preprocess_text)

  # Créer le corpus et la matrice document-fréquence (dfm)
  corp_new <- corpus(discussion, docnames = paste0("new_", seq_along(discussion)))
  dfm_new <- dfm(corp_new)

  # Vérifiez si dfm_new contient des termes
  if (nfeat(dfm_new) == 0) {
    stop("No Products for this discussion")
  }

  # Obtenir le sujet dominant de la nouvelle discussion
  new_1 <- posterior(topic_model, newdata = dfm_new)
  dominant_topic <- which.max(new_1$topics)

  # Extraire les topics pour tous les documents existants
  topics <- posterior(topic_model)$topics
  topic_docs <- apply(topics, 1, which.max) == as.integer(dominant_topic)

  # Vérifiez si des documents correspondent au sujet dominant
  if (!any(topic_docs)) {
    stop("No Products for this discussion")
  }

  dfm_topic <- dfm(grouped_train_data$discussion_text_client[topic_docs], docnames = paste0("doc_", seq_along(topic_docs)))

  # Harmoniser les vocabulaires
  dfm_new <- dfm_match(dfm_new, features = featnames(dfm_topic))

  # Vérifiez si dfm_new et dfm_topic contiennent des termes après harmonisation
  if (nfeat(dfm_new) == 0) {
    stop("No Products for this discussion")
  }
  if (nfeat(dfm_topic) == 0) {
    stop("No Products for this discussion")
  }

  # Convertir les DFMs en matrices denses
  dfm_new_matrix <- as.matrix(dfm_new)
  dfm_topic_matrix <- as.matrix(dfm_topic)

  # Calculer les similarités cosinus
  similarities <- cosine_similarity(dfm_new_matrix, dfm_topic_matrix)

  # Vérifiez si des similarités sont calculées
  if (length(similarities) == 0) {
    stop("No Products for this discussion")
  }

  # Identifier les outliers supérieurs au troisième quartile
  Q3_outliers <- similarities > quantile(similarities, probs = 0.75, na.rm = TRUE)
  
  # Compter le nombre d'outliers supérieurs au troisième quartile
  num_outliers <- sum(Q3_outliers, na.rm = TRUE)
  
  # Sélectionner les indices des documents les plus similaires
  k <- num_outliers
  
  # Trouver les k documents les plus similaires
  knn_indices <- order(similarities, decreasing = TRUE)[1:k]
  closest_docs <- colnames(similarities)[knn_indices]

  # Extraire les numéros des documents
  closest_docs_numbers <- gsub("text", "", closest_docs)

  # Sélectionner les produits associés aux documents les plus similaires
  selected_products <- grouped_train_data[closest_docs_numbers, "products"]

  # Vérifiez si des produits ont été sélectionnés
  if (nrow(selected_products) == 0) {
    stop("No Products for this discussion")
  }

  # Extraire les valeurs de similarité correspondantes
  similarity_values <- similarities[1, knn_indices]

  # Créer un dataframe pondéré de tous les produits
  weighted_products <- data.frame(products = character(), similarity = numeric(), stringsAsFactors = FALSE)
  for (i in 1:length(selected_products$products)) {
    products <- selected_products$products[[i]]
    similarity <- rep(similarity_values[i], length(products))
    weighted_products <- rbind(weighted_products, data.frame(products = products, similarity = similarity))
  }

  # Vérifiez si il y'a des lignes à aggreger
  if (nrow(weighted_products) == 0) {
    stop("No Products for this discussion")
  }
  # Compter les occurrences pondérées de chaque produit
  product_counts <- aggregate(similarity ~ products, data = weighted_products, sum)
  
  
  # Calculer les scores de recommandation pour chaque produit
  product_counts$score <- round(product_counts$similarity / sum(product_counts$similarity) * 100, 2)

  # Trier le dataframe en fonction des scores décroissants
  score_df <- product_counts[order(-product_counts$score), c("products", "score")]
  names(score_df) <- c("Product", "Score")
  row.names(score_df) <- NULL

  return(score_df)
}

# Interface utilisateur
recommandation_ui <- function(id) {
  ns <- NS(id)
  
  fluentPage(
    useShinyjs(),  # Activer shinyjs
    tags$style("
      #error-message {
        display: none;
        background-color: #f8d7da;
        color: #721c24;
        border: 1px solid #f5c6cb;
        border-radius: 4px;
        padding: 10px;
        margin-bottom: 15px;
      }
    "),
    div(class="container-fluid",
        div(class="row p-0 m-0",
            div(class="col-lg-6 pr-1 pl-0", style = "text-align: center;",
                textAreaInput(ns("discussion"), tags$h4("Entrez la discussion du client :"), "", width = "1000px", rows = 5),
                DefaultButton.shinyInput(ns("recommender"), "Recommander des produits",
                                         style = "color: #fff; background-color: #007BFF; float: left; border-radius: 5px;")
            ),
            div(class="row p-0 m-0",
                div(class="col-lg-6 pr-1 pl-0", style = "text-align: center;",
                    reactableOutput(ns("recommendations"), width = "800")
                )
            )
        ),
        div(id = ns("error-message"), style = "color: gray; font-weight: bold; padding: 10px; font-size: 60px;")
    )
  )
}


# Serveur
recommandation_server <- function(input, output, session) {
  # Charger les données d'entraînement et le modèle
  load("./modules/recommandation_system/grouped_train_data.RData")
  load("./modules/recommandation_system/topic_model.RData")
  
  observeEvent(input$recommender, {
    req(input$discussion)
    discussion <- input$discussion
    
    tryCatch({
      recommendations <- recommender_system(discussion, topic_model, grouped_train_data)
      output$recommendations <- renderReactable({
        reactable(recommendations,
                  striped = TRUE,
                  highlight = TRUE,
                  bordered = TRUE,
                  theme = reactableTheme(
                    borderColor = "#B5E4FB",
                    stripedColor = "#f6f8fa",
                    highlightColor = "#91A5FE",
                    cellPadding = "8px 12px",
                    style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif",
                                 fontSize = "1.10rem"),
                    searchInputStyle = list(width = "100%")
                  )
        )
      })
      shinyjs::hide("error-message")
    }, error = function(e) {
      error_message <- e$message
      shinyjs::html("error-message", error_message)
      shinyjs::show("error-message")
      output$recommendations <- renderReactable({ })
    })
  })
}


