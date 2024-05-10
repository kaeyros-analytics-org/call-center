######## UI for ENTREE RELATION
call_sentiments_ui <- function(id){
  ns <- NS(id)
  fluentPage(
    tags$style("
               .fieldGroup-82{border: none;}
               "),
    div(class="container-fluid",
        div(class="row p-0 m-0", 
            div(class="col-lg-6 pr-1 pl-0", style = "text-align: center;", tags$h4("Most Occurring Scenario Chatbot"), plotlyOutput(ns("most_occurring_scenario"))
            ),
            div(class="col-lg-6 pl-1 pr-0", style = "text-align: center;", tags$h4("Call Sentiment"), id = "linechart",
                plotlyOutput(ns("call_sentiment"))
            )
        )
    )
  )
  
}



########### Server for ENTREE RELATION
call_sentiments_server <- function(input, output, session, filterStates){
  
  data_en_filter <- reactive({ data_en %>%
      filter(Start_time_discusion >= ymd(filterStates$date_start) &
               Start_time_discusion <= ymd(filterStates$date_end)) 
  })

  client_values <-  reactive({
    client_values <- c()
    # Parcourir les colonnes de 10 à 50
    for (i in 1:nrow(data_en_filter())) {
      # Extraire les valeurs de la colonne i pour les discussions client
      values <- data_en_filter()[["customer_agent_discussion"]][[i]][data_en_filter()[["customer_agent_discussion"]][[i]]$key == "client", "value"]
      
      # Ajouter les valeurs et les temps extraits aux vecteurs correspondants
      client_values <- c(client_values, values)
    }
    
    return(client_values)
  })
  
  
  output$most_occurring_scenario <- renderPlotly({ 
    plot_ly(top_30, x = ~Count, y = ~Scenario, type = 'bar', orientation = 'h') %>%
      layout(
             xaxis = list(title = "Occurrences"),
             yaxis = list(title = "Scénario"))
    }) 
  
  output$call_sentiment <- renderPlotly({
    # Convertir le texte en minuscules
    client_values <- tolower(client_values())
    
    # Supprimer la ponctuation
    client_values <- gsub("[[:punct:]]", " ", client_values)
    
    # Supprimer les chiffres
    client_values <- gsub("\\d+", "", client_values)
    
    # Selection des discussion uniquement anglaise
    file_pretrained = system.file("language_identification/lid.176.ftz", package = "fastText")
    dtbl_out <- language_identification(client_values, file_pretrained)
    indexes <- which(dtbl_out$iso_lang_1 == "en")
    
    client_values <- client_values[indexes]
    # Obtention des sentiments
    sentiment_scores <- get_nrc_sentiment(client_values, lang="english")
    
    emotions <- colSums(prop.table(sentiment_scores[, 1:8]))
    pourcentage_positive  <- colSums(prop.table(sentiment_scores[, 9:10]))
    
    plot_ly(labels = names(emotions), values = emotions, type = "pie", hole = 0.5) %>%
      add_annotations(
        text = paste0(round(pourcentage_positive[2]*100, 2),  "% positive"),
        x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 20, color = "gray")) %>%
      add_trace(
        textinfo = "percent",
        hoverinfo = "text+percent",
        text = ~paste(names(emotions)))
  }) 
}