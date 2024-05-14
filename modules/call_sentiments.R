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
  
  data_en_filter <- reactive({ sentiment_data %>%
      filter(Start_time_discusion >= ymd(filterStates$date_start) &
               Start_time_discusion <= ymd(filterStates$date_end)) 
  })
  
  
  output$most_occurring_scenario <- renderPlotly({ 
    plot_ly(top_30, x = ~Count, y = ~Scenario, type = 'bar', orientation = 'h') %>%
      layout(
             xaxis = list(title = "Occurrences"),
             yaxis = list(title = "Scénario"))
    }) 
  
  output$call_sentiment <- renderPlotly({
    
    #client_values <- data_en_filter()$value
    # data_en_filter()[1:5,3:10]
    emotions <- colSums(prop.table(data_en_filter()[,3:10]))
    pourcentage_positive  <- colSums(prop.table(data_en_filter()[,3:10]))
    
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