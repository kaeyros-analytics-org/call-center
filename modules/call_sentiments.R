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
                echarts4rOutput(ns("call_sentiment"))
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
    
    # Créer le graphique
    # top_30_sorted %>%
    #   e_charts(Scenario) %>%
    #   e_bar(Count) %>%
    #   e_flip_coords() %>% 
    #   e_tooltip(trigger = "axis", axisPointer = list(type = "shadow")) %>%
    #   e_x_axis(name = "Occurrences") %>%
    #   e_y_axis(name = "Scénario") %>% 
    #   e_toolbox_feature() %>% 
    #   e_toolbox_feature(
    #     feature = "magicType",
    #     type = list("line", "bar") 
    #   ) %>% 
    #   e_legend(type = "scroll", orient = "vertical", right = "0%", top = "10%")

    plot_ly(top_30, x = ~Count, y = ~Scenario, type = 'bar', orientation = 'h') %>%
      layout(
             xaxis = list(title = "Occurrences"),
             yaxis = list(title = "Scénario"))
    }) 
  
  output$call_sentiment <- renderEcharts4r({
    
    #client_values <- data_en_filter()$value
    # data_en_filter()[1:5,3:10]
    emotions <- colSums(prop.table(data_en_filter()[,3:10]))
    pourcentage_positive  <- colSums(prop.table(data_en_filter()[,11:12]))
    
    
    df_emotions <- as.data.frame(emotions)
    df_emotions$label <- rownames(df_emotions)
    
    pie_chart <- df_emotions |> 
      e_charts(label) |> 
      e_pie(
        emotions,
        radius = c("40%", "70%"),
        itemStyle = list(
          borderRadius = 20,
          borderColor = '#fff',
          borderWidth = 2
        )
      ) %>%  # Centrer le graphique
      e_tooltip(formatter = htmlwidgets::JS("function(params) { return params.name + ': ' + (params.percent + '%'); }")) %>%
      e_labels(show = TRUE,
               formatter = "{d}%",
               position = "inside") %>%
      e_legend(right = 0, 
               orient = "vertical")
    
    # Affichage du graphique
    pie_chart
    
    # plot_ly(labels = names(emotions), values = emotions, type = "pie", hole = 0.5) %>%
    #   add_annotations(
    #     text = paste0(round(pourcentage_positive[2]*100, 2),  "% positive"),
    #     x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 20, color = "gray")) %>%
    #   add_trace(
    #     textinfo = "percent",
    #     hoverinfo = "text+percent",
    #     text = ~paste(names(emotions)))
  }) 
}