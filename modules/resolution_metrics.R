
resolution_metrics_ui <- function(id){
 
  ns <- NS(id)
  fluentPage(
    tags$style("
               .fieldGroup-82{border: none;}
               "),
    div(class="row p-0 m-0",
        div(class="col-lg-4", valueBoxOutput(ns("call_answer_rate"), width = 20)),
        div(class="col-lg-4", valueBoxOutput(ns("Call_Abandonment_Rate"), width = 20))
    ),
    div(class="container-fluid",
        div(class="row p-0 m-0", 
            div(class="col-lg-6 pr-1 pl-0", style = "text-align: center;", tags$h4("Daily Evolution of First Response Time of Agent"), plotlyOutput(ns("first_response_time"))
            ),
            div(class="col-lg-6 pl-1 pr-0", style = "text-align: center;", tags$h4("Daily Evolution of Full Resolution Time of Agent"), id = "linechart",
                plotlyOutput(ns("full_resolution_time"))
                ),
            div(class="col-lg-6 pr-1 pl-0", style = "text-align: center;", tags$h4("Average Frequency Time to Solve an Issue"),
                plotlyOutput(ns("avg_time_to_solve_issue"))
            ),
            div(class="col-lg-6 pl-1 pr-0", style = "text-align: center;", tags$h4("Frequency of Not Solve Issue"),
                plotlyOutput(ns("not_solve_issue"))
            )
        )
    )
  )
}


resolution_metrics_server <- function(input, output, session, filterStates){
  resolution_metrics_data_filter <- reactive({ resolution_metrics_data %>%
      filter(Start_time_discusion >= ymd(filterStates$date_start) &
               Start_time_discusion <= ymd(filterStates$date_end)) 
  })
  
  # data_first_response_bon_filter <- reactive ({
  #   data_first_response_bon %>%
  #     filter(Start_time_discusion >= ymd(filterStates$date_start) &
  #            Start_time_discusion <= ymd(filterStates$date_end)) 
  # })
  # 
  # data_full_response_filter <- reactive ({
  #   data_full_response %>%
  #     filter(Start_time_discusion >= ymd(filterStates$date_start) &
  #              Start_time_discusion <= ymd(filterStates$date_end)) 
  # })
  
  output$call_answer_rate <- renderInfoBox({
    # Identifier les indices des éléments non vides
    non_empty_indices <- which(lengths(resolution_metrics_data_filter()$customer_agent_discussion) > 0)
    # Extraire les éléments non vides de la liste d'origine
    non_empty_elements <- resolution_metrics_data_filter()$customer_agent_discussion[non_empty_indices]
    # Filtrer et compter les éléments dont la colonne "key" contient le mot "agent"
    filtered_elements <- non_empty_elements[sapply(non_empty_elements, function(x) any(grepl("agent", x$key)))]
    # Calculer le taux de réponse
    call_answer_rate <- round(length(filtered_elements) * 100 / length(non_empty_indices), 2)
    
    infoBox(
      HTML("<strong><h4> Call Answer Rate </h4></strong>"),
      value = tags$b(style = "font-size: 24px; font-weight: bold; color: blue;", paste0(call_answer_rate, " %")),
      icon = icon("clock"),
      color = "olive",
      width = 500
    )
    
  })
  
  output$Call_Abandonment_Rate <- renderInfoBox({
    infoBox(
      HTML("<strong><h4> Call Abandonment Rate </h4></strong>"),
      value = tags$b(style = "font-size: 24px; font-weight: bold; color: blue;", paste0(80, " %")),
      icon = icon("clock"),
      color = "olive",
      width = 500
    )

  })
  
  
  output$first_response_time <- renderPlotly({
    chat_hours_by_day <- resolution_metrics_data_filter() %>%
      group_by(Start_time_discusion) %>%
      summarise(total_hours = mean(hours_first))
    
    # Créer un graphique interactif avec Plotly
    plot_ly(chat_hours_by_day, x = ~Start_time_discusion, y = ~total_hours, type = 'scatter', mode = 'lines') %>%
      layout(xaxis = list(title = "Date"),
             yaxis = list(title = "Time"))
  })
  
  output$full_resolution_time <- renderPlotly({
    chat_hours_by_day <- resolution_metrics_data_filter() %>%
      group_by(Start_time_discusion) %>%
      summarise(total_hours = mean(hours_full))
    
    # Créer un graphique interactif avec Plotly
    plot_ly(chat_hours_by_day, x = ~Start_time_discusion, y = ~total_hours, type = 'scatter', mode = 'lines') %>%
      layout(xaxis = list(title = "Date"),
             yaxis = list(title = "Time"))
    })
  
  output$avg_time_to_solve_issue <- renderPlotly({
    # Calculate first response time
    chat_hours_by_day_first <- resolution_metrics_data_filter() %>%
      group_by(Start_time_discusion) %>%
      summarise(first_response_hours = mean(hours_first))
    
    # Calculate full resolution time
    chat_hours_by_day_full <- resolution_metrics_data_filter() %>%
      group_by(Start_time_discusion) %>%
      summarise(full_resolution_hours = mean(hours_full))
    
    # Merge the two datasets
    combined_data <- merge(chat_hours_by_day_first, chat_hours_by_day_full, by = "Start_time_discusion", all = TRUE)
    
    # Create a plot
    plot_ly(combined_data, x = ~Start_time_discusion) %>%
      add_lines(y = ~first_response_hours, name = "First Response Time", line = list(color = "blue")) %>%
      add_lines(y = ~full_resolution_hours, name = "Full Resolution Time", line = list(color = "red")) %>%
      layout(xaxis = list(title = "Date"),
             yaxis = list(title = "Time"))
    
  })
  
  output$not_solve_issue <- renderPlotly({
    plot_ly()
  })
}