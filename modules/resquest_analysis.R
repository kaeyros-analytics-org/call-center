######## UI for ENTREE RELATION
resquest_analysis_ui <- function(id){
  ns <- NS(id)
  fluentPage(
    tags$style("
               .fieldGroup-82{border: none;}
               "),
    div(class="row p-0 m-0",
        div(class="col-lg-4", valueBoxOutput(ns("total_chat_entry"), width = 10)),
        div(class="col-lg-4", valueBoxOutput(ns("total_chat_duration"), width = 20)),
        div(class="col-lg-4", valueBoxOutput(ns("average_chat_duration"), width = 20))
    ),
    div(class="container-fluid",
        div(class="row p-0 m-0", 
            div(class="col-lg-6 pr-1 pl-0", style = "text-align: center;", tags$h4("Occurences of chat by time slot and day of the week"), plotlyOutput(ns("request_per_day_hour"))),
            div(class="col-lg-6 pl-1 pr-0",style = "text-align: center;",  id = "linechart", tags$h4("Customer Request per Month"), plotlyOutput(ns("request_per_month")))))
  )
  
}

########### Server for ENTREE RELATION
resquest_analysis_server <- function(input, output, session, filterStates){
  resquest_analysis_data_filter <- reactive({ resquest_analysis_data %>%
    filter(Start_time_discusion >= ymd(filterStates$date_start) &
             Start_time_discusion <= ymd(filterStates$date_end)) 
  })
  
  
  # Sample output for demonstration purpose
  output$total_chat_entry <- renderInfoBox({
    
    infoBox(
      HTML("<strong><h4> Total Chat Entry </h4></strong>"),
      value = tags$b(style = "font-size: 24px; font-weight: bold; color: blue;",nrow(resquest_analysis_data_filter())),
      icon = icon("users"),
      color = "olive",
      width = 50
    )
    
  })
  
  output$total_chat_duration <- renderInfoBox({
    TotalChatDuration <- sum(resquest_analysis_data_filter()$duration_chat_s)
    hours <- floor(TotalChatDuration / 3600)
    minutes <- floor((TotalChatDuration %% 3600) / 60)
    seconds <- TotalChatDuration %% 60
    TotalChatDuration <- sprintf("%02d:%02d:%02d", hours, minutes, seconds)
    
    infoBox(
      HTML("<strong><h4> Total chat duration </h4></strong><p>(hh:mm:ss)</p"),
      value = tags$b(style = "font-size: 24px; font-weight: bold; color: blue;",TotalChatDuration),
      icon = icon("clock"),
      color = "olive",
      width = 50
    )
      
  })
  output$average_chat_duration <- renderInfoBox({
    AverageChatDuration <- median(resquest_analysis_data_filter()$duration_chat_s)
    hours <- floor(AverageChatDuration / 3600)
    minutes <- floor((AverageChatDuration %% 3600) / 60)
    seconds <- round(AverageChatDuration %% 60)
    AverageChatDuration <- sprintf("%02d:%02d:%02d", hours, minutes, seconds)
    
    infoBox(
      HTML("<strong><h4> Median chat duration </h4></strong><p>(hh:mm:ss)</p"),
      value = tags$b(style = "font-size: 24px; font-weight: bold; color: blue;",AverageChatDuration),
      icon = icon("info"),
      color = "olive",
      width = 50
    )
      
  })
  
  
  output$request_per_day_hour <- renderPlotly({
    
    data_per_day <- as.data.frame(table(resquest_analysis_data_filter()[c("time_period","date")]))

    # Ordonner les jours de la semaine en fonction de la fréquence (hauteur des courbes)
    ordered_days <- unique(data_per_day$date)[order(-tapply(data_per_day$Freq, data_per_day$date, max))]
    
    # Créer une palette de couleur allant du gris au rouge foncé, avec des nuances plus foncées pour les jours de la semaine ayant les valeurs les plus élevées
    yiord_palette <- colorRampPalette(c("red", "gray20", "gray80"))(n = length(unique(data_per_day$date)))
    
    # Créer le graphique Plotly
    data_per_day %>%
      plot_ly(x = ~time_period, y = ~Freq, color = ~factor(date, levels = ordered_days), type = "scatter", mode = "lines+markers", colors = yiord_palette,
              text = ~paste("Day: ", date, "<br>Time: ", time_period, "<br>Occurences: ", Freq )) %>%
      layout(xaxis = list(title = "Time slot"), yaxis = list(title = "Occurences")) %>%
      plotly::style(hoverinfo = "text") %>% 
      print()
  })
  
  output$request_per_month <- renderPlotly({
    monthly_data <- resquest_analysis_data_filter() %>%
      group_by(month = format(Start_time_discusion, "%b.%Y")) %>%
      summarise(total_requests = n()) %>%
      mutate(month_date = as.Date(paste(month, "01", sep = "-"), format = "%b.%Y-%d")) %>%
      arrange(month_date)
    
    plot_ly(monthly_data, x = ~month_date, y = ~total_requests, type = "scatter", mode = "markers+lines",
            marker = list(color = "gray90"), line = list(color = "gray90"),
            text = ~paste("Month: ", format(month_date, "%b %Y") , "<br>Number: ", total_requests )) %>%
      layout(xaxis = list(title = "Month"), yaxis = list(title = "Number of requests")) %>% 
      plotly:: style(hoverinfo = "text")
    
  })
}