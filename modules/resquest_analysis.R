######## UI for ENTREE RELATION
resquest_analysis_ui <- function(id){
  ns <- NS(id)
  fluentPage(
    tags$style("
               .fieldGroup-82{border: none;}
               "),
    div(class="row p-0 m-0",
        div(class="col-lg-4", infoBoxOutput(ns("total_chat_entry"), width = 10)),
        div(class="col-lg-4", infoBoxOutput(ns("total_chat_duration"), width = 20)),
        div(class="col-lg-4", infoBoxOutput(ns("average_chat_duration"), width = 20))
    ),
    div(class="container-fluid",
        div(class="row p-0 m-0", 
            div(class="col-lg-6 pr-1 pl-0", style = "text-align: center;", tags$h4("Occurences of chat by time slot and day of the week"), echarts4rOutput(ns("request_per_day_hour"))),
            div(class="col-lg-6 pl-1 pr-0",style = "text-align: center;",  id = "linechart", tags$h4("Customer Request per Month"), echarts4rOutput(ns("request_per_month")))))
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
      value = tags$b(style = "font-size: 24px; font-weight: bold; color: blue;", AverageChatDuration),
      icon = icon("info"),
      color = "olive",
      width = 50
    )
      
  })
  
  
  output$request_per_day_hour <- renderEcharts4r({
    
    data_per_day <- as.data.frame(table(resquest_analysis_data_filter()[c("time_period","date")]))

    # Spécifiez l'ordre des jours de la semaine
    day_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    
    # Créer une palette de couleur allant du gris au rouge foncé, avec des nuances plus foncées pour les jours de la semaine ayant les valeurs les plus élevées
    yiord_palette <- colorRampPalette(c("red", "gray20", "gray80"))(n = length(unique(data_per_day$date)))
    
    # Créer le graphique 
    data_per_day %>%
      mutate(date = factor(date, levels = day_order)) %>%
      group_by(date) %>%
      e_charts(time_period) %>%
      e_line(Freq, series = date) %>%
      e_tooltip(trigger = 'axis') %>%
      e_color(yiord_palette) %>%
      e_x_axis(name = "Time slot") %>%
      e_y_axis(name = "Occurrences") %>%
      e_legend(type = "scroll",  orient = "vertical", right = "0%", top = "10%") %>% 
      e_toolbox_feature() %>% 
      e_toolbox_feature(
        feature = "magicType",
        type = list("line", "bar")
      )
    
  })
  
  output$request_per_month <- renderEcharts4r({
    monthly_data <- resquest_analysis_data_filter() %>%
      group_by(month = format(Start_time_discusion, "%b.%Y")) %>%
      summarise(total_requests = n()) %>%
      mutate(month_date = as.Date(paste(month, "01", sep = "-"), format = "%b.%Y-%d")) %>%
      arrange(month_date)
    
    monthly_data %>%
      e_charts(month_date) %>%
      e_line(total_requests) %>%
      e_toolbox() %>%
      e_toolbox_feature() %>%
      e_tooltip(formatter = htmlwidgets::JS("
    function(params) { 
      var date = new Date(params.value[0]);
      var month = date.toLocaleString('en-US', { month: 'long' });
      return 'Month:  ' + month + '<br>Number: ' + params.value[1]; 
    }")) %>%
      e_x_axis(name = "Month") %>%
      e_y_axis(name = "Number of requests") %>%
      e_legend(type = "scroll",  orient = "vertical", right = "0%", top = "20%") %>%
      e_toolbox_feature(
        feature = "magicType",
        type = list("line", "bar")
      )
    
  })
}