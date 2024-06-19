customer_interaction_ui <- function(id){
  ns <- NS(id)
  
  fluentPage(
    tags$style("
               .fieldGroup-82{border: none;}
               "),
    div(class="container-fluid", style = "text-align: center;", tags$h4("Average Call Duration by Agent"), 
                plotlyOutput(ns("requests_by_agent"))
            ),
    div(class="container-fluid", style = "text-align: center;", tags$h4("Wordcloud"),
            uiOutput(ns("Wordcloud"), style="display: inline-block;")
        ),
    div(class="container-fluid", style = "text-align: center;", tags$h4("Average Time of Resolution by Topic"),
            htmlOutput(ns('topicChart'))
        )
  )
  
  
}

customer_interaction_server <- function(input, output, session, filterStates){
  # Calculate the average call duration and number of calls per agent
  data_average_call_agent_filter <- reactive({ data_average_call_agent %>%
      filter(Start_time_discusion >= ymd(filterStates$date_start) &
               Start_time_discusion <= ymd(filterStates$date_end)) 
  })
  
  wordcloud_data_filter <- reactive({ wordcloud_data %>%
      filter(Start_time_discusion >= ymd(filterStates$date_start) &
               Start_time_discusion <= ymd(filterStates$date_end)) 
  })
  
  output$requests_by_agent <- renderPlotly({
    average_call_duration_with_count <- data_average_call_agent_filter() %>%
      group_by(Agent) %>%
      summarise(avg_call_duration = mean(call_duration),
                num_calls = n()) %>%
      arrange(desc(avg_call_duration))
    
    plot_ly(average_call_duration_with_count, x = ~avg_call_duration , y = ~Agent, type = "bar", 
            marker = list(color = colorRampPalette(c("gray80", "gray20", "red"))(n = length(unique(average_call_duration_with_count$Agent)))),
            text = ~paste("Agent: ", Agent, "<br>Call Duration: ", round(avg_call_duration , 2), " mins",
                          "<br>Number of Calls: ", num_calls),
            hoverinfo = "text") %>%
      layout(title = "",
             xaxis = list(title = "Agent"),
             yaxis = list(title = "Average Call Duration (minutes)"))
  })
  
  output$topicChart <- renderUI({
    root <- getwd()
    path_data <- file.path(root,"data")
    route <- paste(file.path(path_data,"customer_interaction_data"),"/Topic_modelling", sep="")
    addResourcePath("lda", route)
    url = "lda/index.html"
    lda <- tags$iframe(src=url, height=700, width=1400)
    lda
    })
  
  output$Wordcloud <- renderUI({
    
    word_freq <- wordcloud_data_filter() %>%
      select(value) %>%
      unnest_tokens(word, value) %>%
      count(word, sort = TRUE)
    
    wordcloud2(word_freq, size = 2)
    
  })
  
}