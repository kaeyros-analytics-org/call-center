######## UI for ENTREE RELATION
resquest_analysis_ui <- function(id){
  ns <- NS(id)
  fluentPage(
    tags$style("
               .fieldGroup-82{border: none;}
               "),
    div(class="container-fluid",
        div(class="row p-0 m-0", 
            div(class="col-lg-6 pr-1 pl-0", plotlyOutput(ns("plot2"), width = "100px", height = "500px")
                ),
            div(class="col-lg-6 pl-1 pr-0", id = "linechart", plotlyOutput(ns("plot"), width = "100px", height = "500px"))))
  )
  
}

########### Server for ENTREE RELATION
resquest_analysis_server <- function(input, output, session){
  
  output$plot <- renderPlotly({
    stock <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/finance-charts-apple.csv')
    
    fig <- plot_ly(stock, type = 'scatter', mode = 'lines', line = list(color = "#05529B"))%>%
      add_trace(x = ~Date, y = ~AAPL.High, marker = list(color = "#0077B6"))%>%
      layout(showlegend = F)
    fig <- fig %>%
      layout(
        xaxis = list(zerolinecolor = '#ffff',
                     zerolinewidth = 2,
                     gridcolor = 'ffff'),
        yaxis = list(zerolinecolor = '#ffff',
                     zerolinewidth = 2,
                     gridcolor = 'ffff'),
        plot_bgcolor='#e5ecf6', width = 650)
    
    
    fig
  })
  
  output$plot2 <- renderPlotly({
    stock <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/finance-charts-apple.csv')
    
    fig <- plot_ly(stock, type = 'scatter', mode = 'lines', line = list(color = "#0DDD50"))%>%
      add_trace(x = ~Date, y = ~AAPL.High, marker = list(color = "#00DDDD"))%>%
      layout(showlegend = F)
    fig <- fig %>%
      layout(
        xaxis = list(zerolinecolor = '#ffff',
                     zerolinewidth = 2,
                     gridcolor = 'ffff'),
        yaxis = list(zerolinecolor = '#ffff',
                     zerolinewidth = 2,
                     gridcolor = 'ffff'),
        plot_bgcolor='#e5ecf6', width = 650)
    
    
    fig
  })
}