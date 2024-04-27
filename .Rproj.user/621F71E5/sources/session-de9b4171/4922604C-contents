######## UI for ENTREE RELATION
call_sentiments_ui <- function(id){
  ns <- NS(id)
  fluentPage(
    tags$style("
               .fieldGroup-82{border: none;}
               "),
    div(class="container-fluid",
        div(class="row p-0 m-0", 
            div(class="col-lg-6 pr-1 pl-0", br(), reactableOutput(ns("table")),
                TextField.shinyInput(
                  ns("textInput"),
                  label = "Ecrivez votre remarque",
                  style = "border: 1px solid blue; border-radius: 10px;"
                )),
            div(class="col-lg-6 pl-1 pr-0", plotlyOutput(ns("plot"), width = "100px", height = "500px"))))
  )
  
}

fichier <- "./data/tab1.xlsx"
table <- readxl::read_excel(fichier)
table <- as.data.frame(table)

########### Server for ENTREE RELATION
call_sentiments_server <- function(input, output, session){
  
  output$table <- renderReactable({
    reactable(table, resizable = TRUE, selection = "single",
              onClick = "select", pagination = TRUE, defaultPageSize = 11,
              #searchable = TRUE,
              wrap = FALSE,
              striped = FALSE,
              highlight = FALSE,
              bordered = TRUE,
              defaultColDef = colDef(
                header = function(value) gsub(".", " ", value, fixed = TRUE),
                align = "left",
                headerStyle = list(background = "#f0f5f9")
              ),
              theme = reactableTheme(
                stripedColor = "#f6f8fa",
                highlightColor = "#5547AC",
                cellPadding = "8px 12px",
                style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI,
          Helvetica, Arial, sans-serif"),
                searchInputStyle = list(width = "100%"),
                headerStyle = list(
                  "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
                  "&[aria-sort='ascending'], &[aria-sort='descending']"
                  = list(background = "hsl(0, 0%, 96%)"),
                  borderColor = "grey"
                ),
                rowSelectedStyle = list(backgroundColor = "#0091E5",
                                        boxShadow = "inset 2px 0 0 0 #ffa62d")
                
              )
              
    ) # End Reactable
  })
  
  
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
}