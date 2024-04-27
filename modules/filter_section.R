filterStatesRouter_ui <- function(id) {
  
  ns <- NS(id)
  fluentPage(
    shinyjs::useShinyjs(),
    div(id = "filterBox",
        uiOutput(ns("dateRange")),
        uiOutput(ns("country")),
        uiOutput(ns("city")),
        ################### This button apply a filter
        uiOutput(ns("filter_button")),
        ############ This button is for generate and Download report in .docx format
        tags$br(),
        uiOutput(ns("countryselect"))
    )
  )
  
}

filterStatesRouter_server <- function(input, output, session, filterStates) {
  #ns <- session$ns
  observeEvent(filterStates$dataNavi$dataset, {
    if(filterStates$dataNavi$dataset == "Home"){ ########## We don't need to display filter when we are at home page.
      
      output$dateRange <- renderUI({
        "Quit home to see filter."
      })
      output$country <- renderUI({
      ""
      })
      output$city <- renderUI({
        ""
      })
      output$filter_button <- renderUI({
        ""
      })
      output$countryselect <- renderUI({
        ""
      })
    }else{
      ################## Date filter
      output$dateRange <- renderUI({
        tagList(
          div(class="sidebar-header", tags$a("Choisir l'écart de date: ")),
          backendTooltip(span(`data-toggle`="tooltip",
                              `data-placement`="right",
                              `data-html` = "true",
                              title = "Choississez l'écart de de date. Il doit être d'une semaine max.<br/>
                           <b>Comment ça fonctionne:</b>
                           Crée une paire d'entrées de texte qui, lorsqu'elles sont cliquées,
                          font apparaître des calendriers sur lesquels l'utilisateur peut cliquer pour sélectionner des dates.", 
                              HTML('<i class="bi bi-question-circle"></i>'))),
          dateRangeInput("dateRangeInput", label = NULL,
                         start = as.Date(filterStates$date_start), end = as.Date(filterStates$date_end),
                         min = "2021-10-20", max = "2024-3-31"),
          tags$script(src = "./js/tooltip.js")
        )
      })
      
      ################## Country selection filter
      output$country <- renderUI({
        selection <- filterStates$countrySelected
        choices = c("Cameroun", "Congo", "Guinnée", "Tchad")
        tagList(
          div(class="sidebar-header", tags$a("Sélection du pays: ")),
          backendTooltip(span(`data-toggle`="tooltip",
                              `data-placement`="right", 
                              `data-html` = "true",
                              title = "Vous pouvez choisir le pays. 
                            Cette sélection a un impact sur les données affichés", 
                              HTML('<i class="bi bi-question-circle"></i>'))),
          selectInput("countryInput", label = NULL,
                      choices = choices, selected = selection)
        )
      })
      
      ################## city selection filter
      output$city <- renderUI({
        selection <- filterStates$citySelected
        choices = c("Yaounde", "Douala", "Bafoussam", "Bertoua")
        tagList(
          div(class="sidebar-header", tags$a("Sélection de la ville: ")),
          backendTooltip(span(`data-toggle`="tooltip",
                              `data-placement`="right", 
                              `data-html` = "true",
                              title = "Vous pouvez choisir la ville 
                            Cette sélection a un impact sur les données affichés", 
                              HTML('<i class="bi bi-question-circle"></i>'))),
          selectInput("cityInput", label = NULL,
                      choices = choices, selected = selection)
        )
      })
      
      ############ Apply  Filter button
      output$filter_button <- renderUI({
        DefaultButton.shinyInput("filter_data", class = "btn-filter",
                                 text = "Appliquer le filtre",
                                 iconProps = list(iconName = "Refresh"),
                                 style = "background-color: #0093FF; color: #fff;"
        )
      })
      
      ########### Display select country
      output$countryselect <- renderUI({
        paste("Pays sélectionner: ",  filterStates$countrySelected, sep = "")
      })
      ########## End
    }
  })

}