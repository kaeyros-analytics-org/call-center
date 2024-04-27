
################ function to manage pivot item by id 
setClickedId <- function(inputId) {
  shiny.fluent::JS(glue::glue("item => Shiny.setInputValue('{inputId}', item.props.id)"))
}

mainContentRouter_ui <- function(id) {

  ns <- NS(id)
  # fluentPage(
  #   uiOutput(ns("mainContent"))
  # )
  fluentPage(withSpinner(uiOutput(ns("mainContent")),
                         type = 8,
                         color = 'grey', size = 0.7))
}

mainContentRouter_server <- function(input, output, session, filterStates) {
  ############# This UI is for ressources Layout Page
  ui_resquest_analysis = resquest_analysis_ui(session$ns("resquest_analysis"))

  ############# This UI is for Réemploies Layout Page
  ui_resolution_metrics = resolution_metrics_ui(session$ns("resolution_metrics"))
  
  
  ############# This UI is for recouvrement Layout Page
  ui_service_experience = Pivot(linkFormat = "tabs",
                       onLinkClick = setClickedId(session$ns("ressources_tabs")),
                       PivotItem(id = "customer_interaction", headerText = "Customer-Agent Interaction", customer_interaction_ui(session$ns("customer_interaction"))),
                       PivotItem(id = "call_sentiments", headerText = "Automation and Call sentiments", call_sentiments_ui(session$ns("call_sentiments"))), 
  )
  
  observeEvent(filterStates$dataNavi$dataset, 
    { print(paste("mon dataset: ", filterStates$dataNavi$dataset))
      # generate Ressources content ####
      if(filterStates$dataNavi$dataset == "Customer Resquest Analysis") {
        output$mainContent <- renderUI({
          div( id = "navtabs",
               ui_resquest_analysis
          )
        })
      # generate Réemploies content ####
      } else if(filterStates$dataNavi$dataset == "Resolution Metrics") {
        output$mainContent <- renderUI({
          ui_resolution_metrics
        })
        # generate Recouvrement content ####
      } else if(filterStates$dataNavi$dataset == "Customer Service Experience") {
        output$mainContent <- renderUI({
          ui_service_experience
        })
        # generate Production content ####
      } else { # Home
        output$mainContent <- renderUI({
          tagList(
            includeMarkdown("./www/htmlComponents/home.html"),
          )
        })
      }
      
  })

  observeEvent(input$ressources_tabs, {
    print("ok")
    cat(" dans Service Experience Vous avez cliqué sur le tabPanel avec l'ID :", input$ressources_tabs, "\n")
  })
  
  callModule(resquest_analysis_server, id = "resquest_analysis")
  callModule(call_sentiments_server, id = "call_sentiments")
  callModule(resolution_metrics_server, id = "resolution_metrics")
  callModule(customer_interaction_server, id = "customer_interaction")
}










