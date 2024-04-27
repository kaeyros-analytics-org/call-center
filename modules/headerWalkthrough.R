headerWalkthrough_ui <- function(id) {
  
  ns <- NS(id)
  
  out <- actionButton(ns("startTour"), label = HTML('<i class="bi bi-geo"></i>'))
  
  return(out)
  
}


headerWalkthrough_server <- function(input,
                             output,
                             session,
                             filterStates) {
  
  observeEvent(input$startTour, {
    print("Start Tour")
    
    if (filterStates$dataNavi$dataset != "Home") {
      
      dataset <- filterStates$dataNavi$dataset
      
      print(dataset)
      
      if (dataset == "Customer Resquest Analysis") tabset = "resquest_analysis"
      if (dataset == "Resolution Metrics") tabset = ""
      if (dataset == "Customer Service Experience") tabset = ""
      
      if (tabset == "resquest_analysis") {
        df <- helpText %>% 
          filter(tabpanel == "resquest_analysis")
        print(df)
        rintrojs::introjs(
          session,
          options = list(
            "nextLabel" = "Suivant",
            "prevLabel" = "Précédent",
            "skipLabel" = "Passer",
            "doneLabel" = "Ok",
            steps = data.frame(
              element = as.vector(df$container),
              intro = as.vector(df$text)
            )
          )
        )
      } else if (tabset == "network_situation") {
        df <- helpText %>% 
          filter(tabpanel == "network_situation")
        rintrojs::introjs(
          session,
          options = list(
            "nextLabel" = "Suivant",
            "prevLabel" = "Précédent",
            "skipLabel" = "Passer",
            "doneLabel" = "Ok",
            steps = data.frame(
              element = as.vector(df$container),
              intro = as.vector(df$text)
            )
          )
        )
      }
    } else if (filterStates$dataNavi$dataset == "Home") {
      df <- helpText %>% 
        filter(navtab == "allgemein") 
      rintrojs::introjs(
        session,
        options = list(
          "nextLabel" = "Suivant",
          "prevLabel" = "Précédent",
          "skipLabel" = "Passer",
          "doneLabel" = "Ok",
          steps = data.frame(
            element = as.vector(df$container),
            intro = as.vector(df$text)
          )
        )
      )
    }
    
    
  })
}
