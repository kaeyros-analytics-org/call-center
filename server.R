# Define server logic ----
server <- function(input, output, session) {
 
  ### Login logic server
  # res_auth <- shinymanager::secure_server(
  #   check_credentials = shinymanager::check_credentials(
  #     "./data/database.sqlite",
  #     #passphrase = keyring::key_get("R-shinymanager-key", "datacatalogapp")
  #     passphrase = "passphrase_wihtout_keyring"
  #   ), keep_token = FALSE) #keep_token = TRUE
  # 
  # data_connexion <- reactive({
  #   reactiveValuesToList(res_auth)
  # })

  ################### This code change the Tab Head Layout
  #Sys.sleep(1.5)
  
  observeEvent(input$datasetNav,
               {
                 filterStates$allDataset <- input$allNav
                 filterStates$dataNavi$dataset <- input$datasetNav
               })
  
  ######### Set the first active page Layout
  callModule(mainContentRouter_server, id = "mainContentRouter", filterStates = filterStates)
  
  ########## Call filter server module
  callModule(filterStatesRouter_server, id = "filterStates", filterStates = filterStates)
  
  ####### Call server module for start tour.
  callModule(headerWalkthrough_server, id = "walkthrough", filterStates = filterStates)
  
  ############## Icon selection for display modal form
  observeEvent(input$iconSelection, {
    callModule(headerFormModal_server, id = "formModal", iconSelection = input$iconSelection)
    callModule(headerFeedbackModal_server, id = "feedbackModal", iconSelection = input$iconSelection)
  })
  
  
  ################ Apply filter woth sidebar DATA
  observeEvent(input$filter_data, {
    print("Apply the filter")
    filterStates$countrySelected <- input$countryInput
    filterStates$date_start <- input$dateRangeInput[1]
    filterStates$date_end <- input$dateRangeInput[2]
    #filterStates$whoAsPrint <- input$eventTypePicker
    filterStates$whoAsPrint <- input$eventTypePicker
    filterStates$filterButton <- TRUE
  })
  
  
}