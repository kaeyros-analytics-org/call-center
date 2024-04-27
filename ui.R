
# ############## creation du keyring and connect
#keyring::key_set("R-shinymanager-key", "qc_reporting")
#keyring::key_set_with_value("R-shinymanager-key", "datacatalogapp", password = "Kaeyros@data@237@key")

######### Define Credential
# credentials <- data.frame(
#   user = c("user","admin"),
#   password = c("admin","admin"),
#   # password will automatically be hashed
#   admin = c(FALSE, TRUE),
#   grade = c("user","admin"),
#   stringsAsFactors = FALSE
# )
# ###############" Creation de la DB SQLite
# shinymanager::create_db(
#   credentials_data = credentials,
#   sqlite_path = "./data/database.sqlite", # will be created
#   #passphrase = keyring::key_get("R-shinymanager-key", "datacatalogapp")
#   passphrase = "passphrase_wihtout_keyring"
# )



#Change labels
shinymanager::set_labels(
  language = c("en"),
  "Please authenticate" = "Login"
)


ui <- function(id){
      shiny::htmlTemplate("www/index.html",
  
                 ############## Content of sideBar navigation
                 sidebarNavigation =
                   tagList(
                     introjsUI(),
                     useShinyjs(),
                     filterStatesRouter_ui("filterStates")
                   ),
  
                 ################## Content of Header Navigation
                 headerNav = htmlTemplate("./www/htmlComponents/headerNav.html",
                                          formModal = headerFormModal_ui(id = "formModal"),
                                          feedbackModal = headerFeedbackModal_ui(id = "feedbackModal"),
                                          methodsModal = headerMethodsModal_ui(id = "methodsModal"),
                                          dataModal = headerDataModal_ui(id = "data"),
                                          walkthrough = headerWalkthrough_ui(id = "walkthrough")
                 ),
                 ################# Content of Body display
                 contentSection = mainContentRouter_ui("mainContentRouter")
      )
}

# ui <- shinymanager::secure_app(ui,
#                                tags_top =
#                                  tags$div(
#                                    tags$head(
#                                      tags$link(rel = "stylesheet", type="text/css", href ="css/login.css"),
#                                      tags$script(src = "./js/login.js"),
#                                      tags$link(rel = "stylesheet",
#                                                href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"),
#                                    ),
#                                    tags$img(src = "images/logo_preview.png", width = 100)
#                                  ),
#                                tags_bottom = tags$div(
#                                  tags$a(
#                                    href = "https://kaeyros-analytics.com/#/pages/contact-us",
#                                    target="_blank", "Forgot password ?"
#                                  )
#                                ),
#                                enable_admin = TRUE, choose_language = FALSE, fab_position = "bottom-right")



