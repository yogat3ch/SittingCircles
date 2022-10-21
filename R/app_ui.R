#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    bs4Dash::bs4DashPage(title = "Sitting Circles",
                         skin = "light",
      header = mod_navbar_ui("navbar"),
      sidebar = mod_sidebar_ui("sidebar"),
      body = mod_body_ui("body")
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    shinyjs::useShinyjs(),
#     shinyjs::extendShinyjs(text = "
#                            Shiny.addCustomMessageHandler('shinyjs-getcookie', function(params) { shinyjs.debugMessage('shinyjs: calling function \"getcookie\" with parameters:'); shinyjs.debugMessage(params); shinyjs.getcookie(params);});
# Shiny.addCustomMessageHandler('shinyjs-setcookie', function(params) { shinyjs.debugMessage('shinyjs: calling function \"setcookie\" with parameters:'); shinyjs.debugMessage(params); shinyjs.setcookie(params);});
# Shiny.addCustomMessageHandler('shinyjs-rmcookie', function(params) { shinyjs.debugMessage('shinyjs: calling function \"rmcookie\" with parameters:'); shinyjs.debugMessage(params); shinyjs.rmcookie(params);});
#   shinyjs.getcookie = function(params) {
# console.log(\"getcookie ran\")
#   var cookie = Cookies.get(\"shinyauthr\");
#   if (typeof cookie !== \"undefined\") {
#     Shiny.setInputValue(\"body-jscookie\", cookie, {priority: \"event\"});
#   } else {
#     var cookie = \"\";
#     Shiny.setInputValue(\"body-jscookie\", cookie, {priority: \"event\"});
#   }
#   return cookie;
# }
# shinyjs.setcookie = function(params) {
#   Cookies.set(\"shinyauthr\", escape(params), { expires: 7 });
#   Shiny.setInputValue(\"body-jscookie\", params, {priority: \"event\"});
# }
# shinyjs.rmcookie = function(params) {
#   Cookies.remove(\"shinyauthr\");
#   Shiny.setInputValue(\"body-jscookie\", \"\", {priority: \"event\"});
# }
#                            ", functions = c("getcookie", 
#                                             "setcookie", "rmcookie")),
#     shinyauthr:::jscookie_script(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'SittingCircles'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}
