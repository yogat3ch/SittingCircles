$(document).on("shiny:sessioninitialized", function(event) {
  debugger;
  Shiny.setInputValue('body-loaded', true)
})