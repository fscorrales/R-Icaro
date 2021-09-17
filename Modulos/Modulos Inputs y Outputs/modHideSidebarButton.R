
HideSidebarButtonUI <- function(id) {
  tagList(
      (bsButton(NS(id,"ShowPanel"), "", icon = icon("bars"),
              type = "toggle", value = TRUE))
  )
}

HideSidebarButtonServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    input$ShowPanel
  })
}