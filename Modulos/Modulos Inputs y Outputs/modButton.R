ButtonUI <- function(id, Leyenda = "") {
  tagList(
    useShinyjs(),
    disabled(actionButton("Boton", Leyenda))
  )
}

ButtonServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # input$Boton
  })
}