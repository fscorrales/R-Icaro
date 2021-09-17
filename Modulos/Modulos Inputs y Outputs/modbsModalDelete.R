# Ver
# https://stackoverflow.com/questions/40985684/r-shiny-present-a-shinybs-modal-popup-on-page-visit-no-user-action

bsModalDeleteUI <- function(id, bsTrigger = "NoEsNecesario") {
  
  ns <- NS(id)
  
  tagList(
    tags$head(tags$style(paste0("#", ns("bsModalBorrar"),  " .modal-footer{ display:none} ", 
                                "#", ns("bsModalBorrar"),  " .modal-header .close{display:none} ",
                                "#", ns("bsModalBorrar"),  " .modal({backdrop: 'static', keyboard: false})"))),
    bsModalNoClose(
      id = ns("bsModalBorrar"),
      title = "¡ADVERTENCIA!", trigger = bsTrigger,
      textOutput(ns("GlosaModalBorrar")),
      br(),
      footer = column(12, align='right',
               bsButton(ns('Cancelar'), 'Cancelar'),
               bsButton(ns('Aceptar'), 'Aceptar'),
               ),
      br(),
      br()
      )
  )
}

bsModalDeleteServer <- function(id, toggleOpen, Glosa, MostrarAceptar) {

  stopifnot(is.reactive(toggleOpen))
  stopifnot(is.reactive(Glosa))
  stopifnot(is.reactive(MostrarAceptar))
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$GlosaModalBorrar <- renderText(Glosa())
    
    observeEvent(MostrarAceptar(), {
      updateButton(session, ns("Aceptar"), disabled = !MostrarAceptar())
      # shinyjs::toggleElement("Aceptar", condition = MostrarAceptar())
    })
    
    observeEvent(input$Cancelar, {
      toggleModal(session, "bsModalBorrar")
    })

    Borrar <- reactiveVal(0)
    
    observeEvent(input$Aceptar, {
      
      n <- Borrar() + 1
      Borrar(n)
      
      toggleModal(session, "bsModalBorrar")
    })
    
    observeEvent(toggleOpen(), {
      req(toggleOpen)
      toggleModal(session, "bsModalBorrar", "open")
    })
    
    # reactive(toggleModal(session, "bsModalBorrar", "open"))
    
      # (toggleModal(session, "bsModalBorrar"))

    return({
      list(
        Borrar = Borrar
        # n =  (isolate(i()))
      )
    })
    
    # list(
    #   open = openR,
    #   n =  (isolate(i()))
    # )
  
  })
  
}