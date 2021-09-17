# Ver
# https://stackoverflow.com/questions/40985684/r-shiny-present-a-shinybs-modal-popup-on-page-visit-no-user-action

bsModalEstructuraUI <- function(id, 
                                bsTrigger = "NoEsNecesario") {
  
  shinyjs::useShinyjs()
  shinyFeedback::useShinyFeedback()
  
  ns <- NS(id)
  
  tagList(
    tags$head(tags$style(paste0("#", ns("bsModalModificar"),  " .modal-footer{ display:none} ", 
                                "#", ns("bsModalModificar"),  " .modal-header .close{display:none} ",
                                "#", ns("bsModalModificar"),  " .modal({backdrop: 'static', keyboard: false})"))),
    bsModalNoClose(
      id = ns("bsModalModificar"),
      title = textOutput(ns("TituloModal")), trigger = bsTrigger,
      size = "medium",
      fluidRow(
        column(3, (textInput(ns("NroProg"), "Programa", placeholder = "00"))),
        column(9, disabled(textInput(ns("DescProg"), "")))
      ),
      conditionalPanel(condition = "!output.hide_Subprograma", ns = ns,
                       fluidRow(
                         column(3, textInput(ns("NroSubprog"), "Subprograma", placeholder = "00")),
                         column(9, disabled(textInput(ns("DescSubprog"), "")))
                       )),
      conditionalPanel(condition = "!output.hide_Proyecto", ns = ns,
                       fluidRow(
                         column(3, textInput(ns("NroProy"), "Proyecto", placeholder = "00")),
                         column(9, disabled(textInput(ns("DescProy"), "")))
                       )),
      conditionalPanel(condition = "!output.hide_Actividad", ns = ns,
                       fluidRow(
                         column(3, textInput(ns("NroAct"), "Actividad", placeholder = "00")),
                         column(9, disabled(textInput(ns("DescAct"), "")))
                       )),
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

bsModalEstructuraServer <- function(id, toggleOpen, IDModificacion, 
                                    Estructura, NroFila) {

  stopifnot(is.reactive(toggleOpen))
  stopifnot(is.reactive(IDModificacion))
  stopifnot(is.reactive(Estructura))
  stopifnot(is.reactive(NroFila))

  moduleServer(id, function(input, output, session) {
    
    CamposEstructura <- c("NroProg", "NroSubprog", "NroProy", "NroAct",
                          "DescProg", "DescSubprog", "DescProy", "DescAct")
    
    DatosFormulario <- reactive({
      data <- sapply(CamposEstructura, function(x) input[[x]])
      # data <- c(data, timestamp = epochTime())
      # data[2] <- sprintf("%02d", as.numeric(data[2]))
    })
    
    TipoEstructura <- reactive(str_length(Estructura()))
    
    output$hide_Subprograma <- reactive({
      
      condicion = TipoEstructura() <= 2

    })
    
    outputOptions(output, "hide_Subprograma", suspendWhenHidden = FALSE)
    
    output$hide_Proyecto <- reactive({
      
      condicion = TipoEstructura() < 8
      
    })
    
    outputOptions(output, "hide_Proyecto", suspendWhenHidden = FALSE)
    
    output$hide_Actividad <- reactive({
      
      condicion = TipoEstructura() < 11
      
    })
    
    outputOptions(output, "hide_Actividad", suspendWhenHidden = FALSE)

    EtiquetaTitulo <- reactive({

      if (IDModificacion() == 1){
        case_when(
          TipoEstructura() == 2 ~ "Agregar Nuevo Programa",
          TipoEstructura() == 5 ~ "Agregar Nuevo Subprograma",
          TipoEstructura() == 8 ~ "Agregar Nuevo Proyecto",
          TipoEstructura() == 11 ~ "Agregar Nueva Actividad",
          TRUE ~ "ERROR")
      } else{
        case_when(
          TipoEstructura() == 2 ~ "Editar Programa",
          TipoEstructura() == 5 ~ "Editar Subprograma",
          TipoEstructura() == 8 ~ "Editar Proyecto",
          TipoEstructura() == 11 ~ "Editar Actividad",
          TRUE ~ "ERROR")
      }

    })

    output$TituloModal <- renderText(EtiquetaTitulo())
    
    observeEvent(toggleOpen(), {
      
      req(toggleOpen() > 0)
      toggleModal(session, "bsModalModificar", "open")
      
      updateTextInput(session, "NroProg", value = "")
      updateTextInput(session, "NroSubprog", value = "")
      updateTextInput(session, "NroProy", value = "")
      updateTextInput(session, "NroAct", value = "")
      updateTextInput(session, "DescProg", value = "")
      updateTextInput(session, "DescSubprog", value = "")
      updateTextInput(session, "DescProy", value = "")
      updateTextInput(session, "DescAct", value = "")
      
      shinyjs::disable("NroProg")
      shinyjs::disable("NroSubprog")
      shinyjs::disable("NroProy")
      shinyjs::disable("NroAct")
      shinyjs::disable("DescProg")
      shinyjs::disable("DescSubprog")
      shinyjs::disable("DescProy")
      shinyjs::disable("DescAct")
      shinyFeedback::feedback("NroProg", show = FALSE)
      shinyFeedback::feedback("NroSubprog", show = FALSE)
      shinyFeedback::feedback("NroProy", show = FALSE)
      shinyFeedback::feedback("NroAct", show = FALSE)
      
      
      if (IDModificacion() == 2) {
        
        if (TipoEstructura() == 2) {
          updateTextInput(session, "NroProg", value = str_sub(Estructura(), 1, 2))
          shinyjs::enable("DescProg")
        }
        if (TipoEstructura() == 5) {
          updateTextInput(session, "NroProg", value = str_sub(Estructura(), 1, 2))
          updateTextInput(session, "NroSubprog", value = str_sub(Estructura(), -2))
          shinyjs::enable("DescSubprog")
        }
        if (TipoEstructura() == 8) {
          updateTextInput(session, "NroProg", value = str_sub(Estructura(), 1, 2))
          updateTextInput(session, "NroSubprog", value = str_sub(Estructura(), 4, 5))
          updateTextInput(session, "NroProy", value = str_sub(Estructura(), -2))
          shinyjs::enable("DescProy")
        }
        if (TipoEstructura() == 11) {
          updateTextInput(session, "NroProg", value = str_sub(Estructura(), 1, 2))
          updateTextInput(session, "NroSubprog", value = str_sub(Estructura(), 4, 5))
          updateTextInput(session, "NroProy", value = str_sub(Estructura(), 7, 8))
          updateTextInput(session, "NroAct", value = str_sub(Estructura(), -2))
          shinyjs::enable("DescAct")
        }

      } else {
        shinyjs::enable("NroProg")
        shinyjs::enable("NroSubprog")
        shinyjs::enable("NroProy")
        shinyjs::enable("NroAct")
        if (TipoEstructura() == 2) {
          shinyjs::enable("DescProg")
        }
        if (TipoEstructura() == 5) {
          shinyjs::enable("DescSubprog")
        }
        if (TipoEstructura() == 8) {
          shinyjs::enable("DescProy")
        }
        if (TipoEstructura() == 11) {
          shinyjs::enable("DescAct")
        }
      }

    })
    
    observeEvent(input$NroProg, {
      
      req(input$NroProg != "")
      Validez <- ValidarNroProg(TipoEstructura(), DatosFormulario(),
                                IDModificacion())
      
      if (Validez$EsValido) {
        updateTextInput(session, "DescProg", value = Validez$Descripcion)
      }
      
      # data <- ProgramasIcaro.df() %>% 
      #   filter(Programa == input$NroProg)
      # updateTextInput(session, "DescProg", value = unname(data["DescProg"]))
      # 
      # ShowAdvertencia <- nrow(data) < 1
      # if (TipoEstructura() == 2 & IDModificacion() == 1) {
      #   shinyFeedback::feedbackWarning("NroProg", !ShowAdvertencia, 
      #                                  "El Nro de Prog ya existe")        
      # } else{
      #   shinyFeedback::feedbackWarning("NroProg", ShowAdvertencia, 
      #                                  "No existe el Programa")        
      # }

    })
    
    observeEvent(input$NroSubprog, {

      req(input$NroSubprog != "")
      Validez <- ValidarNroSubprog(TipoEstructura(), DatosFormulario(),
                                   IDModificacion())
      
      if (Validez$EsValido) {
        updateTextInput(session, "DescSubprog", value = Validez$Descripcion)
      }
      
      
      # EstructuraBuscada <- paste0(input$NroProg, "-", 
      #                             input$NroSubprog)
      # data <- SubprogramasIcaro.df() %>%
      #   filter(Subprograma == EstructuraBuscada)
      # updateTextInput(session, "DescSubprog", value = unname(data["DescSubprog"]))
      # 
      # ShowAdvertencia <- nrow(data) < 1
      # if (TipoEstructura() == 5 & IDModificacion() == 1) {
      #   shinyFeedback::feedbackWarning("NroSubprog", !ShowAdvertencia, 
      #                                 "La estructura ya existe")        
      # } else{
      #   shinyFeedback::feedbackWarning("NroSubprog", ShowAdvertencia, 
      #                                  "No existe el Subprograma")        
      # }

    })
    
    observeEvent(input$NroProy, {

      req(input$NroProy != "")
      Validez <- ValidarNroProy(TipoEstructura(), DatosFormulario(),
                                IDModificacion())
      
      if (Validez$EsValido) {
        updateTextInput(session, "DescProy", value = Validez$Descripcion)
      }
      
      # EstructuraBuscada <- paste0(input$NroProg, "-", 
      #                             input$NroSubprog, "-", 
      #                             input$NroProy)
      # data <- ProyectosIcaro.df() %>%
      #   filter(Proyecto == EstructuraBuscada)
      # updateTextInput(session, "DescProy", value = unname(data["DescProy"]))
      # 
      # ShowAdvertencia <- nrow(data) < 1
      # if (TipoEstructura() == 8 & IDModificacion() == 1) {
      #   shinyFeedback::feedbackWarning("NroProy", !ShowAdvertencia, 
      #                                 "La estructura ya existe")        
      # } else{
      #   shinyFeedback::feedbackWarning("NroProy", ShowAdvertencia, 
      #                                  "No existe el Proyecto")        
      # }
      
    })

    observeEvent(input$NroAct, {

      req(input$NroAct != "")
      Validez <- ValidarNroAct(TipoEstructura(), DatosFormulario(),
                                IDModificacion())
      
      if (Validez$EsValido) {
        updateTextInput(session, "DescAct", value = Validez$Descripcion)
      }
      # EstructuraBuscada <- paste0(input$NroProg, "-", 
      #                             input$NroSubprog, "-", 
      #                             input$NroProy, "-",
      #                             input$NroAct)
      # data <- ActividadesIcaro.df() %>%
      #   filter(Actividad == EstructuraBuscada)
      # updateTextInput(session, "DescAct", value = unname(data["DescAct"]))
      # 
      # ShowAdvertencia <- nrow(data) < 1
      # if (TipoEstructura() == 11 & IDModificacion() == 1) {
      #   shinyFeedback::feedbackWarning("NroAct", !ShowAdvertencia, 
      #                                 "La estructura ya existe")        
      # } else{
      #   shinyFeedback::feedbackWarning("NroAct", ShowAdvertencia, 
      #                                  "No existe la Actividad")   
      # }

    })
    
    observeEvent(input$Cancelar, {
      
      toggleModal(session, "bsModalModificar", "close")
      
    })
    
    observeEvent(input$Aceptar, {
      
      ListaDeVariables <- list(IDModificacion(), TipoEstructura(),
                               DatosFormulario())

      if (invoke(ValidarEstructura, ListaDeVariables) == TRUE){
        
        #AGREGAR (1), EDITAR (2) y BORRAR (3) estructura programática

        AgregarEditarEstructuraICARO(IDModificacion(), TipoEstructura(), 
                                     DatosFormulario(), NroFila())
        
        # BlanquearEditandoEstructuraINVICO.rmd()
        toggleModal(session, "bsModalModificar", "close")
        
      }
      
    })
  
  })
  
}