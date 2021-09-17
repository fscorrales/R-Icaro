# Ver
# https://stackoverflow.com/questions/40985684/r-shiny-present-a-shinybs-modal-popup-on-page-visit-no-user-action

bsModalAutocargaUI <- function(id, bsTrigger = "NoEsNecesario") {
  
  shinyjs::useShinyjs()
  shinyFeedback::useShinyFeedback()
  
  ns <- NS(id)
  # DescripcionObra <- (ObrasIcaro.df()["Descripcion"])
  
  tagList(
    tags$head(tags$style(paste0("#", ns("bsModalModificar"),  " .modal-footer{ display:none} ", 
                                "#", ns("bsModalModificar"),  " .modal-header .close{display:none} ",
                                "#", ns("bsModalModificar"),  " .modal({backdrop: 'static', keyboard: false})"))),
    bsModalNoClose(
      id = ns("bsModalModificar"),
      title = textOutput(ns("TituloModal")), trigger = bsTrigger,
      size = "medium",
      fluidRow(
        column(4, shinyjs::disabled(textInput(ns("CUITAutocarga"), "CUIT Contratista", value = ""))),
        column(8, selectizeInput(ns("DescCUITAutocarga"), "Descripcion Contratista",
                                 choices = "", selected = "", multiple = F, width = '200%',
                                 options = list(placeholder = "Elegir una opción")))
      ),
      selectizeInput(ns("DescObraAutocarga"), "Descripcion Obra", choices = NULL, multiple = F, 
                     width = '400%', options = list(placeholder = 'Elegir una opción del listado')
                     ),
      fluidRow(
        column(4, numericInput(ns("ImporteBrutoAutocarga"), "Importe Bruto", value = 0)),
        column(4, selectizeInput(ns("TotalRetenidoAutocarga"), "Total Retenido",
                                 choices = "", selected = "", multiple = F, 
                                 options = list(placeholder = "Elegir una opción"))),
        column(4, shinyjs::disabled(textInput(ns("ImporteNetoAutocarga"), "Importe Neto", value = "")))
      ),
      br(),
      footer = column(12, align='right',
               bsButton(ns('Cancelar'), 'Cancelar'),
               bsButton(ns('Aceptar'), 'Editar'),
               ),
      br(),
      br()
      )
  )
}

bsModalAutocargaServer <- function(id, toggleOpen, DatosOriginales) {

  stopifnot(is.reactive(toggleOpen))
  stopifnot(is.reactive(DatosOriginales))

  Origen <- reactive(DatosOriginales()$Origen)
  DescripcionObra <- reactive(DatosOriginales()$DescObra)
  NroCUIT <- reactive(DatosOriginales()$CUIT)
  DescCUIT <- reactive(DatosOriginales()$DescCUIT)
  ImporteBruto <- reactive(DatosOriginales()$ImporteBruto)
  TotalRetenido <- reactive(DatosOriginales()$TotalRetenido)
  ImporteNeto <- reactive(DatosOriginales()$ImporteNeto)
  
  moduleServer(id, function(input, output, session) {
    
    CamposEstructura <- c("CUITAutocarga", "DescCUITAutocarga",
                          "DescObraAutocarga",
                          "ImporteBrutoAutocarga", "TotalRetenidoAutocarga", "ImporteNetoAutocarga")

    DatosFormulario <- reactive({
      data <- sapply(CamposEstructura, function(x) input[[x]])
    })

    Leyendas <- reactiveValues(Titulo = "", BotonAceptar = "")

    observeEvent(Origen(), {

      if (Origen() == "Obras"){
        Leyendas$Titulo <- "Editar Registro Autocarga OBRAS"
        # Leyendas$BotonAceptar <- "Agregar"
      } else{
        Leyendas$Titulo <- "Editar Registro Autocarga EPAM"
        # Leyendas$BotonAceptar <- "Editar"
      }

      output$TituloModal <- renderText(Leyendas$Titulo)
      # updateButton(session, ns("Aceptar"), label = Leyendas$BotonAceptar)

    })

    observeEvent(toggleOpen(), {

      req(toggleOpen() > 0)
      toggleModal(session, "bsModalModificar", "open")

      DescCUITChoices <- FiltrarBD(
        "SELECT DISTINCT Descripcion FROM PROVEEDORES"
      )
      # DescCUITChoices <- (ProveedoresSGF.df() %>%
      #   filter(!is.na(Descripcion)) %>%
      #   select(Descripcion) %>%
      #   unique())

      data <- DescCUITChoices %>%
        filter(Descripcion == DescCUIT())
      Existe <- nrow(data) >= 1
      
      DescCUITChoices <- DescCUITChoices[["Descripcion"]]

      if (!Existe) {
        DescCUITChoices <- c(DescCUIT(), DescCUITChoices)
      }
      
      updateSelectizeInput(session, inputId = ('DescCUITAutocarga'), choices = DescCUITChoices,
                           selected = DescCUIT(), server = TRUE)
      updateTextInput(session, "CUITAutocarga", value = NroCUIT())
      updateNumericInput(session, "ImporteBrutoAutocarga", value = ImporteBruto())
      updateSelectizeInput(session, inputId = ('TotalRetenidoAutocarga'), choices = c("0", TotalRetenido()),
                           selected = TotalRetenido(), server = TRUE)
      updateNumericInput(session, "ImporteNetoAutocarga", value = ImporteNeto())
      
      updateSelectizeInput(session, inputId = ('DescObraAutocarga'), choices = DescripcionObra(),
                           selected = DescripcionObra(), server = TRUE)
      shinyjs::disable("DescObraAutocarga")

    })

    observeEvent(input$DescCUITAutocarga, {

      req(input$DescCUITAutocarga)
      Validez <- ValidarDescCUITEditarAutocarga(DatosFormulario())

      if (Validez$EsValido) {

        updateTextInput(session, inputId = ('CUITAutocarga'),
                        value = Validez$Descripcion)
      }

    }, ignoreInit = TRUE)

    observeEvent(input$ImporteBrutoAutocarga, {

      # req(input$ImporteBrutoAutocarga)
      Validez <- ValidarImporteBrutoEditarAutocarga(DatosFormulario())
      
        if (Validez$EsValido) {
          req(input$TotalRetenidoAutocarga)
          ImporteNeto = Validez$Descripcion - as.numeric(input$TotalRetenidoAutocarga)
          updateTextInput(session, inputId = ('ImporteNetoAutocarga'),
                        value = ImporteNeto)
        } else {
          updateTextInput(session, inputId = ('ImporteNetoAutocarga'),
                          value = 0)
        }

    }, ignoreInit = TRUE)
    
    observeEvent(input$TotalRetenidoAutocarga, {
      
      # req(input$ImporteBrutoAutocarga)
      Validez <- ValidarTotalRetenidoEditarAutocarga(DatosFormulario())
      
      if (Validez$EsValido) {
        req(input$ImporteBrutoAutocarga)
        ImporteNeto = as.numeric(input$ImporteBrutoAutocarga) - Validez$Descripcion
        updateTextInput(session, inputId = ('ImporteNetoAutocarga'),
                        value = ImporteNeto)
      } else {
        updateTextInput(session, inputId = ('ImporteNetoAutocarga'),
                        value = 0)
      }
      
    }, ignoreInit = TRUE)

    observeEvent(input$Aceptar, {

      ListaDeVariables <- list(DatosFormulario())

      if (invoke(ValidarEditarAutocarga, ListaDeVariables) == TRUE){

        EditarAutocargaICARO(DatosOriginales(), DatosFormulario())

        CleanBsModalAutocarga(session)
        toggleModal(session, "bsModalModificar", "close")

      }

    })

    observeEvent(input$Cancelar, {

      CleanBsModalAutocarga(session)
      toggleModal(session, "bsModalModificar", "close")

    })
    
  
  })
  
}