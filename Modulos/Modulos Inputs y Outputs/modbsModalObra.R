# Ver
# https://stackoverflow.com/questions/40985684/r-shiny-present-a-shinybs-modal-popup-on-page-visit-no-user-action

bsModalObraUI <- function(id, bsTrigger = "NoEsNecesario") {
  
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
      size = "large",
      selectizeInput(ns("DescripcionObra"), "Descripcion Obra", choices = NULL, multiple = F, 
                     width = '400%', options = list(create = TRUE, 
                                    placeholder = 'Elegir una opción del listado o agregar una nueva')),
      fluidRow(
        column(4, selectizeInput(ns("CUITObra"), "CUIT Contratista",
                              choices = "", selected = "", multiple = F, 
                              options = list(placeholder = "Elegir una opción"))),
        column(8, selectizeInput(ns("DescCUITObra"), "Descripcion Contratista",
                                                   choices = "", selected = "", multiple = F,
                                                   options = list(placeholder = "Elegir una opción")))
      ),
      fluidRow(
        column(4, selectizeInput(ns("ImputacionObra"), "Imputacion Presupuestaria",
                              choices = "", selected = "", multiple = F, 
                              options = list(placeholder = "Elegir una opción"))),
        column(8, shinyjs::disabled(textInput(ns("DescImputacionObra"), "Descripcion Proyecto - Actividad",
                                              value = "", width = '200%')))
      ),
      fluidRow(
        column(4, selectizeInput(ns("PartidaObra"), "Partida Presupuestaria",
                              choices = "", selected = "", multiple = F, 
                              options = list(placeholder = "Elegir una opción"))),
        column(4),
        column(4, selectizeInput(ns("FuenteObra"), "Fuente Financiamiento",
                              choices = "", selected = "", multiple = F, 
                              options = list(placeholder = "Elegir una opción")))
      ),
      fluidRow(
        column(4, selectizeInput(ns("CuentaObra"), "Cuenta Bancaria",
                              choices = "", selected = "", multiple = F,
                              options = list(placeholder = "Elegir una opción"))),
        column(8, shinyjs::disabled(textInput(ns("DescCuentaObra"), "Denominacion Cuenta Bancaria",
                                              value = "", width = '200%')))
      ),
      fluidRow(
        column(4, selectizeInput(ns("LocalidadObra"), "Localidad",
                                 choices = "", selected = "",
                                 multiple = F, options = list(create = TRUE, 
                                                              placeholder = "Elegir o Agregar"))),
        column(4, textInput(ns("NormaLegalObra"), "Norma Legal", value = "")),
        column(4, selectizeInput(ns("InformacionAdicionalObra"), "Informacion Adicional",
                                 choices = "", selected = "",
                                 multiple = F, options = list(create = TRUE, 
                                                              placeholder = "Campo opcional...")))
      ),
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

bsModalObraServer <- function(id, toggleOpen, DatosOriginales) {

  stopifnot(is.reactive(toggleOpen))
  # stopifnot(is.reactive(IDModificacion))
  # stopifnot(is.reactive(DescripcionObra))
  stopifnot(is.reactive(DatosOriginales))
  
  Origen <- reactive(unlist(DatosOriginales()$Origen, use.names = FALSE))
  IDModificacion <- reactive((DatosOriginales()$IDModificacion))
  DescripcionObra <- reactive((DatosOriginales()$DescObra))
  CUIT <- reactive(unlist(DatosOriginales()$CUIT, use.names = FALSE))
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    updateCUITyDescCUIT <- reactiveValues(
      FromCUIT = FALSE,
      FromDescCUIT = FALSE
    )
    
    CamposEstructura <- c("DescripcionObra", "CUITObra", "DescCUITObra", "ImputacionObra",
                          "DescImputacionObra", "PartidaObra", "FuenteObra", "CuentaObra",
                          "DescCuentaObra", "LocalidadObra", "NormaLegalObra", 
                          "InformacionAdicionalObra")

    DatosFormulario <- reactive({
      data <- sapply(CamposEstructura, function(x) input[[x]])
      # data <- c(data, timestamp = epochTime())
      # data[2] <- sprintf("%02d", as.numeric(data[2]))
    })

    Leyendas <- reactiveValues(Titulo = "", BotonAceptar = "")
     
    observeEvent(IDModificacion(), {
       
      if (IDModificacion() == 1){
        Leyendas$Titulo <- "Agregar Obra"
        Leyendas$BotonAceptar <- "Agregar"
      } else{
        Leyendas$Titulo <- "Editar Obra"
        Leyendas$BotonAceptar <- "Editar"
      }
       
      output$TituloModal <- renderText(Leyendas$Titulo)
      updateButton(session, ns("Aceptar"), label = Leyendas$BotonAceptar)
      
    }, ignoreInit = TRUE)

    
    observeEvent(toggleOpen(), {
      
      req(toggleOpen() > 0)
      toggleModal(session, "bsModalModificar", "open")
      
      shinyjs::enable("DescripcionObra")
      shinyjs::enable("ImputacionObra")
      shinyjs::enable("PartidaObra")
      shinyjs::enable("CUITObra")
      shinyjs::enable("DescCUITObra")
      
      con <- ConectarBD()
      
      DescripcionObra <- dbGetQuery(
        con, "SELECT DISTINCT Descripcion FROM OBRAS")
      
      CUITObra <- dbGetQuery(
        con, "SELECT DISTINCT CUIT FROM PROVEEDORES")
      
      DescCUITObra <- dbGetQuery(
        con, "SELECT DISTINCT Descripcion FROM PROVEEDORES")
      # filter(!is.na(Descripcion))
      
      ImputacionObra <- dbGetQuery(
        con, "SELECT DISTINCT Actividad FROM ACTIVIDADES")
      
      PartidaObra <- dbGetQuery(
        con, "SELECT DISTINCT Partida FROM PARTIDAS")
      
      CuentaObra <- dbGetQuery(
        con, "SELECT DISTINCT Cuenta FROM CUENTASBANCARIAS")

      FuenteObra <- dbGetQuery(
        con, "SELECT DISTINCT Fuente FROM FUENTES")
      
      LocalidadObra <- dbGetQuery(
        con, "SELECT DISTINCT Localidad FROM OBRAS ORDER BY Localidad")
      # filter(!is.na(Localidad))

      InformacionAdicionalObra <- dbGetQuery(
        con, paste0("SELECT DISTINCT InformacionAdicional FROM OBRAS ",
                    "ORDER BY InformacionAdicional"))
      # filter(!is.na(Localidad))
      
      DesconectarBD(con)
      
      if (IDModificacion() == 1) {

        updateSelectizeInput(session, inputId = ('DescCUITObra'), choices = c("", DescCUITObra),
                             selected = character(0), server = TRUE) #CAMBIE
        
        if (is.null(Origen())) {
          
          updateSelectizeInput(session, inputId = ('DescripcionObra'), choices = c("", DescripcionObra), 
                               selected = character(0), server = TRUE)
          updateSelectizeInput(session, inputId = ('CUITObra'), choices = c("", CUITObra),
                               selected = character(0), server = TRUE) #CAMBIE

        } else if(!is.null(Origen())) {

          updateSelectizeInput(session, inputId = ('DescripcionObra'), choices = c(DescripcionObra()), 
                               selected = DescripcionObra(), server = TRUE)
          if (Origen() == "EPAM") {
            CUITObra = "30632351514"
            if (CUIT() != "30632351514") {
              CUITObra = c(CUITObra, CUIT())
            }
            
            updateSelectizeInput(session, inputId = ('CUITObra'), choices = c("", CUITObra),
                                 selected = CUIT(), server = TRUE) #CAMBIE
          } else {
            updateSelectizeInput(session, inputId = ('CUITObra'), choices = CUIT(),
                                 selected = CUIT(), server = TRUE) #CAMBIE
          }
          
          shinyjs::disable("DescripcionObra")
          # shinyjs::disable("CUITObra")
          shinyjs::disable("DescCUITObra")
          
        }

        updateSelectizeInput(session, inputId = ('ImputacionObra'), choices = c("", ImputacionObra),
                             selected = character(0), server = TRUE)
        updateSelectizeInput(session, inputId = ('PartidaObra'), choices = c("", PartidaObra),
                             selected = character(0), server = TRUE)
        updateSelectizeInput(session, inputId = ('CuentaObra'), choices = c("", CuentaObra),
                             selected = character(0), server = TRUE)
        updateSelectizeInput(session, inputId = ('FuenteObra'), choices = c("", FuenteObra),
                             selected = character(0), server = TRUE)
        updateSelectizeInput(session, inputId = ('LocalidadObra'), choices = c("", LocalidadObra), 
                             selected = character(0), server = TRUE)
        updateSelectizeInput(session, inputId = ('InformacionAdicionalObra'), choices = c("", InformacionAdicionalObra), 
                             selected = character(0), server = TRUE)
        updateTextInput(session, "DescImputacionObra", value = "")
        updateTextInput(session, "DescCuentaObra", value = "")
        updateTextInput(session, "NormaLegalObra", value = "")
        
      } else {
        
        #Dificil lograr en SQL sin un ID en OBRAS
        DescObra <- DescripcionObra()[[1]]
        updateSelectizeInput(session, inputId = ('DescripcionObra'), choices = c("", DescripcionObra), 
                             selected = DescObra, server = TRUE)
        shinyjs::disable("DescripcionObra")
        shinyjs::disable("ImputacionObra")
        shinyjs::disable("PartidaObra")
        
        data <- FiltrarBD(
          "SELECT * FROM OBRAS Where Descripcion = ?",
          params = DescObra
        )
        
        # data <- ObrasIcaro.df() %>%
        #   filter(Descripcion == DescObra)

        updateSelectizeInput(session, inputId = ('CUITObra'), choices = c("", CUITObra),
                             selected = (data[["CUIT"]]), server = TRUE)
        updateSelectizeInput(session, inputId = ('DescCUITObra'), choices = c("", DescCUITObra),
                             selected = character(0), server = TRUE)
        updateSelectizeInput(session, inputId = ('ImputacionObra'), choices = c("", ImputacionObra),
                             selected = (data[["Imputacion"]]), server = TRUE)
        updateSelectizeInput(session, inputId = ('PartidaObra'), choices = c("", PartidaObra),
                             selected = (data[["Partida"]]), server = TRUE)
        updateSelectizeInput(session, inputId = ('CuentaObra'), choices = c("", CuentaObra),
                             selected = (data[["Cuenta"]]), server = TRUE)
        updateSelectizeInput(session, inputId = ('FuenteObra'), choices = c("", FuenteObra),
                             selected = (data[["Fuente"]]), server = TRUE)
        updateSelectizeInput(session, inputId = ('LocalidadObra'), choices = c("", LocalidadObra), 
                             selected = (data[["Localidad"]]), server = TRUE)
        updateSelectizeInput(session, inputId = ('InformacionAdicionalObra'), choices = c("", InformacionAdicionalObra), 
                             selected = (data[["InformacionAdicional"]]), server = TRUE)
        updateTextInput(session, "NormaLegalObra", value = (data[["NormaLegal"]]))
        
      }


      
      shinyFeedback::feedback("DescripcionObra", show = FALSE)
      shinyFeedback::feedback("CUITObra", show = FALSE)
      shinyFeedback::feedback("DescCUITObra", show = FALSE)
      shinyFeedback::feedback("ImputacionObra", show = FALSE)
      shinyFeedback::feedback("PartidaObra", show = FALSE)
      shinyFeedback::feedback("FuenteObra", show = FALSE)
      shinyFeedback::feedback("CuentaObra", show = FALSE)
      shinyFeedback::feedback("LocalidadObra", show = FALSE)
      shinyFeedback::feedback("InformacionAdicional", show = FALSE)

    }, ignoreInit = TRUE)
    
    #VALIDAR
    observeEvent(input$DescripcionObra, {
      
      req(input$DescripcionObra)
      Validez <- ValidarDescripcionObra(DatosFormulario(), IDModificacion())
      
      # if (Validez$EsValido) {
      #   updateSelectizeInput(session, inputId = ('DescCUITObra'),
      #                        selected = Validez$Descripcion)
      # }
      
    }, ignoreInit = TRUE)
        
    observeEvent(input$CUITObra, {
      
      req(input$CUITObra)
      Validez <- ValidarCUITObra(DatosFormulario(), IDModificacion())

      if (Validez$EsValido) {
        
        if (updateCUITyDescCUIT$FromDescCUIT == FALSE) {
          
          DescCUITObra <- FiltrarBD("SELECT DISTINCT Descripcion FROM PROVEEDORES")
          
          updateCUITyDescCUIT$FromCUIT = TRUE
          
          updateSelectizeInput(session, inputId = ('DescCUITObra'), 
                               choices = c(DescCUITObra),
                               selected = Validez$Descripcion, server = TRUE)
        } else {
          updateCUITyDescCUIT$FromDescCUIT = FALSE
        }
        
      }

    }, ignoreInit = TRUE)
    
    observeEvent(input$DescCUITObra, {
      
      req(input$DescCUITObra)
      Validez <- ValidarDescCUITObra(DatosFormulario(), IDModificacion())

      if (Validez$EsValido) {
        
        if (updateCUITyDescCUIT$FromCUIT == FALSE) {
          
          CUITObra <- FiltrarBD("SELECT DISTINCT CUIT FROM PROVEEDORES")
          
          updateCUITyDescCUIT$FromDescCUIT = TRUE
          
          updateSelectizeInput(session, inputId = ('CUITObra'),
                               choices = c(CUITObra), server = TRUE,
                               selected = Validez$Descripcion)
        } else {
          updateCUITyDescCUIT$FromCUIT = FALSE
        }
        

      }

    }, ignoreInit = TRUE)
    
    observeEvent(input$ImputacionObra, {

      req(input$ImputacionObra)
      Validez <- ValidarImputacionObra(DatosFormulario(), IDModificacion())
      
      if (Validez$EsValido) {
        updateTextInput(session, "DescImputacionObra", value = Validez$Descripcion)
      }
      
    }, ignoreInit = TRUE)
    
    observeEvent(input$PartidaObra, {
      
      req(input$PartidaObra)
      Validez <- ValidarPartidaObra(DatosFormulario(), IDModificacion())
      
    }, ignoreInit = TRUE)
    
    observeEvent(input$FuenteObra, {
      
      req(input$FuenteObra)
      Validez <- ValidarFuenteObra(DatosFormulario(), IDModificacion())
      
    }, ignoreInit = TRUE)
    
    observeEvent(input$LocalidadObra, {
      
      req(input$LocalidadObra)
      Validez <- ValidarLocalidadObra(DatosFormulario(), IDModificacion())
      
    }, ignoreInit = TRUE)
    
    observeEvent(input$CuentaObra, {
      
      req(input$CuentaObra)
      Validez <- ValidarCuentaObra(DatosFormulario(), IDModificacion())
      
      if (Validez$EsValido) {
        updateSelectizeInput(session, inputId = ('DescCuentaObra'),
                             selected = Validez$Descripcion)
      }
      
    }, ignoreInit = TRUE)
    
    observeEvent(input$Aceptar, {
      
      ListaDeVariables <- list(IDModificacion(), DatosFormulario())

      if (invoke(ValidarObra, ListaDeVariables) == TRUE){

        #AGREGAR (1), EDITAR (2) y BORRAR (3) estructura programática

        AgregarEditarObraICARO(IDModificacion(), DatosFormulario())

        toggleModal(session, "bsModalModificar", "close")

      }

    }, ignoreInit = TRUE)
    
    observeEvent(input$Cancelar, {
      
      toggleModal(session, "bsModalModificar", "close")
      
    })
    
  
  })
  
}