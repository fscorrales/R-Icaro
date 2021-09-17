# Ver
# https://stackoverflow.com/questions/40985684/r-shiny-present-a-shinybs-modal-popup-on-page-visit-no-user-action

bsModalRegistroUI <- function(id, bsTrigger = "NoEsNecesario") {
  
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
        column(4, numericInput(ns("NroComprobanteRegistro"), "Nro Comprobante", 
                            value = NULL, min = 1, max = 99999)),
        column(3, dateInput(ns("FechaRegistro"), "Fecha", value = Sys.time(), 
                            format = "dd-mm-yyyy", language = "es")),
        column(3, shinyjs::disabled(textInput(ns("ComprobanteRegistro"), "Nro ICARO",
                                              value = ""))),
        column(2, (selectizeInput(ns("TipoRegistro"), "Tipo",
                                  choices = c("CYO", "REG", "PA6"), selected = "CYO", multiple = F)))
      ),
      fluidRow(
        
        column(4, selectizeInput(ns("CUITRegistro"), 
                                 tagList(
                                   tags$span("CUIT Contratista"), 
                                   tags$span(icon("info-circle"), id = ns("iconCUIT"), style = "color: blue;")
                                 ),
                                 choices = "", selected = "", multiple = F, 
                                 options = list(placeholder = "Elegir una opción"))),
        column(8, selectizeInput(ns("DescCUITRegistro"), "Descripcion Contratista",
                                 choices = "", selected = "", multiple = F, width = '200%',
                                 options = list(placeholder = "Elegir una opción")))
      ),
      selectizeInput(ns("DescObraRegistro"), 
                     tagList(
                       tags$span("Descripcion Obra"), 
                       tags$span(icon("info-circle"), id = ns("iconDescObra"), style = "color: blue;")
                     ),
                     choices = NULL, multiple = F, 
                     width = '400%', options = list(placeholder = 'Elegir una opción del listado')
                     ),
      fluidRow(
        column(4, selectizeInput(ns("CuentaRegistro"), "Cuenta Bancaria",
                                 choices = "", selected = "", multiple = F,
                                 options = list(placeholder = "Elegir una opción"))),
        column(8, shinyjs::disabled(textInput(ns("DescCuentaRegistro"), "Denominacion Cuenta Bancaria",
                                              value = "", width = '200%')))
      ),
      fluidRow(
        column(4, selectizeInput(ns("FuenteRegistro"), "Fuente Financiamiento",
                                 choices = "", selected = "", multiple = F, 
                                 options = list(placeholder = "Elegir una opción"))),
        column(8, shinyjs::disabled(textInput(ns("DescFuenteRegistro"), "Denominacion Fuente",
                                              value = "", width = '200%')))
      ),
      fluidRow(
        column(4, numericInput(ns("MontoBrutoRegistro"), "Monto Bruto", value = 0)),
        column(4, numericInputIcon(ns("AvanceRegistro"), "Avance Fisico Acum. %", value = 0,
                                   min = 0, max = 100, icon = list(NULL, icon("percent")))),
        column(4, textInput(ns("CertificadoRegistro"), "Nro Certificado", value = ""))
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

bsModalRegistroServer <- function(id, toggleOpen, DatosOriginales) {

  stopifnot(is.reactive(toggleOpen))
  # stopifnot(is.reactive(IDModificacion))
  stopifnot(is.reactive(DatosOriginales))
  # stopifnot(is.reactive(NroFila))
  
  # Origen <- reactive(unlist(DatosOriginales()$Origen, use.names = FALSE))
  IDModificacion <- reactive((DatosOriginales()$IDModificacion))
  NroComprobante <- reactive((DatosOriginales()$NroComprobante))
  # DescripcionObra <- reactive({
  #   Ans <- FiltrarBD(
  #     paste0("SELECT DISTINCT Descripcion, Imputacion, Partida ",
  #            "FROM OBRAS WHERE Descripcion = ?"),
  #     params = DatosOriginales()[["DescObra"]]
  #   )
  #   Ans <- paste0(Ans[["Descripcion"]], " (", 
  #                 Ans[["Imputacion"]], "-",
  #                 Ans[["Partida"]], ")")
  #   
  # })
  
  # NroCUIT <- reactive(unique(DatosOriginales()[["CUIT"]]))
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    shinyBS::addPopover(session, ns("iconCUIT"), "Opciones Listado CUIT:", 
                        placement = "right",
                        content = paste0(
                          "<ul>",
                          "<li><strong>Al seleccionar un CUIT</strong> del listado, automáticamente ",
                          "se completará su descripción y el listado con las obras que haya ejecutado</li>",
                          "<li>Si desconoce el CUIT, <strong>puede seleccionar su descripción</strong> ",
                          "logrando el mismo efecto</li>",
                          "<li>Si el <strong>CUIT queda en blanco</strong>, se blanqueará su descripcion ",
                          "y se listarán todas las obras del sistema</li>",
                          "</ul>")
                        )
    
    shinyBS::addPopover(session, ns("iconDescObra"), "Opciones Listado Obras:", 
                        placement = "right",
                        content = paste0(
                          "<ul>",
                          "<li><strong>Si ha seleccionado un CUIT</strong>, solo podrá elegir ",
                          "del listado <strong>las obras vinculadas con el mismo</strong></li>",
                          "<li>Si el <strong>CUIT está en blanco</strong>, podrá seleccionar ",
                          "<strong>cualquier obra del sistema</strong> y luego el CUIT que ",
                          "desea asociar a la misma</li>",
                          "</ul>")
                        )
    
    updateCUITyDescCUIT <- reactiveValues(
      FromCUIT = FALSE,
      FromDescCUIT = FALSE
    )
    
    updateFuenteyCuenta <- reactiveVal(TRUE)
    
    CamposEstructura <- c("FechaRegistro", "NroComprobanteRegistro", "ComprobanteRegistro", 
                          "TipoRegistro", "DescObraRegistro", "CUITRegistro", "DescCUITRegistro", 
                          "MontoBrutoRegistro", "AvanceRegistro", "CertificadoRegistro", 
                          "FuenteRegistro", "DescFuenteRegistro",
                          "CuentaRegistro", "DescCuentaRegistro")

    DatosFormulario <- reactive({
      data <- sapply(CamposEstructura, function(x) input[[x]])
      data["DescObraRegistro"] <- str_sub(data["DescObraRegistro"], 1,
                                            (str_length(data["DescObraRegistro"]) - 18))
      data
      # data <- c(data, timestamp = epochTime())
      # data[2] <- sprintf("%02d", as.numeric(data[2]))
    })

    Leyendas <- reactiveValues(Titulo = "", BotonAceptar = "")

    observeEvent(IDModificacion(), {

      if (IDModificacion() == 1){
        Leyendas$Titulo <- "Agregar Comprobante Gasto"
        Leyendas$BotonAceptar <- "Agregar"
      } else{
        Leyendas$Titulo <- "Editar Comprobante Gasto"
        Leyendas$BotonAceptar <- "Editar"
      }

      output$TituloModal <- renderText(Leyendas$Titulo)
      updateButton(session, ns("Aceptar"), label = Leyendas$BotonAceptar)

    })


    observeEvent(toggleOpen(), {

      req(toggleOpen() > 0)
      toggleModal(session, "bsModalModificar", "open")

      Origen <- (unlist(DatosOriginales()$Origen, use.names = FALSE))
      # IDModificacion <- reactive((DatosOriginales()$IDModificacion))
      # NroComprobante <- reactive((DatosOriginales()$NroComprobante))
      if (!is.null(DatosOriginales()[["DescObra"]])) {
        DescripcionObra <- FiltrarBD(
          paste0("SELECT DISTINCT Descripcion, Imputacion, Partida ",
                 "FROM OBRAS WHERE Descripcion = ?"),
          params = DatosOriginales()[["DescObra"]]
        )
        DescripcionObra <- paste0(DescripcionObra[["Descripcion"]], " (", 
                                  DescripcionObra[["Imputacion"]], "-",
                                  DescripcionObra[["Partida"]], ")")        
      }
        
      
      NroCUIT <- (unique(DatosOriginales()[["CUIT"]]))
      TipoComprobante <-(unique(DatosOriginales()[["Tipo"]]))
      
      shinyjs::enable("CUITRegistro")
      shinyjs::enable("DescCUITRegistro")
      shinyjs::enable("DescObraRegistro")
      shinyjs::enable("MontoBrutoRegistro")

      con <- ConectarBD()
      DescObraRegistro <- dbGetQuery(
        con, "SELECT DISTINCT Descripcion, Imputacion, Partida FROM OBRAS")
      DescObraRegistro <- paste0(DescObraRegistro[["Descripcion"]], " (", 
                                 DescObraRegistro[["Imputacion"]], "-",
                                 DescObraRegistro[["Partida"]], ")")
      
      
      CUITRegistro <- dbGetQuery(
        con, "SELECT DISTINCT CUIT FROM PROVEEDORES")
      
      DescCUITRegistro <- dbGetQuery(
        con, "SELECT DISTINCT Descripcion FROM PROVEEDORES")
      
      CuentaRegistro <- dbGetQuery(
        con, "SELECT DISTINCT Cuenta FROM CUENTASBANCARIAS")
      
      FuenteRegistro <- dbGetQuery(
        con, "SELECT DISTINCT Fuente FROM FUENTES")
      DesconectarBD(con)

      updateSelectizeInput(session, inputId = ('CuentaRegistro'), choices = c("", CuentaRegistro),
                           selected = character(0), server = TRUE)
      updateSelectizeInput(session, inputId = ('FuenteRegistro'), choices = c("", FuenteRegistro),
                           selected = character(0), server = TRUE)

      if (IDModificacion() == 1) {
        
        updateSelectizeInput(session, inputId = ('DescCUITRegistro'), choices = c("", DescCUITRegistro),
                             selected = character(0), server = TRUE)

        if (Origen == "") {

          updateSelectizeInput(session, inputId = ('CUITRegistro'), choices = c("", CUITRegistro),
                               selected = character(0), server = TRUE)
          updateSelectizeInput(session, inputId = ('DescObraRegistro'), choices = c("", DescObraRegistro),
                               selected = character(0), server = TRUE)
          updateTextInput(session, "DescCuentaRegistro", value = "")
          updateTextInput(session, "DescFuenteRegistro", value = "")

        } else if (Origen != "") {

          # updateSelectizeInput(session, inputId = ('CUITRegistro'), choices = c("", CUITRegistro),
          #                      selected = NroCUIT(), server = TRUE)
          updateNumericInput(session, "MontoBrutoRegistro",
                             value = DatosOriginales()[["ImporteBruto"]])
          updateTextInput(session, "CertificadoRegistro",
                          value = DatosOriginales()[["NroCertificado"]])
          
          shinyjs::disable("MontoBrutoRegistro")

          if (Origen == "Obras") {
            shinyjs::disable("CUITRegistro")
            shinyjs::disable("DescCUITRegistro")


            updateSelectizeInput(session, inputId = ('DescObraRegistro'), choices = DescripcionObra,
                                 selected = DescripcionObra, server = TRUE)
            shinyjs::disable("DescObraRegistro")
          } else if(Origen == "EPAM") {

            DescObraRegistro <- FiltrarBD(
              "SELECT Descripcion, Imputacion, Partida FROM OBRAS WHERE CUIT = ?",
              params = NroCUIT
            )
            
            DescObraRegistro <- paste0(DescObraRegistro[["Descripcion"]], " (", 
                                       DescObraRegistro[["Imputacion"]], "-",
                                       DescObraRegistro[["Partida"]], ")")

            updateSelectizeInput(session, inputId = ('DescObraRegistro'), choices = c("", DescObraRegistro),
                                 selected = DescripcionObra, server = TRUE)
          }
          
          updateSelectizeInput(session, inputId = ('CUITRegistro'),
                               choices = c(CUITRegistro),
                               selected = NroCUIT, server = TRUE)

        }

      }
      
      if ((IDModificacion() == 2)) {

        updateNumericInput(session = session, "NroComprobanteRegistro",
                        value = as.numeric(str_sub(NroComprobante(), 1, 5)))
        

        data <- FiltrarBD(
          "SELECT * FROM CARGA WHERE Comprobante = ? AND Tipo = ?",
          params = c(NroComprobante(), TipoComprobante)
        )
        
        DescObraObras <- FiltrarBD(
          "SELECT Descripcion, Imputacion, Partida FROM OBRAS WHERE CUIT = ?",
          params = data[["CUIT"]]
        )
        
        DescObraRegistro <- FiltrarBD(
          "SELECT DISTINCT Obra, Imputacion, Partida FROM CARGA WHERE CUIT = ?",
          params = data[["CUIT"]]
        )
        
        DescObraRegistro <- DescObraRegistro %>% 
          rename(Descripcion = Obra) %>% 
          filter(!(Descripcion %in% DescObraObras[["Descripcion"]])) %>%
          bind_rows(DescObraObras) %>% 
          unique()


        DescObraRegistro <- paste0(DescObraRegistro[["Descripcion"]], " (",
                                   DescObraRegistro[["Imputacion"]], "-",
                                   DescObraRegistro[["Partida"]], ")")
        
        DescObraSeleccionado <- paste0(data[["Obra"]], " (", 
                                       data[["Imputacion"]], "-",
                                       data[["Partida"]], ")")
        
        updateSelectizeInput(session, inputId = ('CUITRegistro'), 
                             choices = c(CUITRegistro),
                             selected = data[["CUIT"]], server = TRUE)
        updateFuenteyCuenta(FALSE)
        updateSelectizeInput(session, inputId = ('DescObraRegistro'), 
                             choices = c(DescObraRegistro),
                             selected = DescObraSeleccionado, 
                             server = FALSE)
        updateSelectizeInput(session = session, "TipoRegistro",
                             selected = (data[["Tipo"]]))
        updateSelectizeInput(session, inputId = ('CuentaRegistro'), choices = CuentaRegistro,
                             selected = (data[["Cuenta"]]))
        updateSelectizeInput(session, inputId = ('FuenteRegistro'), choices = FuenteRegistro, 
                             selected = (data[["Fuente"]]))
        updateDateInput(session, "FechaRegistro",
                        value = zoo::as.Date((data[["Fecha"]])))
        updateNumericInput(session, "MontoBrutoRegistro",
                           value = (data[["Importe"]]))
        updateTextInput(session, "CertificadoRegistro",
                        value = (data[["Certificado"]]))
        updateNumericInputIcon(session, "AvanceRegistro",
                               value = (data[["Avance"]]) * 100)

        #Es necesario desabilitar tanto???
        if (Origen != "") {
          # shinyjs::disable("CUITRegistro")
          # shinyjs::disable("DescCUITRegistro")
          shinyjs::disable("MontoBrutoRegistro")
          if (Origen == "Obras") {
            # shinyjs::disable("DescObraRegistro")
          } else if(Origen == "EPAM") {
            # shinyjs::disable("MontoBrutoRegistro")
          }
        }

      }
  
      # shinyFeedback::feedback("LocalidadObra", show = FALSE)

    })

    observe({
      
      updateTextInput(session, "ComprobanteRegistro", value = "")
      
      req(input$FechaRegistro)
      req(input$NroComprobanteRegistro)
      
      NroComprobanteConCeros <- (sprintf("%05d", as.numeric(input$NroComprobanteRegistro)))
      
      FechaEjercicio <- (format((input$FechaRegistro), format="%y"))
      
      ComprobanteCompleto <- paste(NroComprobanteConCeros,
                                   FechaEjercicio, sep = "/")
      
      updateTextInput(session, "ComprobanteRegistro",
                      value = ComprobanteCompleto)
      
    })
    
    #VALIDAR
    
    observeEvent(input$NroComprobanteRegistro, {
      
      
      req(input$NroComprobanteRegistro)
      Validez <- ValidarNroComprobanteRegistro(DatosFormulario(), IDModificacion())
      
    }, ignoreInit = TRUE)
    
    observeEvent(input$FechaRegistro, {
      
      
      req(input$FechaRegistro)
      Validez <- ValidarFechaRegistro(DatosFormulario())
      
    }, ignoreInit = TRUE)
    
    observeEvent(input$ComprobanteRegistro, {
      
      
      req(input$ComprobanteRegistro)
      Validez <- ValidarComprobanteRegistro(DatosFormulario(), IDModificacion(),
                                            NroComprobante())
      
    }, ignoreInit = TRUE)
    
    observeEvent(input$CUITRegistro, {

      if (input$CUITRegistro == "") {
        
        DescObraRegistro <- FiltrarBD(
          "SELECT DISTINCT Descripcion, Imputacion, Partida FROM OBRAS"
        )
        DescObraRegistro <- paste0(DescObraRegistro[["Descripcion"]], " (", 
                                   DescObraRegistro[["Imputacion"]], "-",
                                   DescObraRegistro[["Partida"]], ")")
        updateSelectizeInput(session, inputId = ('DescObraRegistro'), choices = c("", DescObraRegistro),
                             selected = character(0), server = TRUE)
        
        DescCUITRegistro <- FiltrarBD(
          "SELECT DISTINCT Descripcion FROM PROVEEDORES"
        )
        DescCUITRegistro <- DescCUITRegistro[["Descripcion"]]
        updateSelectizeInput(session, inputId = ('DescCUITRegistro'), choices = c("", DescCUITRegistro),
                             selected = character(0), server = TRUE)
        
      }
      
      req(input$CUITRegistro)
      Validez <- ValidarCUITRegistro(DatosFormulario())

      if (Validez$EsValido) {
        
        if (updateCUITyDescCUIT$FromDescCUIT == FALSE) {
          
          DescCUITRegistro <- FiltrarBD("SELECT DISTINCT Descripcion FROM PROVEEDORES")
          
          updateCUITyDescCUIT$FromCUIT = TRUE
          
          updateSelectizeInput(session, inputId = ('DescCUITRegistro'), 
                               choices = c(DescCUITRegistro),
                               selected = Validez$Descripcion, server = TRUE)
        } else {
          updateCUITyDescCUIT$FromDescCUIT = FALSE
        }
        
        req(input$DescObraRegistro == "")
        
        DescObraObras <- FiltrarBD(
          paste0("SELECT DISTINCT Descripcion, Imputacion, Partida ",
                 "FROM OBRAS WHERE CUIT = ?"),
          params = input$CUITRegistro
          )
        
        DescObraRegistro <- FiltrarBD(
          paste0("SELECT DISTINCT Obra, Imputacion, Partida ",
                 "FROM CARGA WHERE CUIT = ?"),
          params = input$CUITRegistro
        )
        
        DescObraRegistro <- DescObraRegistro %>% 
          rename(Descripcion = Obra) %>%
          filter(!(Descripcion %in% DescObraObras[["Descripcion"]])) %>% 
          bind_rows(DescObraObras) %>% 
          unique()

        DescObraRegistro <- paste0(DescObraRegistro[["Descripcion"]], " (", 
                                   DescObraRegistro[["Imputacion"]], "-",
                                   DescObraRegistro[["Partida"]], ")")
        
        updateSelectizeInput(session, inputId = ('DescObraRegistro'), choices = c("", DescObraRegistro),
                             selected = character(0), server = TRUE)
        
      }

    }, ignoreInit = TRUE)

    observeEvent(input$DescCUITRegistro, {

      req(input$DescCUITRegistro)
      Validez <- ValidarDescCUITRegistro(DatosFormulario())

      if (Validez$EsValido) {
        
        if (updateCUITyDescCUIT$FromCUIT == FALSE) {
          
          CUITRegistro <- FiltrarBD("SELECT DISTINCT CUIT FROM PROVEEDORES")
          
          updateCUITyDescCUIT$FromDescCUIT = TRUE
          
          updateSelectizeInput(session, inputId = ('CUITRegistro'),
                               choices = c(CUITRegistro), server = TRUE,
                               selected = Validez$Descripcion)
        } else {
          updateCUITyDescCUIT$FromCUIT = FALSE
        }

      }

    }, ignoreInit = TRUE)
    
    observeEvent(input$DescObraRegistro, {
      
      req(input$DescObraRegistro)
      Validez <- ValidarDescObraRegistro(DatosFormulario())
      
      
      if (Validez$EsValido) {
        
        if (updateFuenteyCuenta()){
         
          #Cargamos Cuenta y Fuente en base a la obra selecionada
          DescObraSinEstructura <- isolate(input$DescObraRegistro)
          DescObraSinEstructura <- str_sub(DescObraSinEstructura, 1, 
                                           (str_length(DescObraSinEstructura) - 18))
          
          data <- FiltrarBD(
            "SELECT Cuenta, Fuente FROM OBRAS WHERE Descripcion = ?",
            params = DescObraSinEstructura
          )
          
          updateSelectizeInput(session, inputId = ('CuentaRegistro'),
                               selected = (data[["Cuenta"]]))
          
          updateSelectizeInput(session, inputId = ('FuenteRegistro'),
                               selected = (data[["Fuente"]]))
           
        } else {
          updateFuenteyCuenta(TRUE)
        }
        
      }
      
    }, ignoreInit = TRUE)

    observeEvent(input$CuentaRegistro, {
      
      req(input$CuentaRegistro)
      Validez <- ValidarCuentaRegistro(DatosFormulario())

      if (Validez$EsValido) {
        updateTextInput(session, inputId = ('DescCuentaRegistro'),
                        value = Validez$Descripcion)
      }

    }, ignoreInit = TRUE)
    
    observeEvent(input$FuenteRegistro, {
      
      req(input$FuenteRegistro)
      Validez <- ValidarFuenteRegistro(DatosFormulario())
      
      if (Validez$EsValido) {
        updateTextInput(session, inputId = ('DescFuenteRegistro'),
                        value = Validez$Descripcion)
      }

    }, ignoreInit = TRUE)
    
    observeEvent(input$MontoBrutoRegistro, {
      
      req(input$MontoBrutoRegistro)
      Validez <- ValidarMontoBrutoRegistro(DatosFormulario())
      
    }, ignoreInit = TRUE)

    observeEvent(input$AvanceRegistro, {

      
      req(input$AvanceRegistro)
      Validez <- ValidarAvanceRegistro(DatosFormulario())

    }, ignoreInit = TRUE)


    observeEvent(input$Aceptar, {

      ListaDeVariables <- list(IDModificacion(), DatosFormulario(), NroComprobante())

      if (invoke(ValidarRegistro, ListaDeVariables) == TRUE){

        #AGREGAR (1), EDITAR (2) y BORRAR (3) estructura programática
        
        AgregarEditarRegistroICARO(DatosOriginales(), DatosFormulario())
        
        strNotificacion = paste0("El ", DatosFormulario()["TipoRegistro"],
                                 " Nro ", DatosFormulario()["ComprobanteRegistro"])
        if (IDModificacion() == 1) {
          strNotificacion = paste0(strNotificacion, " se agregó con éxito")
        } else if (IDModificacion() == 2){
          strNotificacion = paste0(strNotificacion, " se modificó con éxito")
        }
        showNotification(strNotificacion, type = "message")
        
        CleanBsModalRegistro(session)

        toggleModal(session, "bsModalModificar", "close")

      }

    })
    # 
    observeEvent(input$Cancelar, {

      CleanBsModalRegistro(session)
      toggleModal(session, "bsModalModificar", "close")

    })
    
  
  })
  
}