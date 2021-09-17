
AutocargaUI <- function(id) {
  
  ns <- NS(id)
  
  tabPanel("Autocarga",
           useShinyjs(),
           sidebarLayout(
             sidebarPanel(id = ns("Sidebar"),
                          tabsetPanel(id = ns("switcher"), type = "hidden",
                            tabPanel("1",
                                     
                                     p(style="text-align: justify;",
                                       "En caso que desee", strong("actualizar"), "la", 
                                       strong("Base de Datos de Autocarga Obras de ICARO"),
                                       ", deberá proceder a", strong("importar"),
                                       "desde el", strong("Sistema de Gestión Financiera (SGF)")),
                                     
                                     hr(style = "border-top: 1px solid #000000;"),
                                     
                                     FileInputUI(ns("fileAutocargaObras")),
                                     
                                     hr(style = "border-top: 1px solid #000000;"),
                                     
                                     disabled(textInput(ns("EstructuraAutocargaObras"), "Estructura", value = "")),
                                     
                                     fluidRow(
                                       column(6,
                                              disabled(textInput(ns("ImporteBrutoAutocargaObras"), "Importe Bruto", value = ""))),
                                       column(6,
                                              disabled(textInput(ns("ImporteNetoAutocargaObras"), "Importe Neto", value = "")))
                                     ),
                                
                                     
                                     hr(style = "border-top: 1px solid #000000;"),
                                     
                                     fluidRow(
                                       column(4, align='center',
                                              shinyjs::disabled(actionButton(ns("AgregarAutocargaObras"), 
                                                                             "Agregar", width = "100px"))),
                                       column(4, align='center',
                                              shinyjs::disabled(actionButton(ns("EditarAutocargaObras"), 
                                                                             "Editar", width = "100px"))),
                                       column(4, align='center',
                                              shinyjs::disabled(actionButton(ns("BorrarAutocargaObras"), 
                                                                             "Borrar", width = "100px")))
                                     )
                                     ),
                            
                            tabPanel("2",
                                     p(style="text-align: justify;",
                                       "En caso que desee", strong("actualizar"), "la", 
                                       strong("Base de Datos de Autocarga EPAM de ICARO"),
                                       ", deberá proceder a", strong("importar"),
                                       "desde el", strong("Sistema de Gestión Financiera (SGF)")),
                                     
                                     hr(style = "border-top: 1px solid #000000;"),
                                     
                                     FileInputUI(ns("fileAutocargaEPAM")),
                                     
                                     hr(style = "border-top: 1px solid #000000;"),
                                     
                                     disabled(textInput(ns("EstructuraAutocargaEPAM"), "Estructura", value = "")),
                                     
                                     fluidRow(
                                       column(6,
                                              disabled(textInput(ns("ImporteBrutoAutocargaEPAM"), "Importe Bruto", value = ""))),
                                       column(6,
                                              disabled(textInput(ns("ImporteNetoAutocargaEPAM"), "Importe Neto", value = "")))
                                     ),
                                     
                                     hr(style = "border-top: 1px solid #000000;"),
                                     
                                     fluidRow(
                                       column(4, align='center',
                                              shinyjs::disabled(actionButton(ns("AgregarAutocargaEPAM"), 
                                                                             "Agregar", width = "100px"))),
                                       column(4, align='center',
                                              shinyjs::disabled(actionButton(ns("EditarAutocargaEPAM"), 
                                                                             "Editar", width = "100px"))),
                                       column(4, align='center',
                                              shinyjs::disabled(actionButton(ns("BorrarAutocargaEPAM"), 
                                                                             "Borrar", width = "100px")))
                                     )

                                     )
                            ),
                          
                          bsModalRegistroUI(ns("ModalAgregarRegistroAutocarga")),
                          bsModalObraUI(ns("ModalAgregarObraAutocarga")),
                          bsModalAutocargaUI(ns("ModalEditarRegistroAutocarga")),
                          bsModalDeleteUI(ns("ModalBorrar")),
                          
             ),
             mainPanel(id = ns("Main"),
                       
                       fluidRow(
                         column(1, HideSidebarButtonUI(ns("showpanel"))),
                         column(11, tabsetPanel(id = ns("controller"),
                           tabPanel("Autocarga OBRAS", value = "1", ReacTableUI(ns("DTAutocargaObrasRegistro"))),
                           tabPanel("Autocarga EPAM", value = "2", ReacTableUI(ns("DTAutocargaEPAMRegistro")))
                         ))
                       )
             ) 
           )
  )
  
}

AutocargaServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    FileInputServer("fileAutocargaObras", 
                    ArchivoImportar = "CertificadosObrasSGF",
                    ContenidoAyuda = paste0(
                      "<ol>",
                      "<li>Ingrese al <strong>SGF</strong>, y seleccione el menú ",
                      "<strong>Informes / Certificados de Obra / Informe para Contable [Certificados]</strong></li>",
                      "<li>Seleccione <strong>Mes</strong> y <strong>Año</strong> ",
                      "<li>Presione el botón <strong>Exportar</strong></li>",
                      "<li>En la ventana emergente, mantenga la opción <strong>Archivo...</strong> ",
                      "antes de presionar aceptar</li>",
                      "<li>Elija el destino del archivo a descargar y preste atención a que el tipo sea ",
                      "<strong>.csv</strong></li>",
                      "<li><strong>Importar</strong> el archivo descargado previamente</li>",
                      "</ol>")
                    )
    
    FileInputServer("fileAutocargaEPAM",
                    ArchivoImportar = "RendicionEPAMSGF",
                    ContenidoAyuda = paste0(
                      "<ol>",
                      "<li>Ingrese al <strong>SGF</strong>, y seleccione el menú ",
                      "<strong>Informes / Resumen de Rendiciones</strong></li>",
                      "<li>Seleccione <strong>Agrupamiento = Por Obra</strong>, <strong>Origen = EPAM</strong> ",
                      "y el <strong>rango de fechas</strong> que desee</li>",
                      "<li>Presione el botón <strong>Exportar</strong></li>",
                      "<li>En la ventana emergente, mantenga la opción <strong>Archivo...</strong> ",
                      "antes de presionar aceptar</li>",
                      "<li>Elija el destino del archivo a descargar y preste atención a que el tipo sea ",
                      "<strong>.csv</strong></li>",
                      "<li><strong>Importar</strong> el archivo descargado previamente</li>",
                      "</ol>")
                    )
    
    observeEvent(input$controller, {
      updateTabsetPanel(inputId = "switcher", selected = input$controller)
    })
    
    # #"Bandera" para transportar datos de OBRAS AGREGAR (1), EDITAR (2) y BORRAR (3)
    # EditandoRegistroINVICO <- reactiveValues(IDModificacion = 0, Estructura = "",
    #                                          Descripcion = "", NroFila = 0)
    
    AutocargaINVICO <- reactiveValues(IDModificacion = 1, Origen = "", NroComprobante = "",
                                      Tipo = "", NroCertificado = "", DescObra = "", 
                                      CUIT = "", DescCUIT = "", FomdoDeReparo = 0, 
                                      ImporteBruto = 0, TotalRetenido = 0, ImporteNeto = 0,
                                      Gcias = 0, Sellos = 0, TL = 0, IIBB = 0,
                                      SUSS = 0, INVICO = 0, Estructura = "", NroFila = 0)
    
    #Expandable Sidebar
    toggleSidebar <- reactive(HideSidebarButtonServer("showpanel"))
    
    observeEvent(toggleSidebar(), {
      
      if(toggleSidebar() == TRUE) {
        removeCssClass("Main", "col-sm-12")
        addCssClass("Main", "col-sm-8")
        shinyjs::show(id = session$ns("Sidebar"), asis = TRUE)
      } else {
        removeCssClass("Main", "col-sm-8")
        addCssClass("Main", "col-sm-12")
        shinyjs::hide(id = session$ns("Sidebar"), asis = TRUE)
      }
      
    })
    
    #DataTable
    
    AutocargaObras <- ReacTableServer("DTAutocargaObrasRegistro",
                                      reactive(CertificadosIcaroSGF.df() %>%
                                                 select(Periodo, NroCertificado, Beneficiario,
                                                        Obra, ImporteBruto, FondoDeReparo,
                                                        everything()) %>%
                                                 filter((NroComprobanteSIIF == "")) %>%
                                                 select(-Origen, -NroComprobanteSIIF,
                                                        -TipoComprobanteSIIF)),
                                      ListColDef = list(Periodo = colDef(width = 75),
                                                        NroCertificado = colDef(width = 75),
                                                        Beneficiario = colDef(minWidth = 200),
                                                        Obra = colDef(minWidth = 400)))

    observeEvent(AutocargaObras$NroFila(), {
      if(AutocargaObras$NroFila() == 0) {
        HabilitarBotonesAutocargaObras(FALSE)
        CleanAutocargaObras(session)
        shinyFeedback::feedback("EstructuraAutocargaObras", show = FALSE)
      } else {
        AutocargaINVICO$NroFila <- AutocargaObras$NroFila()
        # data <- ObrasIcaroSGF.df()[AutocargaINVICO$NroFila, ]
        data <- FiltrarBD(
          paste0("SELECT * FROM ",
                 "(SELECT ROW_NUMBER() OVER () NroFila, * ", 
                 "FROM CERTIFICADOS WHERE NroComprobanteSIIF = '') ",
                 "WHERE NroFila = ?"),
          params = AutocargaINVICO$NroFila
        )
        AutocargaINVICO$IDModificacion <- 1
        AutocargaINVICO$Origen <- (data[["Origen"]])
        AutocargaINVICO$DescCUIT <- (data[["Beneficiario"]])
        AutocargaINVICO$DescObra <- (data[["Obra"]])
        AutocargaINVICO$NroCertificado <- (data[["NroCertificado"]])
        AutocargaINVICO$FondoDeReparo <- (data[["FondoDeReparo"]])
        AutocargaINVICO$ImporteBruto <- (data[["ImporteBruto"]])
        AutocargaINVICO$IIBB <- (data[["IIBB"]])
        AutocargaINVICO$TL <- (data[["LP"]])
        AutocargaINVICO$SUSS <- (data[["SUSS"]])
        AutocargaINVICO$Gcias <- (data[["Gcias"]])
        AutocargaINVICO$INVICO <- (data[["INVICO"]])
        AutocargaINVICO$Sellos <- 0
        AutocargaINVICO$ImporteNeto <- (data[["ImporteNeto"]])
        data <- data %>%
            mutate(TotalRetenido = IIBB + LP + SUSS + Gcias + INVICO)
        AutocargaINVICO$TotalRetenido <- (data[["TotalRetenido"]])

        Validez <- ValidarDescCUITAutocarga(AutocargaINVICO$DescCUIT)
        if (Validez$EsValido) {
          HabilitarBotonesAutocargaObras()
          AutocargaINVICO$CUIT <- Validez$Descripcion
          Validez <- ValidarDescObraAutocarga(AutocargaINVICO$DescObra)
          if (Validez$EsValido) {
            AutocargaINVICO$Estructura <- Validez$Descripcion
            updateTextInput(session, "EstructuraAutocargaObras",
                            value = Validez$Descripcion)
          } else {
            updateTextInput(session, "EstructuraAutocargaObras",
                            value = "NO existe la OBRA seleccionada en ICARO")
          }
        } else {
          HabilitarBotonesAutocargaObras(FALSE)
          AutocargaINVICO$CUIT <- ""
          shinyjs::enable("EditarAutocargaObras")
          updateTextInput(session, "EstructuraAutocargaObras",
                          value = "NO existe la CUIT seleccionado en ICARO")
        }

        updateTextInput(session, "ImporteBrutoAutocargaObras",
                        value = AutocargaINVICO$ImporteBruto)
        # ImporteNeto <- data %>%
        #   transmute(ImporteNeto = ImporteBruto -
        #               IIBB - LP - SUSS - Gcias - INVICO)
        updateTextInput(session, "ImporteNetoAutocargaObras",
                        value = AutocargaINVICO$ImporteNeto)

      }

    })

    AutocargaEPAM <- ReacTableServer("DTAutocargaEPAMRegistro",
                                     reactive(EPAMIcaroSGF.df() %>%
                                                select(Periodo, LibramientoSGF, Beneficiario,
                                                       Obra, ImporteBruto, everything()) %>%
                                                filter(NroComprobanteSIIF == "") %>%
                                                select(-Origen, -NroComprobanteSIIF,
                                                       -TipoComprobanteSIIF)),
                                     selection = "multiple",
                                     ListColDef = list(Periodo = colDef(width = 75),
                                                       LibramientoSGF = colDef(width = 75),
                                                       Beneficiario = colDef(minWidth = 200),
                                                       Obra = colDef(minWidth = 400)
                                                       # ImporteBruto = colDef(
                                                       #   html = TRUE,
                                                       #   footer = JS("function(colInfo) {
                                                       #   var total = 0
                                                       #   colInfo.data.forEach(function(row) {
                                                       #   total += row[colInfo.column.id]
                                                       #   })
                                                       #   return '$' + total.toFixed(2)
                                                       #               }")
                                                       #   ),
                                                       # Periodo = colDef(html = TRUE,
                                                       #                  footer = "<b>Total</b>")
                                                       )
                                     )

    observeEvent(AutocargaEPAM$NroFila(), {
      if(AutocargaEPAM$NroFila()[1] == 0) {
        HabilitarBotonesAutocargaEPAM(FALSE)
        CleanAutocargaEPAM(session)
        shinyFeedback::feedback("EstructuraAutocargaEPAM", show = FALSE)
      } else {
        AutocargaINVICO$NroFila <- AutocargaEPAM$NroFila()
        data <- FiltrarBD(
          paste0("SELECT * FROM ",
                 "(SELECT ROW_NUMBER() OVER () NroFila, * ", 
                 "FROM EPAM WHERE NroComprobanteSIIF = '') ",
                 "WHERE NroFila = ?"),
          params = list(AutocargaINVICO$NroFila)
        )
        # data <- EPAMIcaroSGF.df()[AutocargaINVICO$NroFila, ] %>%
        dataObra <- data %>% 
          group_by(Obra) %>%
          summarise(Gcias = sum(Gcias),
                    Sellos = sum(Sellos),
                    TL = sum(TL),
                    IIBB = sum(IIBB),
                    SUSS = sum(SUSS),
                    ImporteBruto = sum(ImporteBruto),
                    ImporteNeto = sum(ImporteNeto))
        if (nrow(dataObra) > 1) {
          HabilitarBotonesAutocargaEPAM(FALSE)
          CleanAutocargaEPAM(session)
          updateTextInput(session, "EstructuraAutocargaEPAM",
                          value = "Más de una obra / imputación seleccionado")
          shinyFeedback::feedback("EstructuraAutocargaEPAM", show = FALSE)
          shinyFeedback::feedbackDanger("EstructuraAutocargaEPAM", TRUE,
          "Solo puede seleccionar múltiples filas que pertenezcan a la
          misma obra")

        } else {
          HabilitarBotonesAutocargaEPAM()
          if (length(AutocargaINVICO$NroFila) > 1) {
            shinyjs::disable("EditarAutocargaEPAM")
          }
          shinyFeedback::feedback("EstructuraAutocargaEPAM", show = FALSE)
          AutocargaINVICO$IDModificacion <- 1
          AutocargaINVICO$Origen <- "EPAM"
          AutocargaINVICO$DescObra <- (dataObra[["Obra"]])
          AutocargaINVICO$NroCertificado <- ""
          AutocargaINVICO$FondoDeReparo <- 0
          AutocargaINVICO$ImporteBruto <- (dataObra[["ImporteBruto"]])
          AutocargaINVICO$IIBB <- (dataObra[["IIBB"]])
          AutocargaINVICO$Sellos <- (dataObra[["Sellos"]])
          AutocargaINVICO$TL <- (dataObra[["TL"]])
          AutocargaINVICO$SUSS <- (dataObra[["SUSS"]])
          AutocargaINVICO$Gcias <- (dataObra[["Gcias"]])
          AutocargaINVICO$INVICO <- 0
          AutocargaINVICO$ImporteNeto <- (dataObra[["ImporteNeto"]])
          dataObra <- dataObra %>%
            mutate(TotalRetenido = IIBB + TL + SUSS + Gcias + Sellos)
          AutocargaINVICO$TotalRetenido <- (dataObra[["TotalRetenido"]])
          dataBeneficiario <- data %>%
            group_by(Beneficiario) %>%
            summarise(ImporteBruto = sum(ImporteBruto))
          if (nrow(dataBeneficiario) > 1) {
            AutocargaINVICO$CUIT <- "30632351514"
            AutocargaINVICO$DescCUIT <- ""
          } else if (nrow(dataBeneficiario) == 1) {
            # dataBeneficiario <- FiltrarBD(
            #   paste0("SELECT P.Descripcion FROM OBRAS O INNER JOIN PROVEEDORES P ",
            #          "ON O.CUIT = P.CUIT WHERE O.Descripcion = ?"),
            #   params = AutocargaINVICO$DescObra
            # )
            # AutocargaINVICO$DescCUIT <- (dataBeneficiario[["Descripcion"]])
            AutocargaINVICO$DescCUIT <- dataBeneficiario[["Beneficiario"]]
            Validez <- ValidarDescCUITAutocarga(AutocargaINVICO$DescCUIT,
                                                AutocargaINVICO$Origen)
            if (Validez$EsValido) {
              AutocargaINVICO$CUIT <- Validez$Descripcion
            } else {
              AutocargaINVICO$CUIT <- ""
            }
          }
          Validez <- ValidarDescObraAutocarga(AutocargaINVICO$DescObra,
                                              AutocargaINVICO$Origen)
          if (Validez$EsValido) {
            AutocargaINVICO$Estructura <- Validez$Descripcion
            updateTextInput(session, "EstructuraAutocargaEPAM",
                            value = Validez$Descripcion)
          } else {
            updateTextInput(session, "EstructuraAutocargaEPAM",
                            value = "NO existe la OBRA seleccionada en ICARO")
          }
          updateTextInput(session, "ImporteBrutoAutocargaEPAM",
                          value = AutocargaINVICO$ImporteBruto)
          updateTextInput(session, "ImporteNetoAutocargaEPAM",
                          value = AutocargaINVICO$ImporteNeto)

        }

      }

    })

    #Botones

    AgregarRegistro <- reactiveVal(0)
    AgregarObra <- reactiveVal(0)

    ##Agregar

    observeEvent(input$AgregarAutocargaObras, {

      HabilitarBotonesAutocargaObras(FALSE)

      TextoEstructura <- str_sub(input$EstructuraAutocargaObras, 1, 2)

      if (TextoEstructura == "NO") {

        n <- AgregarObra() + 1
        AgregarObra(n)

      } else {

        n <- AgregarRegistro() + 1
        AgregarRegistro(n)
        
        dataBeneficiario <- FiltrarBD(
          paste0("SELECT P.CUIT, P.Descripcion FROM OBRAS O INNER JOIN PROVEEDORES P ",
                 "ON O.CUIT = P.CUIT WHERE O.Descripcion = ?"),
          params = AutocargaINVICO$DescObra
        )
        AutocargaINVICO$DescCUIT <- (dataBeneficiario[["Descripcion"]])
        AutocargaINVICO$CUIT <- (dataBeneficiario[["CUIT"]])

      }

    })

    observeEvent(input$AgregarAutocargaEPAM, {

      HabilitarBotonesAutocargaEPAM(FALSE)

      TextoEstructura <- str_sub(input$EstructuraAutocargaEPAM, 1, 2)

      if (TextoEstructura == "NO") {

        n <- AgregarObra() + 1
        AgregarObra(n)

      } else {

        n <- AgregarRegistro() + 1
        AgregarRegistro(n)
        
        # dataBeneficiario <- FiltrarBD(
        #   paste0("SELECT P.CUIT, P.Descripcion FROM OBRAS O INNER JOIN PROVEEDORES P ",
        #          "ON O.CUIT = P.CUIT WHERE O.Descripcion = ?"),
        #   params = AutocargaINVICO$DescObra
        # )
        
        # Cuando arregle CUIT puedo usar esto (Puede causar errores)
        dataBeneficiario <- FiltrarBD(
          paste0("SELECT DISTINCT P.CUIT, P.Descripcion FROM CARGA C INNER JOIN PROVEEDORES P ",
                 "ON C.CUIT = P.CUIT WHERE C.Obra = ? AND C.CUIT = ?"),
          params = list(AutocargaINVICO$DescObra, AutocargaINVICO$CUIT)
        )
        if(nrow(dataBeneficiario) > 0){
            AutocargaINVICO$CUIT <- (dataBeneficiario[["CUIT"]])
            AutocargaINVICO$DescCUIT <- (dataBeneficiario[["Descripcion"]])
        } else {
          dataBeneficiario <- FiltrarBD(
            paste0("SELECT P.CUIT, P.Descripcion FROM OBRAS O INNER JOIN PROVEEDORES P ",
                   "ON O.CUIT = P.CUIT WHERE O.Descripcion = ?"),
            params = AutocargaINVICO$DescObra
          )
          AutocargaINVICO$DescCUIT <- (dataBeneficiario[["Descripcion"]])
          AutocargaINVICO$CUIT <- (dataBeneficiario[["CUIT"]])
        }

      }

    })

    bsModalObraServer("ModalAgregarObraAutocarga",
                      toggleOpen = AgregarObra,
                      DatosOriginales = reactive(AutocargaINVICO))

    bsModalRegistroServer("ModalAgregarRegistroAutocarga",
                          toggleOpen = AgregarRegistro,
                          DatosOriginales = reactive(AutocargaINVICO))


    ##Editar
    EditarRegistro <- reactiveVal(0)

    observeEvent(input$EditarAutocargaObras, {

      HabilitarBotonesAutocargaObras(FALSE)

      n <- EditarRegistro() + 1

      EditarRegistro(n)

    })

    observeEvent(input$EditarAutocargaEPAM, {

      HabilitarBotonesAutocargaEPAM(FALSE)

      n <- EditarRegistro() + 1

      EditarRegistro(n)

    })

    bsModalAutocargaServer("ModalEditarRegistroAutocarga",
                          toggleOpen = EditarRegistro,
                          Datos = reactive(AutocargaINVICO))

    # 
    # ##Borrar
    # observeEvent(input$BorrarEstructura, {
    #   
    #   HabilitarBotonesEstructura(FALSE)
    #   
    #   EditandoEstructuraINVICO$IDModificacion <- 3
    #   
    # })
    # 
    # PermitirBorrar <- reactiveVal(FALSE)
    # 
    # GlosaBorrar <-  reactive({
    #   req(EditandoEstructuraINVICO$Estructura)
    #   
    #   ValidarBorrar <- ValidarBorrarEstructura(EditandoEstructuraINVICO$Estructura)
    #   
    #   if (ValidarBorrar$EsValido) {
    #     PermitirBorrar(TRUE)
    #   } else {
    #     PermitirBorrar(FALSE)
    #   }
    # 
    #   Glosa <- ValidarBorrar$Descripcion
    #   
    # }) 
    # 
    # BorrarEstructura <- bsModalDeleteServer("ModalBorrar", 
    #                                         reactive(input$BorrarEstructura),
    #                                         (GlosaBorrar),
    #                                         (PermitirBorrar))
    # 
    # observeEvent(BorrarEstructura$Borrar(), {
    #   
    #   req(BorrarEstructura$Borrar() == TRUE)
    #   
    #   EliminarEstructuraICARO(EditandoEstructuraINVICO$Estructura,
    #                           EditandoEstructuraINVICO$NroFila)
    #   
    # })
    
  })
}

# ui <- fluidPage(
#   EstructuraUI(id = "Prueba")
# )
# 
# server <- function(input, output) {
#   EstructuraServer(id = "Prueba")
# }
# 
# shinyApp(ui, server)