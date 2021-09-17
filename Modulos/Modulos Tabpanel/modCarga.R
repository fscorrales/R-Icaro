
CargaUI <- function(id) {
  
  ns <- NS(id)
  
  tabPanel("Carga",
           
           useShinydashboard(),
           
           bsModalRegistroUI(ns("ModalModificar")),
           bsModalDeleteUI(ns("ModalBorrar")),
           
           fluidRow(
             box(title = "Comprantes Gastos SIIF cargados en ICARO",
                              status = "primary", width = 12, collapsible = FALSE,
                              solidHeader = TRUE,
                              ReacTableUI(ns("DTCargaRegistro")),
                 column(12, 
                        column(3, align = "center",
                               (actionButton(ns("AutocargaComprobante"), 
                                                              "Autocarga", width = "125px"))),
                        column(3, align = "center",
                               (actionButton(ns("CargaManualComprobante"), 
                                                              "Carga Manual", width = "125px"))),
                        column(3, align = "center",
                               shinyjs::disabled(actionButton(ns("EditarComprobante"), 
                                                              "Editar", width = "125px"))),
                        column(3, align = "center",
                               shinyjs::disabled(actionButton(ns("BorrarComprobante"), 
                                                              "Borrar", width = "125px")))
                        
                 )
                 )
             
           ),
           
           fluidRow(
             
             box(title = "Imputación",
                 status = "primary", width = 6, collapsible = TRUE,
                 solidHeader = TRUE,
                 ReacTableUI(ns("DTImputacionRegistro")),
                 column(12,
                        column(4, align = "center",
                               shinyjs::disabled(actionButton(ns("CargaImputacion"), 
                                                              "Agregar", width = "125px"))),
                        column(4, align = "center",
                               shinyjs::disabled(actionButton(ns("EditarImputacion"), 
                                                              "Editar", width = "125px"))),
                        column(4, align = "center",
                               shinyjs::disabled(actionButton(ns("BorrarImputacion"), 
                                                              "Borrar", width = "125px"))))
                 ),
             
             box(title = "Rentenciones",
                 status = "primary", width = 6, collapsible = TRUE,
                 solidHeader = TRUE,
                 ReacTableUI(ns("DTRetencionesRegistro")),
                 column(12,
                        column(4, align = "center",
                               shinyjs::disabled(actionButton(ns("CargaRetencion"), 
                                                              "Agregar", width = "125px"))),
                        column(4, align = "center",
                               shinyjs::disabled(actionButton(ns("EditarRetencion"), 
                                                              "Editar", width = "125px"))),
                        column(4, align = "center",
                               shinyjs::disabled(actionButton(ns("BorrarRetencion"), 
                                                              "Borrar", width = "125px")))))
           )

           
  )

}

CargaServer <- function(id, parent_session) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    shinyBS::addPopover(session, ns("CargaManualComprobante"), "PREFERIR SIEMBRE AUTOCARGA", 
                        "SOLO utilizar CARGA MANUAL en caso de extrema necesidad", placement = "top")
    
    #"Bandera" para transportar datos de OBRAS AGREGAR (1), EDITAR (2) y BORRAR (3)
    EditandoCargaRegistro <- reactiveValues(IDModificacion = 0, Origen = "",
                                            NroComprobante = "", Tipo = "", 
                                            NroFila = 0, FondoDeReparo = 0)
    
    CargaRegistro <- ReacTableServer("DTCargaRegistro", 
                                     reactive(CargaIcaro.df() %>%
                                                mutate(Imputacion = str_c(Imputacion, 
                                                                          Partida, sep = "-")) %>% 
                                                # filter(year(Fecha) == 2021) %>% 
                                                select(-Origen, -Partida) %>% 
                                                select(Fecha, Comprobante, Tipo, Obra, Imputacion,
                                                       Fuente, Importe, CUIT, Cuenta, 
                                                       Certificado, Avance, everything())), 
                                     height = 425, defaultPageSize = 10,
                                     pageSizeOptions = c(10, 50, 100, 1000, 10000),
                                     compact = TRUE, defaultSortOrder = "desc",
                                     defaultSorted = c("Fecha", "Comprobante"),
                                     ListColDef = list(Importe = colDef(format = colFormat(prefix = "$", 
                                                                                           separators = TRUE, digits = 2)),
                                                       Avance = colDef(format = colFormat(percent = TRUE, digits = 1)),
                                                       FondoDeReparo = colDef(format = colFormat(prefix = "$", 
                                                                                                 separators = TRUE, digits = 2)),
                                                       Fecha = colDef(width = 100),
                                                       Comprobante = colDef(width = 75),
                                                       Tipo = colDef(width = 50),
                                                       Fuente = colDef(width = 50),
                                                       Certificado = colDef(width = 50),
                                                       Avance = colDef(width = 75),
                                                       Importe = colDef(minWidth = 200),
                                                       Imputacion = colDef(width = 125),
                                                       Obra = colDef(minWidth = 400))
                                     )
    
    observeEvent(CargaRegistro$NroFila(), {
      if (CargaRegistro$NroFila() == 0) {
        EditandoCargaRegistro$NroFila <- NULL
        EditandoCargaRegistro$NroComprobante <- ""
      }
      
      req(CargaRegistro$NroFila() > 0)
      
      EditandoCargaRegistro$NroFila <- CargaRegistro$NroFila()
      data <- CargaIcaro.df()[EditandoCargaRegistro$NroFila, ]
      EditandoCargaRegistro$NroComprobante <- unname(data[["Comprobante"]])
      EditandoCargaRegistro$Origen <- unname(data[["Origen"]])
      EditandoCargaRegistro$Tipo <- unname(data[["Tipo"]])
      EditandoCargaRegistro$FondoDeReparo <- unname(data[["FondoDeReparo"]])
      HabilitarBotonesCargaRegistro()
      
    })
    
    RetencionesRegistro <- ReacTableServer("DTRetencionesRegistro", 
                                           data <- reactive(FiltrarBD(
                                             paste0("SELECT Codigo, Importe FROM RETENCIONES ",
                                                    "WHERE Comprobante = '", EditandoCargaRegistro$NroComprobante, "' ",
                                                    "AND Tipo = '", EditandoCargaRegistro$Tipo, "' ")
                                           )),
                                           height = 250, defaultPageSize = 6,
                                           pageSizeOptions = c(6, 10, 50),
                                           compact = TRUE, filterable = FALSE,
                                           ListColDef = list(Codigo = colDef(footer = "Total"),
                                                             Importe = colDef(footer = function(Importe) sprintf("$%.2f", sum(Importe)))
                                                             ),
                                           defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
                                           )
    
    ImputacionRegistro <- ReacTableServer("DTImputacionRegistro", 
                                          data <- reactive(FiltrarBD(
                                            paste0("SELECT (C.Imputacion || '-' || C.Partida) AS Imputacion, ",
                                                   "(P.DescProy || ' - ' || A.DescAct) AS Descripcion, C.Importe ",
                                                   "FROM CARGA C LEFT JOIN ACTIVIDADES A ON C.Imputacion = A.Actividad ",
                                                   "LEFT JOIN PROYECTOS P ON A.Proyecto = P.Proyecto ",
                                                   "WHERE Comprobante = '", EditandoCargaRegistro$NroComprobante, "' ",
                                                   "AND Tipo = '", EditandoCargaRegistro$Tipo, "' ")
                                          )),
                                           height = 250, defaultPageSize = 6, wrap = TRUE,
                                           pageSizeOptions = c(6, 10, 50),
                                           compact = TRUE, filterable = FALSE,
                                           ListColDef = list(Imputacion = colDef(footer = "Total"),
                                                             Importe = colDef(footer = function(Importe) sprintf("$%.2f", sum(Importe))),
                                                             Importe = colDef(minWidth = 125),
                                                             Imputacion = colDef(width = 125),
                                                             Descripcion = colDef(minWidth = 300)
                                           ),
                                           defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
    )
    

    #Botones

    ##Autocarga
    
    observeEvent(input$AutocargaComprobante, {
      
      updateTabsetPanel(session = parent_session, "ICARO",selected = "Autocarga")
    
      })
    
    AgregarOEditar <- reactiveVal(0)

    ##Agregar

    observeEvent(input$CargaManualComprobante, {

      HabilitarBotonesCargaRegistro(FALSE)

      EditandoCargaRegistro$IDModificacion <- 1

      n <- AgregarOEditar() + 1

      AgregarOEditar(n)

    })

    ##Editar
    observeEvent(input$EditarComprobante, {
      
      HabilitarBotonesCargaRegistro(FALSE)
      
      EditandoCargaRegistro$IDModificacion <- 2
      
      n <- AgregarOEditar() + 1
      
      AgregarOEditar(n)
      
    })
        
    bsModalRegistroServer("ModalModificar",
                          toggleOpen = AgregarOEditar, 
                          DatosOriginales = reactive(EditandoCargaRegistro))
    
    ##Borrar
    observeEvent(input$BorrarComprobante, {

      HabilitarBotonesCargaRegistro(FALSE)

      EditandoCargaRegistro$IDModificacion <- 3

    })

    PermitirBorrar <- reactiveVal(FALSE)

    GlosaBorrar <-  reactive({
      req(EditandoCargaRegistro$NroComprobante)

      ValidarBorrar <- ValidarBorrarRegistro((EditandoCargaRegistro))

      if (ValidarBorrar$EsValido) {
        PermitirBorrar(TRUE)
      } else {
        PermitirBorrar(FALSE)
      }

      Glosa <- ValidarBorrar$Descripcion

    })

    BorrarComprobante <- bsModalDeleteServer("ModalBorrar",
                                      reactive(input$BorrarComprobante),
                                      (GlosaBorrar),
                                      (PermitirBorrar))

    observeEvent(BorrarComprobante$Borrar(), {

      req(BorrarComprobante$Borrar() > 0)

      EliminarComprobanteICARO((EditandoCargaRegistro))

    })

  })
}

# ui <- fluidPage(
#   ProveedoresUI(id = "Prueba")
# )
# 
# server <- function(input, output) {
#   ProveedoresServer(id = "Prueba")
# }
# 
# shinyApp(ui, server)