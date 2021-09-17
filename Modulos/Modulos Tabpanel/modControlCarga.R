
ControlCargaUI <- function(id) {
  
  ns <- NS(id)
  
  tabPanel("Control Carga ICARO",
           useShinyjs(),
           sidebarLayout(
             sidebarPanel(id = ns("Sidebar"),
                          tabsetPanel(id = ns("switcher"), type = "hidden",
                            tabPanel("1",
                                     
                                     p(style="text-align: justify;",
                                       "Control en base al", strong("Reporte SIIF rf602."), 
                                       "Dado que el mencionado reporte expone",
                                       strong("la ejecución acumulada a la fecha"), "del mismo,",
                                       "deberá", strong("importar"), "una versión", 
                                       strong("actualizada"), "del rf602",
                                       "siguiendo las instrucciones."),
                                     
                                     hr(style = "border-top: 1px solid #000000;"),
                                     
                                     FileInputUI(ns("fileControlCargaAnual")),
                                     
                                     hr(style = "border-top: 1px solid #000000;"),
                                     
                                     p(style="text-align: center;", 
                                       strong("Filtros de Control")),
                                     
                                     fluidRow(
                                       column(6,
                                              (selectizeInput(ns("EjercicioControlCargaAnual"), "Ejercicio", 
                                                              choices = "", selected = "", multiple = F, 
                                                              options = list(placeholder = "Elegir un Año")))),
                                       column(6,
                                              (selectizeInput(ns("FuentesControlCargaAnual"), "Fuentes", 
                                                              choices = "", selected = "", multiple = T, 
                                                              options = list(placeholder = "Elegir las fuentes a controlar"))))
                                     ),
                                     
                                     fluidRow(
                                       column(6,
                                              (selectizeInput(ns("PartidasControlCargaAnual"), "Partidas", 
                                                              choices = "", selected = "", multiple = T, 
                                                              options = list(placeholder = "Elegir las partidas a controlar")))),
                                       
                                       column(6, align = "center",
                                              (checkboxInput(ns("DiferenciaControlCargaAnual"), 
                                                             "Mostrar registros con diferencias distintas de 0", value = TRUE)))
                                     ),
                                     
                                     hr(style = "border-top: 1px solid #000000;")
                                     
                                     ),
                            
                            tabPanel("2",
                                     p(style="text-align: center;", 
                                       strong("EN DESARROLLO")),
                                     

                                     
                                     

                                     ),
                            
                            tabPanel("3",
                                     p(style="text-align: center;", 
                                       strong("EN DESARROLLO (rcg01_par)")),
                                     
                                     p(style="text-align: justify;", 
                                     "Solicitar que solucionen el Reporte, 
                                     solo incluye comprobantes pagados. TAMPOCO",
                                     "incluye MAPs (CUIDADO)"),
                                     
                                     hr(style = "border-top: 1px solid #000000;"),
                                     
                                     FileInputUI(ns("fileControlCargaRegistro")),
                                     
                                     hr(style = "border-top: 1px solid #000000;"),
                                     
                                     p(style="text-align: center;", 
                                       strong("Filtros de Control")),
                                     
                                     fluidRow(
                                       column(6,
                                              (selectizeInput(ns("EjercicioControlCargaRegistro"), "Ejercicio", 
                                                              choices = "", selected = "", multiple = F, 
                                                              options = list(placeholder = "Elegir un Año")))),
                                       column(6,
                                              (selectizeInput(ns("FuentesControlCargaRegistro"), "Fuentes", 
                                                              choices = "", selected = "", multiple = T, 
                                                              options = list(placeholder = "Elegir las fuentes a controlar"))))
                                     ),
                                     
                                     fluidRow(
                                       column(6,
                                              (selectizeInput(ns("PartidasControlCargaRegistro"), "Partidas", 
                                                              choices = "", selected = "", multiple = T, 
                                                              options = list(placeholder = "Elegir las partidas a controlar")))),
                                       column(6,
                                              (checkboxInput(ns("DiferenciaControlCargaRegistro"), 
                                                             "Mostrar registros con diferencias distintas de 0", value = TRUE)))
                                     ),
                                     
                                     
                                     hr(style = "border-top: 1px solid #000000;")
                                     
                                     )
                            
                            ),
                          
             ),
             mainPanel(id = ns("Main"),
                       
                       fluidRow(
                         column(1, HideSidebarButtonUI(ns("showpanel"))),
                         column(11, tabsetPanel(id = ns("controller"),
                           tabPanel("Año Completo", value = "1", ReacTableUI(ns("DTControlCargaAnual"))),
                           tabPanel("Mes Específico", value = "2", ReacTableUI(ns("DTControlCargaMes"))),
                           tabPanel("Por Registro Individual", value = "3", ReacTableUI(ns("DTControlCargaRegistro")))
                         ))
                       )
             ) 
           )
  )
  
}

ControlCargaServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    FileInputServer("fileControlCargaAnual", 
                    ArchivoImportar = "SIIFrf602",
                    ContenidoAyuda = paste0(
                      "<ol>",
                      "<li>Ingrese al <strong>SIIF</strong>, y seleccione el menú ",
                      "<strong> Reportes / Generación de Reportes / SUB - SISTEMA DE CONTROL DE GASTOS </strong></li>",
                      "<li><strong>Busque e ingrese</strong> al reporte <strong>rf602</strong> ",
                      "o el codigo <strong>38</strong></li>",
                      "<li>Ingrese el <strong>Ejercicio</strong> para el cual desea obtener el reporte ",
                      "y seleccione el formato a exportar como <strong>XLS</strong></li>",
                      "<li>Presione el botón <strong>Ver Reporte</strong></li>",
                      "<li><strong>Guardar</strong> el archivo generado en una ubicación que recuerde</li>",
                      "<li><strong>Importar</strong> el archivo descargado previamente</li>",
                      "</ol>"),
                    ExtensionArchivo = "xls"
                    )
    
    # FileInputServer("fileControlCargaMensual",
    #                 ArchivoImportar = "RendicionEPAMSGF",
    #                 ContenidoAyuda = paste0(
    #                   "<ol>",
    #                   "<li>Ingrese al <strong>SGF</strong>, y seleccione el menú ",
    #                   "<strong>Informes / Resumen de Rendiciones</strong></li>",
    #                   "<li>Seleccione <strong>Agrupamiento = Por Obra</strong>, <strong>Origen = EPAM</strong> ",
    #                   "y el <strong>rango de fechas</strong> que desee</li>",
    #                   "<li>Presione el botón <strong>Exportar</strong></li>",
    #                   "<li>En la ventana emergente, mantenga la opción <strong>Archivo...</strong> ",
    #                   "antes de presionar aceptar</li>",
    #                   "<li>Elija el destino del archivo a descargar y preste atención a que el tipo sea ",
    #                   "<strong>.csv</strong></li>",
    #                   "<li><strong>Importar</strong> el archivo descargado previamente</li>",
    #                   "</ol>")
    #                 )
    
    FileInputServer("fileControlCargaRegistro", 
                    ArchivoImportar = "SIIFrcg01_par",
                    ContenidoAyuda = paste0(
                      "<ol>",
                      "<li>Ingrese al <strong>SIIF</strong>, y seleccione el menú ",
                      "<strong> Reportes / Generación de Reportes / SUB - SISTEMA DE CONTROL DE GASTOS </strong></li>",
                      "<li><strong>Busque e ingrese</strong> al reporte <strong>rcg01_par</strong> ",
                      "o el codigo <strong>2068</strong></li>",
                      "<li>Ingrese el <strong>Ejercicio</strong>, las ",
                      "<strong>fechas desde y hasta</strong> (preferentemente todo el año) ",
                      "para el cual desea obtener el reporte ",
                      "y seleccione el formato a exportar como <strong>XLS</strong></li>",
                      "<li>Presione el botón <strong>Ver Reporte</strong></li>",
                      "<li><strong>Guardar</strong> el archivo generado en una ubicación que recuerde</li>",
                      "<li><strong>Importar</strong> el archivo descargado previamente</li>",
                      "</ol>"),
                    ExtensionArchivo = "xls"
    )
    
    observeEvent(input$controller, {
      updateTabsetPanel(inputId = "switcher", selected = input$controller)
    })
    
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
    
    #Update Inputs
    
    FuentesControlCarga <- FiltrarBD("SELECT Fuente FROM FUENTES")
    FuentesControlCarga <- FuentesControlCarga[["Fuente"]]
    
    PartidasControlCarga <- FiltrarBD("SELECT Partida FROM PARTIDAS")
    PartidasControlCarga <- PartidasControlCarga[["Partida"]]
    
    EjercicioCargaIcaro <- FiltrarBD("SELECT Fecha From CARGA") %>% 
      mutate(Fecha = zoo::as.Date(Fecha)) %>%
      transmute(Ejercicio = year(Fecha)) %>% 
      unique()
    EjercicioCargaIcaro <- EjercicioCargaIcaro[["Ejercicio"]]
    
    updateSelectizeInput(session, inputId = ('EjercicioControlCargaAnual'), choices = EjercicioCargaIcaro,
                         selected = max(EjercicioCargaIcaro), server = TRUE)
    updateSelectizeInput(session, inputId = ('FuentesControlCargaAnual'), choices = FuentesControlCarga,
                         selected = FuentesControlCarga, server = TRUE)
    updateSelectizeInput(session, inputId = ('PartidasControlCargaAnual'), choices = c("", PartidasControlCarga),
                         selected = c("421", "422"), server = TRUE)
    
    updateSelectizeInput(session, inputId = ('EjercicioControlCargaRegistro'), choices = EjercicioCargaIcaro,
                         selected = max(EjercicioCargaIcaro), server = TRUE)
    updateSelectizeInput(session, inputId = ('FuentesControlCargaRegistro'), choices = FuentesControlCarga,
                         selected = FuentesControlCarga, server = TRUE)
    updateSelectizeInput(session, inputId = ('PartidasControlCargaRegistro'), choices = c("", PartidasControlCarga),
                         selected = c("421", "422"), server = TRUE)
    
    
    #DataTable
    
    #CONTROL ANUAL
    ControlAnualCargaIcaro.df <- reactive(
      CargaIcaro.df() %>% 
        filter(year(Fecha) == input$EjercicioControlCargaAnual,
               Fuente %in% input$FuentesControlCargaAnual,
               Partida %in% input$PartidasControlCargaAnual,
               Tipo != "PA6") %>% 
        select(Imputacion, Partida, Fuente, Importe) %>% 
        group_by(Imputacion, Partida, Fuente) %>% 
        summarise(ICARO = sum(Importe))
      )
    
    SIIFrf602.df <- reactive({
      data <- EjecPresPorFteSIIF.df() %>% 
        filter(Ejercicio == input$EjercicioControlCargaAnual,
               Fuente %in% input$FuentesControlCargaAnual,
               Partida %in% input$PartidasControlCargaAnual) %>% 
        transmute(Imputacion = str_c(Programa, Subprograma, 
                                     Proyecto, Actividad, sep = "-"),
                  Partida = Partida,
                  Fuente = Fuente,
                  SIIF = Ordenado) %>% 
        full_join(ControlAnualCargaIcaro.df(),
                  by = c("Imputacion" = "Imputacion",
                         "Fuente" = "Fuente",
                         "Partida" = "Partida")) %>% 
        mutate(ICARO = ifelse(is.na(ICARO), 0, ICARO),
               SIIF = ifelse(is.na(SIIF), 0, SIIF),
               Diferencia = SIIF - ICARO)
      
      if (input$DiferenciaControlCargaAnual) {
        data <- data %>% 
          filter(!near(Diferencia, 0))
      }
      
      data
      
    })
    
    ControlAnual <- ReacTableServer("DTControlCargaAnual",
                                      SIIFrf602.df,
                                      ListColDef = list(
                                        Diferencia = colDef(format = colFormat(separators = TRUE, digits = 2)),
                                        ICARO = colDef(format = colFormat(separators = TRUE, digits = 2)),
                                        SIIF = colDef(format = colFormat(separators = TRUE, digits = 2))
                                        )
                                      )
    
    
    #CONTROL por REGISTRO
    ControlRegistroCargaIcaro.df <- reactive(
      CargaIcaro.df() %>% 
        mutate(Ejercicio = year(Fecha)) %>% 
        filter(Ejercicio == input$EjercicioControlCargaRegistro,
               Fuente %in% input$FuentesControlCargaRegistro,
               Partida %in% input$PartidasControlCargaRegistro,
               Tipo != "PA6") %>% 
        select(Comprobante_ICARO = Comprobante, Fecha_ICARO = Fecha, 
               Partida_ICARO = Partida, Fuente_ICARO = Fuente, 
               Importe_ICARO = Importe, Cuenta_ICARO = Cuenta, 
               CUIT_ICARO = CUIT)
        # select(Comprobante_ICARO, Fecha_ICARO, 
        #        Partida_ICARO, Fuente_ICARO, 
        #        Importe_ICARO, Cuenta_ICARO, 
        #        CUIT_ICARO)
    )
    
    SIIFrcg01_par.df <- reactive(
      ComprobantesGtosPorPartidaSIIF.df() %>% 
        filter(Ejercicio == input$EjercicioControlCargaRegistro,
               Fuente %in% input$FuentesControlCargaRegistro,
               Partida %in% input$PartidasControlCargaRegistro) %>% 
        mutate(Fecha = Fecha,
                  Comprobante = sprintf("%05d", as.numeric(NroEntrada)),
                  Comprobante = str_c(Comprobante, 
                                      format(Fecha, format="%y"), 
                                      sep="/")
                  ) %>% 
        select(Comprobante_SIIF = Comprobante, Fecha_SIIF = Fecha, 
               Partida_SIIF = Partida, Fuente_SIIF = Fuente, 
               Importe_SIIF = Monto, Cuenta_SIIF = Cuenta, 
               CUIT_SIIF = CUIT) %>% 
        full_join(ControlRegistroCargaIcaro.df(),
                  by = c("Comprobante_SIIF" = "Comprobante_ICARO"),
                  keep = TRUE) %>% 
        mutate(DifComprobante = ifelse((Comprobante_SIIF == Comprobante_ICARO), TRUE, FALSE),
               DifComprobante = ifelse(is.na(DifComprobante), FALSE, DifComprobante),
               DifFecha = ifelse((Fecha_SIIF == Fecha_ICARO), TRUE, FALSE),
               DifFecha = ifelse(is.na(DifFecha), FALSE, DifFecha),
               DifPartida = ifelse((Partida_SIIF == Partida_ICARO), TRUE, FALSE),
               DifPartida = ifelse(is.na(DifPartida), FALSE, DifPartida),
               DifFuente = ifelse((Fuente_SIIF == Fuente_ICARO), TRUE, FALSE),
               DifFuente = ifelse(is.na(DifFuente), FALSE, DifFuente),
               DifImporte = ifelse(near(Importe_SIIF, Importe_ICARO), TRUE, FALSE),
               DifImporte = ifelse(is.na(DifImporte), FALSE, DifImporte),
               DifCuenta = ifelse((Cuenta_SIIF == Cuenta_ICARO), TRUE, FALSE),
               DifCuenta = ifelse(is.na(DifCuenta), FALSE, DifCuenta),
               DifCUIT = ifelse((CUIT_SIIF == CUIT_ICARO), TRUE, FALSE),
               DifCUIT = ifelse(is.na(DifCUIT), FALSE, DifCUIT)) %>% 
        filter((DifComprobante + DifFecha + DifPartida + DifFuente +
                  DifImporte + DifCuenta + DifCUIT) < 7)
      )
    
    ControlRegistro <- ReacTableServer("DTControlCargaRegistro",
                                       SIIFrcg01_par.df,
                                       ListColDef = list(
                                          Comprobante_SIIF = colDef(name = "SIIF"),
                                          Comprobante_ICARO = colDef(name = "ICARO"),
                                          DifComprobante = colDef(name = "Dif",
                                                                  cell = function(value) {

                                                                    if (value == TRUE) "\u2713" else "\u2718"},
                                                                  style = function(value) {
                                                                    color <- if (value == TRUE) "#008000" else "#e00000"
                                                                    list(fontWeight = 600, color = color)
                                                                  }),
                                          Fecha_SIIF = colDef(name = "SIIF"),
                                          Fecha_ICARO = colDef(name = "ICARO"),
                                          DifFecha = colDef(name = "Dif",
                                                            cell = function(value) {

                                                              if (value == TRUE) "\u2713" else "\u2718"},
                                                            style = function(value) {
                                                              color <- if (value == TRUE) "#008000" else "#e00000"
                                                              list(fontWeight = 600, color = color)
                                                            }),
                                          Partida_SIIF = colDef(name = "SIIF"),
                                          Partida_ICARO = colDef(name = "ICARO"),
                                          DifPartida = colDef(name = "Dif",
                                                              cell = function(value) {

                                                                if (value == TRUE) "\u2713" else "\u2718"},
                                                              style = function(value) {
                                                                color <- if (value == TRUE) "#008000" else "#e00000"
                                                                list(fontWeight = 600, color = color)
                                                              }),
                                          Fuente_SIIF = colDef(name = "SIIF"),
                                          Fuente_ICARO = colDef(name = "ICARO"),
                                          DifFuente= colDef(name = "Dif",
                                                            cell = function(value) {

                                                              if (value == TRUE) "\u2713" else "\u2718"},
                                                            style = function(value) {
                                                              color <- if (value == TRUE) "#008000" else "#e00000"
                                                              list(fontWeight = 600, color = color)
                                                            }),
                                          Importe_SIIF = colDef(name = "SIIF"),
                                          Importe_ICARO = colDef(name = "ICARO"),
                                          DifImporte= colDef(name = "Dif",
                                                             cell = function(value) {

                                                               if (value == TRUE) "\u2713" else "\u2718"},
                                                             style = function(value) {
                                                               color <- if (value == TRUE) "#008000" else "#e00000"
                                                               list(fontWeight = 600, color = color)
                                                             }),
                                          Cuenta_SIIF = colDef(name = "SIIF"),
                                          Cuenta_ICARO = colDef(name = "ICARO"),
                                          DifCuenta= colDef(name = "Dif",
                                                            cell = function(value) {

                                                              if (value == TRUE) "\u2713" else "\u2718"},
                                                            style = function(value) {
                                                              color <- if (value == TRUE) "#008000" else "#e00000"
                                                              list(fontWeight = 600, color = color)
                                                            }),
                                          CUIT_SIIF = colDef(name = "SIIF"),
                                          CUIT_ICARO = colDef(name = "ICARO"),
                                          DifCUIT= colDef(name = "Dif",
                                                          cell = function(value) {

                                                            if (value == TRUE) "\u2713" else "\u2718"},
                                                          style = function(value) {
                                                            color <- if (value == TRUE) "#008000" else "#e00000"
                                                            list(fontWeight = 600, color = color)
                                                          })
                                    ),
                                    ListColGroups = list(
                                      colGroup(name = "Comprobante", columns = c("Comprobante_SIIF", "Comprobante_ICARO", "DifComprobante")),
                                      colGroup(name = "Fecha", columns = c("Fecha_SIIF", "Fecha_ICARO", "DifFecha")),
                                      colGroup(name = "Partida", columns = c("Partida_SIIF", "Partida_ICARO", "DifPartida")),
                                      colGroup(name = "Fuente", columns = c("Fuente_SIIF", "Fuente_ICARO", "DifFuente")),
                                      colGroup(name = "Importe", columns = c("Importe_SIIF", "Importe_ICARO", "DifImporte")),
                                      colGroup(name = "Cuenta", columns = c("Cuenta_SIIF", "Cuenta_ICARO", "DifCuenta")),
                                      colGroup(name = "CUIT", columns = c("CUIT_SIIF", "CUIT_ICARO", "DifCUIT"))
                                    )
    )
    
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