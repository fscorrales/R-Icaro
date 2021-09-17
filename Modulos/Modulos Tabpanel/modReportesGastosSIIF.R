
ReportesGastosSIIFUI <- function(id) {
  
  ns <- NS(id)
  
  tabPanel("Subsistema Gastos SIIF",
           useShinyjs(),
           sidebarLayout(
             sidebarPanel(id = ns("Sidebar"),
                          tabsetPanel(id = ns("switcher"), type = "hidden",
                            tabPanel("1",
                                     
                                     p(style="text-align: center;",
                                       strong("Filtros del Reporte")),
                                     
                                     fluidRow(
                                       column(6,
                                              (selectizeInput(ns("EjercicioEjecucionAnual"), "Ejercicio", 
                                                              choices = "", selected = "", multiple = T, 
                                                              options = list(placeholder = "Todo seleccionado")))),
                                       column(6,
                                              (selectizeInput(ns("ProgramasEjecucionAnual"), "Programas", 
                                                              choices = "", selected = "", multiple = T, 
                                                              options = list(placeholder = "Todo seleccionado"))))
                                     ),
                                     
                                     fluidRow(
                                       column(6,
                                              (selectizeInput(ns("FuentesEjecucionAnual"), "Fuentes", 
                                                              choices = "", selected = "", multiple = T, 
                                                              options = list(placeholder = "Todo seleccionado")))),
                                       column(6,
                                              (selectizeInput(ns("GpoPartidasEjecucionAnual"), "Grupo de Partidas", 
                                                              choices = "", selected = "", multiple = T, 
                                                              options = list(placeholder = "Todo seleccionado"))))
                                     ),
                                     
                                     (selectizeInput(ns("PartidasEjecucionAnual"), "Partidas", 
                                                     choices = "", selected = "", multiple = T, 
                                                     options = list(placeholder = "Todo seleccionado"))),
                                  
                                     hr(style = "border-top: 1px solid #000000;"),
                                     
                                     p(style="text-align: center;",
                                       strong("Agrupamiento del Reporte")),
                                     
                                     (selectizeInput(ns("AgruparEjecucionAnual"), "Grupos Filas", 
                                                     choices = "", selected = "", multiple = T, 
                                                     options = list(placeholder = "Sin Agrupamiento. Puede elegir varios grupos"))),
                                     
                                     fluidRow(
                                       column(6,
                                              (selectizeInput(ns("ColumnasEjecucionAnual"), "Determinación de Columnas", 
                                                              choices = "", selected = "", multiple = F, 
                                                              options = list(placeholder = "Originales del reporte")))),
                                       column(6,
                                              (selectInput(ns("CampoSumaEjecucionAnual"), "Suma de", 
                                                              choices = c("CreditoOriginal", "CreditoVigente",
                                                                          "Comprometido", "Ordenado", "Saldo",
                                                                          "Pendiente"), 
                                                              selected = "Ordenado", multiple = F)))
                                     ),
                                     
                                     hr(style = "border-top: 1px solid #000000;"),
                                     
                                     p(style="text-align: center;",
                                       ("Es necesario tener actualizado el rf602")),
                                     
                                     ),
                            
                            tabPanel("2",
                                     p(style="text-align: center;",
                                       strong("EN DESARROLLO")),
                                     
                                     br(),
                                     
                                     hr(style = "border-top: 1px solid #000000;"),
                                     
                                     br(),
                                     # FileInputUI(ns("fileAutocargaObras")),
                                     
                                     hr(style = "border-top: 1px solid #000000;")
                                     
                                     ),
                            
                            tabPanel("3",
                                     
                                     p(style="text-align: center;",
                                       strong("Filtros del Reporte")),
                                     
                                     fluidRow(
                                       column(12,
                                              dateRangeInput(ns("FechaMB"), "Rango Fecha:", start = NULL, 
                                                             end = NULL, format = "dd-mm-yyyy", 
                                                             startview = "month", language = "es", 
                                                             separator = " a "))
                                     ),
                                     
                                     fluidRow(
                                       column(6,
                                              (selectizeInput(ns("ConvenioMB"), "Nro Convenio:", 
                                                              choices = "", selected = "", multiple = T, 
                                                              options = list(placeholder = "Todo seleccionado")))),
                                       column(6,
                                              (selectizeInput(ns("FuenteMB"), "Fuente:", 
                                                              choices = "", selected = "", multiple = T, 
                                                              options = list(placeholder = "Todo seleccionado")))),
                                     ),
                                     
                                     (selectizeInput(ns("LocalidadMB"), "Localidad:", 
                                                     choices = "", selected = "", multiple = T, 
                                                     options = list(placeholder = "Todo seleccionado"))),
                                     
                                     hr(style = "border-top: 1px solid #000000;"),
                                     
                                     p(style="text-align: center;",
                                       strong("Agrupamiento del Reporte")),
                                     
                                     (selectizeInput(ns("AgruparMB"), "Grupos Filas", 
                                                     choices = NULL, selected = NULL, multiple = T, 
                                                     options = list(placeholder = "Sin Agrupamiento. Puede elegir varios grupos"))),
                                     
                                     fluidRow(
                                       column(6,
                                              (selectizeInput(ns("ColumnasMB"), "Determinación de Columnas", 
                                                              choices = "", selected = "", multiple = F, 
                                                              options = list(placeholder = "Originales del reporte")))),
                                       column(6)
                                     ),
                                     
                                     hr(style = "border-top: 1px solid #000000;"),
                                     
                                     p(style="text-align: center;",
                                       ("Es necesario tener actualizado ICARO"))
                                     
                            )
                            
                            )
                          
             ),
             mainPanel(id = ns("Main"),
                       
                       fluidRow(
                         column(1, HideSidebarButtonUI(ns("showpanel"))),
                         column(11, 
                                tabsetPanel(id = ns("controller"),
                                            tabPanel("Ejecución Anual", value = "1", 
                                                     DataTableUI(ns("DTEjecucionAnual"))),
                                            tabPanel("Ejecución Anual Obras + ICARO", value = "2", 
                                                     DataTableUI(ns("DTEjecucionAnualObrasICARO"))),
                                            tabPanel("Módulos Básicos ICARO", value = "3", 
                                                     DataTableUI(ns("DTModulosBasicos")))
                                            
                                            )
                                )
                       )
                       ) 
           )
           )
  
}

ReportesGastosSIIFServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    #Switch Tabs
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
    EjercicioEjecucionAnual <- FiltrarBD("SIIF", 
                                         paste0("SELECT DISTINCT Ejercicio ",
                                                "From EjecPresPorFteSIIF ",
                                                "ORDER BY Ejercicio desc"))
    EjercicioEjecucionAnual <- EjercicioEjecucionAnual[["Ejercicio"]]
    
    ProgramasEjecucionAnual <- FiltrarBD("SIIF", 
                                        paste0("SELECT DISTINCT Programa ",
                                               "From EjecPresPorFteSIIF ",
                                               "ORDER BY Programa asc"))
    ProgramasEjecucionAnual <- ProgramasEjecucionAnual[["Programa"]]
    
    GrupoEjecucionAnual <- FiltrarBD("SIIF", 
                                     paste0("SELECT DISTINCT Grupo ",
                                            "From EjecPresPorFteSIIF ",
                                            "ORDER BY Grupo asc"))
    GrupoEjecucionAnual <- GrupoEjecucionAnual[["Grupo"]]
    
    FuentesEjecucionAnual<- FiltrarBD("SIIF", 
                                      paste0("SELECT DISTINCT Fuente ",
                                             "FROM EjecPresPorFteSIIF ",
                                             "ORDER BY Fuente asc"))
    FuentesEjecucionAnual <- FuentesEjecucionAnual[["Fuente"]]

    PartidasEjecucionAnual <- FiltrarBD("SIIF", 
                                        paste0("SELECT DISTINCT Partida ",
                                               "FROM EjecPresPorFteSIIF ",
                                               "ORDER BY Partida asc"))
    PartidasEjecucionAnual <- PartidasEjecucionAnual[["Partida"]]
    
    AgruparEjecucionAnual <- c("Ejercicio", "Fuente", "Programa", "Subprograma",
                               "Proyecto", "Actividad", "Grupo", "Partida")

    updateSelectizeInput(session, inputId = ('EjercicioEjecucionAnual'), choices = EjercicioEjecucionAnual,
                         selected = max(EjercicioEjecucionAnual), server = TRUE)
    updateSelectizeInput(session, inputId = ('ProgramasEjecucionAnual'), choices = ProgramasEjecucionAnual,
                         selected = NULL, server = TRUE)
    updateSelectizeInput(session, inputId = ('FuentesEjecucionAnual'), choices = FuentesEjecucionAnual,
                         selected = NULL, server = TRUE)
    updateSelectizeInput(session, inputId = ('GpoPartidasEjecucionAnual'), choices = GrupoEjecucionAnual,
                         selected = NULL, server = TRUE)
    updateSelectizeInput(session, inputId = ('PartidasEjecucionAnual'), choices = PartidasEjecucionAnual,
                         selected = NULL, server = TRUE)
    updateSelectizeInput(session, inputId = ('AgruparEjecucionAnual'), choices = AgruparEjecucionAnual,
                         selected = NULL, server = TRUE)
    updateSelectizeInput(session, inputId = ('ColumnasEjecucionAnual'), choices = AgruparEjecucionAnual,
                         selected = "", server = TRUE)
    
    #Filter DT
    FiltroEjercicioEjecAnual <- reactiveVal()
    observeEvent(input$EjercicioEjecucionAnual, {
      
      FiltroEjercicioEjecAnual(EjercicioEjecucionAnual)
      
      req(input$EjercicioEjecucionAnual)
      
      FiltroEjercicioEjecAnual(input$EjercicioEjecucionAnual)
      
    }, ignoreNULL = FALSE)
    
    FiltroProgEjecAnual <- reactiveVal(ProgramasEjecucionAnual)
    observeEvent(input$ProgramasEjecucionAnual, {
      FiltroProgEjecAnual(ProgramasEjecucionAnual)
      
      req(input$ProgramasEjecucionAnual)
      
      FiltroProgEjecAnual(input$ProgramasEjecucionAnual)

    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    FiltroFteEjecAnual <- reactiveVal(FuentesEjecucionAnual)
    observeEvent(input$FuentesEjecucionAnual, {
      FiltroFteEjecAnual(FuentesEjecucionAnual)
      
      req(input$FuentesEjecucionAnual)
      
      FiltroFteEjecAnual(input$FuentesEjecucionAnual)
      
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    FiltroGpoPartEjecAnual <- reactiveVal(GrupoEjecucionAnual)
    observeEvent(input$GpoPartidasEjecucionAnual, {
      FiltroGpoPartEjecAnual(GrupoEjecucionAnual)
      
      req(input$GpoPartidasEjecucionAnual)
      
      FiltroGpoPartEjecAnual(input$GpoPartidasEjecucionAnual)
      
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    FiltroPartEjecAnual <- reactiveVal(PartidasEjecucionAnual)
    observeEvent(input$PartidasEjecucionAnual, {
      FiltroPartEjecAnual(PartidasEjecucionAnual)
      
      req(input$PartidasEjecucionAnual)
      
      FiltroPartEjecAnual(input$PartidasEjecucionAnual)
      
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    GroupByEjecucionAnual <- reactiveVal(NULL)
    observeEvent(input$AgruparEjecucionAnual, {
      GroupByEjecucionAnual(NULL)
      
      updateSelectizeInput(session, inputId = ('ColumnasEjecucionAnual'), 
                           choices = AgruparEjecucionAnual,
                           selected = "", server = TRUE)
      
      req(input$AgruparEjecucionAnual)
      
      GroupByEjecucionAnual(input$AgruparEjecucionAnual)
      
      updateSelectizeInput(session, inputId = ('ColumnasEjecucionAnual'), 
                           choices = input$AgruparEjecucionAnual,
                           selected = "", server = TRUE)
      
      
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    
    #DataTable
    DTEjercicioAnual <- reactiveVal(FiltrarBD("SIIF", "SELECT * From EjecPresPorFteSIIF"))
    # DTEjercicioAnual <- reactiveVal(EjecPresPorFteSIIF.df)
    
    DataTableServer("DTEjecucionAnual", 
                    reactive({
                      DT <- EjecPresPorFteSIIF.df() %>% 
                        filter(Ejercicio %in% FiltroEjercicioEjecAnual(),
                               Programa %in% FiltroProgEjecAnual(),
                               Fuente  %in% FiltroFteEjecAnual(),
                               Grupo %in% FiltroGpoPartEjecAnual(),
                               Partida %in% FiltroPartEjecAnual())
                      
                      if (!is_null(GroupByEjecucionAnual())) {
                        DT <- DT %>% 
                          group_by(!!! rlang::syms(GroupByEjecucionAnual())) %>% 
                          summarise(Total = sum(!! rlang::sym(input$CampoSumaEjecucionAnual)))
                      }
                      
                      if (input$ColumnasEjecucionAnual != "") {
                        if (is_null(GroupByEjecucionAnual())) {
                          DT <- DT %>% 
                            pivot_wider(
                              names_from = !! rlang::sym(input$ColumnasEjecucionAnual),
                              values_from = !! rlang::sym(input$CampoSumaEjecucionAnual), 
                              values_fill = 0
                            ) 
                        } else {
                          DT <- DT %>% 
                            pivot_wider(
                              names_from = !! rlang::sym(input$ColumnasEjecucionAnual),
                              values_from = Total, 
                              values_fill = 0
                            ) 
                        }

                      }
                      
                      DT
                      
                      }), DTServer = FALSE)
    
  ### MODULOS BÁSICOS ICARO
    
    #Update Inputs
    CargaIcaroMB  <- reactive(
      FiltrarBD(
        "ICARO",
        paste0("SELECT PY.DescProy, ",
               "A.DescAct , C.* , ",
               "O.Localidad, O.InformacionAdicional FROM PROGRAMAS P ",
               "INNER JOIN SUBPROGRAMAS SP ON P.Programa = SP.Programa ",
               "INNER JOIN PROYECTOS PY ON SP.Subprograma = PY.Subprograma ",
               "INNER JOIN ACTIVIDADES A ON PY.Proyecto = A.Proyecto ",
               "INNER JOIN CARGA C ON A.Actividad = C.Imputacion ",
               "INNER JOIN OBRAS O ON C.Obra = O.Descripcion ",
               "WHERE P.Programa = '29' AND O.InformacionAdicional <> ''"
        )
      ) %>% 
        mutate(Fecha = zoo::as.Date(Fecha),
               Ejercicio = year(Fecha),
               Estructura = str_c(Imputacion, Partida, sep = "-")) %>% 
        select(Ejercicio, Fecha, Convenio = InformacionAdicional, Fuente,
               DescProy, Localidad, DescAct, Obra, Estructura, Comprobante, 
               Importe, Tipo, Cuenta, CUIT)
    )
    
    observe({
      FechasMB <- (CargaIcaroMB()[["Fecha"]] %>% 
                     unique()) 
      updateDateRangeInput(session, inputId = ('FechaMB'), min = min(FechasMB), 
                           max = max(FechasMB), start = min(FechasMB), end = max(FechasMB))
    })
    
    ConveniosMB <- reactiveVal()
    observe({
      DATA <- (CargaIcaroMB()[["Convenio"]] %>%
                 unique())
      ConveniosMB(DATA)
      updateSelectizeInput(session, inputId = ('ConvenioMB'), choices = ConveniosMB(),
                           selected = NULL, server = TRUE)
    })
    
    FuentesMB <- reactiveVal()
    observe({
      DATA <- (CargaIcaroMB()[["Fuente"]] %>% 
                 unique())
      FuentesMB(DATA)
      updateSelectizeInput(session, inputId = ('FuenteMB'), choices = FuentesMB(),
                           selected = NULL, server = TRUE)
    })  
    
    LocalidadesMB <- reactiveVal()
    observe({
      DATA <- (CargaIcaroMB()[["Localidad"]] %>% 
                 unique())
      LocalidadesMB(DATA)
      updateSelectizeInput(session, inputId = ('LocalidadMB'), choices = LocalidadesMB(),
                           selected = NULL, server = TRUE)
    })
    
    AgruparMB <- c("Ejercicio", "Convenio", "Fuente", "Estructura",
                   "DescProy", "Localidad", "DescAct", "Obra", "CUIT",
                   "Cuenta")
    
    updateSelectizeInput(session, inputId = ('AgruparMB'), choices = AgruparMB,
                         selected = NULL, server = TRUE)
    updateSelectizeInput(session, inputId = ('ColumnasMB'), choices = c("", AgruparMB),
                         selected = NULL, server = TRUE)
    
    observeEvent(input$AgruparMB, {
      updateSelectizeInput(session, inputId = ('ColumnasMB'), choices = c("", AgruparMB),
                           selected = NULL, server = TRUE)
      req(input$AgruparMB)
      updateSelectizeInput(session, inputId = ('ColumnasMB'), choices = c("", input$AgruparMB),
                           selected = NULL, server = TRUE)
    })
    
    
    
    #DataTable
    DataTableServer("DTModulosBasicos", 
                    reactive({
                      
                      if (length(input$ConvenioMB) > 0) {
                        FiltroConvenio <- input$ConvenioMB
                      } else {
                        FiltroConvenio <- ConveniosMB()
                      }
                      if (length(input$FuenteMB) > 0) {
                        FiltroFuente <- input$FuenteMB
                      } else {
                        FiltroFuente <- FuentesMB()
                      }
                      if (length(input$LocalidadMB) > 0) {
                        FiltroLocalidad <- input$LocalidadMB
                      } else {
                        FiltroLocalidad <- LocalidadesMB()
                      }
                      
                      DT <- CargaIcaroMB() %>%
                        filter(between(Fecha, ymd(input$FechaMB[[1]]), ymd(input$FechaMB[[2]])),
                               Convenio %in% FiltroConvenio,
                               Fuente %in% FiltroFuente,
                               Localidad %in% FiltroLocalidad)
                      
                      if (!is_empty(input$AgruparMB)) {
                        DT <- DT %>%
                          group_by(!!! rlang::syms(input$AgruparMB)) %>%
                          summarise(Importe = sum(Importe))
                      }
                      
                      if ((input$ColumnasMB != "")) {
                        DT <- DT %>%
                          pivot_wider(
                            names_from = !! rlang::sym(input$ColumnasMB),
                            values_from = Importe, values_fill = 0
                          )
                      }
                      
                      DT
                      
                    }), DTServer = FALSE)

    
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