
ReporteModulosBasicosUI <- function(id) {
  
  ns <- NS(id)
  
  tabPanel("Módulos Básicos",
           useShinyjs(),
           sidebarLayout(
             sidebarPanel(id = ns("Sidebar"),
                          tabsetPanel(id = ns("switcher"), type = "hidden",
                            tabPanel("1",
                                     
                                     p(style="text-align: center;",
                                       strong("Filtros del Reporte")),
                                     
                                     fluidRow(
                                       column(12,
                                              dateRangeInput(ns("FechaMB"), "Rango Fecha:", start = NULL, 
                                                             end = NULL, format = "dd-mm-yyyy", 
                                                             startview = "month", language = "es", 
                                                             separator = " a "))
                                     ),
                                     
                                     # fluidRow(
                                     #   column(6,
                                     #          (selectizeInput(ns("EjercicioEjecucionAnual"), "Ejercicio", 
                                     #                          choices = "", selected = "", multiple = T, 
                                     #                          options = list(placeholder = "Todo seleccionado")))),
                                     #   column(6,
                                     #          (selectizeInput(ns("ProgramasEjecucionAnual"), "Programas", 
                                     #                          choices = "", selected = "", multiple = T, 
                                     #                          options = list(placeholder = "Todo seleccionado"))))
                                     # ),
                                     
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
                                       ("Es necesario realizar control cruzado con SIIF"))
                                     
                                     )
                            )
                          
             ),
             mainPanel(id = ns("Main"),
                       
                       fluidRow(
                         column(1, HideSidebarButtonUI(ns("showpanel"))),
                         column(11, 
                                tabsetPanel(id = ns("controller"),
                                            tabPanel("Ejecución Anual", value = "1", 
                                                     DataTableUI(ns("DTModulosBasicos")))
                                            
                                            )
                                )
                       )
                       ) 
           )
           )
  
}

ReporteModulosBasicosServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
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
    CargaIcaroMB  <- reactive(
      FiltrarBD(
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