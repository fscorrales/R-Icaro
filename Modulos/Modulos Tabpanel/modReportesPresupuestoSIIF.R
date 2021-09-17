
ReportesPresupuestoSIIFUI <- function(id) {
  
  ns <- NS(id)
  
  tabPanel("Subsistema Presupuesto SIIF",
           useShinyjs(),
           sidebarLayout(
             sidebarPanel(id = ns("Sidebar"),
                          tabsetPanel(id = ns("switcher"), type = "hidden",
                            tabPanel("1",
                                     
                                     p(style="text-align: center;",
                                       strong("Filtros del Reporte")),
                                     
                                     fluidRow(
                                       column(6,
                                              (selectizeInput(ns("EjercicioDatosF13"), "Ejercicio BASE:", 
                                                              choices = "", selected = "", multiple = F, 
                                                              options = list(placeholder = "Todo seleccionado")))),
                                       column(6,
                                              (selectizeInput(ns("ProgramasDatosF13"), "Programas", 
                                                              choices = "", selected = "", multiple = T, 
                                                              options = list(placeholder = "Todo seleccionado"))))
                                     ),
                                     
                                     fluidRow(
                                       column(6,
                                              (selectizeInput(ns("FuentesDatosF13"), "Fuentes", 
                                                              choices = "", selected = "", multiple = T, 
                                                              options = list(placeholder = "Todo seleccionado"))))
                                     ),
                                     
                                     (selectizeInput(ns("PartidasDatosF13"), "Partidas", 
                                                     choices = "", selected = "", multiple = T, 
                                                     options = list(placeholder = "Todo seleccionado"))),
                                  
                                     hr(style = "border-top: 1px solid #000000;"),

                                     p(style="text-align: center;",
                                       strong("Agrupamiento del Reporte")),

                                     (selectizeInput(ns("AgruparDatosF13"), "Grupos Filas",
                                                     choices = c("DescProy", "DescAct","Obra", 
                                                                 "Localidad", "NormaLegal"), 
                                                     selected = "", multiple = T,
                                                     options = list(placeholder = "Agrupado por Estructura Programatica (00-00-00-00-000)"))),

                                     hr(style = "border-top: 1px solid #000000;"),
                                     
                                     p(style="text-align: center;",
                                       ("Es necesario tener actualizado ICARO, 
                                        incluyendo PA6")),
                                     
                                     )
                            
                            # tabPanel("2",
                            #          p(style="text-align: center;",
                            #            strong("EN DESARROLLO")),
                            #          
                            #          br(),
                            #          
                            #          hr(style = "border-top: 1px solid #000000;"),
                            #          
                            #          br(),
                            #          # FileInputUI(ns("fileAutocargaObras")),
                            #          
                            #          hr(style = "border-top: 1px solid #000000;")
                            #          
                            #          ),
                            # 
                            # tabPanel("3",
                            #          
                            #          p(style="text-align: center;",
                            #            strong("Filtros del Reporte")),
                            #          
                            #          fluidRow(
                            #            column(12,
                            #                   dateRangeInput(ns("FechaMB"), "Rango Fecha:", start = NULL, 
                            #                                  end = NULL, format = "dd-mm-yyyy", 
                            #                                  startview = "month", language = "es", 
                            #                                  separator = " a "))
                            #          ),
                            #          
                            #          fluidRow(
                            #            column(6,
                            #                   (selectizeInput(ns("ConvenioMB"), "Nro Convenio:", 
                            #                                   choices = "", selected = "", multiple = T, 
                            #                                   options = list(placeholder = "Todo seleccionado")))),
                            #            column(6,
                            #                   (selectizeInput(ns("FuenteMB"), "Fuente:", 
                            #                                   choices = "", selected = "", multiple = T, 
                            #                                   options = list(placeholder = "Todo seleccionado")))),
                            #          ),
                            #          
                            #          (selectizeInput(ns("LocalidadMB"), "Localidad:", 
                            #                          choices = "", selected = "", multiple = T, 
                            #                          options = list(placeholder = "Todo seleccionado"))),
                            #          
                            #          hr(style = "border-top: 1px solid #000000;"),
                            #          
                            #          p(style="text-align: center;",
                            #            strong("Agrupamiento del Reporte")),
                            #          
                            #          (selectizeInput(ns("AgruparMB"), "Grupos Filas", 
                            #                          choices = NULL, selected = NULL, multiple = T, 
                            #                          options = list(placeholder = "Sin Agrupamiento. Puede elegir varios grupos"))),
                            #          
                            #          fluidRow(
                            #            column(6,
                            #                   (selectizeInput(ns("ColumnasMB"), "Determinación de Columnas", 
                            #                                   choices = "", selected = "", multiple = F, 
                            #                                   options = list(placeholder = "Originales del reporte")))),
                            #            column(6)
                            #          ),
                            #          
                            #          hr(style = "border-top: 1px solid #000000;"),
                            #          
                            #          p(style="text-align: center;",
                            #            ("Es necesario tener actualizado ICARO"))
                            #          
                            # )
                            
                            )
                          
             ),
             mainPanel(id = ns("Main"),
                       
                       fluidRow(
                         column(1, HideSidebarButtonUI(ns("showpanel"))),
                         column(11, 
                                tabsetPanel(id = ns("controller"),
                                            tabPanel("Datos F13", value = "1", 
                                                     DataTableUI(ns("DTDatos F13")))
                                            
                                            )
                                )
                       )
                       ) 
           )
           )
  
}

ReportesPresupuestoSIIFServer <- function(id) {
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
    observe({
      EjercicioF13 <- FiltrarBD(
        "ICARO",
        "Select Fecha FROM CARGA"
        ) %>% 
        mutate(
          Fecha = zoo::as.Date(Fecha),
          Ejercicio = year(Fecha)
        )
      EjercicioF13 <- EjercicioF13[["Ejercicio"]] %>% 
        unique() %>% 
        sort(decreasing = TRUE)
      updateSelectizeInput(session, inputId = ('EjercicioDatosF13'), 
                           choices = EjercicioF13,
                           selected = max(EjercicioF13), server = TRUE)
    })

    ProgramasF13 <- reactiveVal()
    observe({
      DATA  <- FiltrarBD(
        "ICARO",
        "Select Imputacion FROM CARGA"
      ) %>% 
        mutate(Programa = str_sub(Imputacion, 1, 2))
      DATA <- DATA[["Programa"]] %>% 
        unique() %>%
        sort()
      ProgramasF13(DATA)
      updateSelectizeInput(session, inputId = ('ProgramasDatosF13'), 
                           choices = ProgramasF13(),
                           selected = "", server = TRUE)
    })
    
    FuentesF13 <- reactiveVal()
    observe({
      DATA  <- FiltrarBD(
        "ICARO",
        "Select Fuente FROM CARGA"
      )
      DATA <- DATA[["Fuente"]] %>% 
        unique() %>%
        sort()
      FuentesF13(DATA)
      updateSelectizeInput(session, inputId = ('FuentesDatosF13'), 
                           choices = FuentesF13(),
                           selected = "11", server = TRUE)
    })
    
    PartidasF13 <- reactiveVal()
    observe({
      DATA  <- FiltrarBD(
        "ICARO",
        "Select Partida FROM CARGA"
      )
      DATA <- DATA[["Partida"]] %>% 
        unique() %>%
        sort()
      PartidasF13(DATA)
      updateSelectizeInput(session, inputId = ('PartidasDatosF13'), 
                           choices = PartidasF13(),
                           selected = c("421", "422"), server = TRUE)
    })
    
  ### Datos Formulario F13
    
    DatosF13.df  <- reactive({
      
      EjecutadasEnEjercBase <- CargaICARO.df() %>% 
        mutate(Ejercicio = year(Fecha)) %>% 
        filter(Ejercicio == input$EjercicioDatosF13)
      
      if (sum(str_detect(input$AgruparDatosF13, "Obra")) == 0) {
        EjecutadasEnEjercBase <- EjecutadasEnEjercBase %>% 
          select(Base = Imputacion) %>% 
          unique()
        EjecutadasEnEjercBase <- EjecutadasEnEjercBase[["Base"]]
        SQLWhere <- paste0("WHERE C.Imputacion IN (", 
                           paste(rep('?',length(EjecutadasEnEjercBase)),collapse=','),")")
      } else {
        EjecutadasEnEjercBase <- EjecutadasEnEjercBase %>% 
          select(Base = Obra) %>% 
          unique()
        EjecutadasEnEjercBase <- EjecutadasEnEjercBase[["Base"]]
        SQLWhere <- paste0("WHERE C.Obra IN (", 
                           paste(rep('?',length(EjecutadasEnEjercBase)),collapse=','),")")
      }

      
      DATA <- FiltrarBD(
        "ICARO",
        paste0("SELECT A.DescAct , C.* , O.NormaLegal, ",
               "O.Localidad, PY.DescProy FROM PROGRAMAS P ",
               "INNER JOIN SUBPROGRAMAS SP ON P.Programa = SP.Programa ",
               "INNER JOIN PROYECTOS PY ON SP.Subprograma = PY.Subprograma ",
               "INNER JOIN ACTIVIDADES A ON PY.Proyecto = A.Proyecto ",
               "INNER JOIN CARGA C ON A.Actividad = C.Imputacion ",
               "INNER JOIN OBRAS O ON C.Obra = O.Descripcion ",
               SQLWhere
        ),
        params = EjecutadasEnEjercBase
      ) %>%
        mutate(Fecha = zoo::as.Date(Fecha),
               Ejercicio = year(Fecha),
               Fecha = dmy(paste("01", sprintf("%02d", month(Fecha)), year(Fecha), sep = "/")),
               Prog = str_sub(Imputacion, 1, 2),
               Sub = str_sub(Imputacion, 4, 5),
               Proy = str_sub(Imputacion, 7, 8),
               Act = str_sub(Imputacion, -2)) %>%
        select(Ejercicio, Fecha, Fuente, Prog, Sub, Proy, Act, DescAct, DescProy,
               Partida, Obra, Localidad, NormaLegal, Comprobante, Importe, Tipo, Avance)
      
      if (length(input$ProgramasDatosF13) > 0) {
        FiltroPrograma <- input$ProgramasDatosF13
      } else {
        FiltroPrograma <- ProgramasF13()
      }
      if (length(input$FuentesDatosF13) > 0) {
        FiltroFuente <- input$FuentesDatosF13
      } else {
        FiltroFuente <- FuentesF13()
      }
      if (length(input$PartidasDatosF13) > 0) {
        FiltroPartida <- input$PartidasDatosF13
      } else {
        FiltroPartida <- PartidasF13()
      }
      
      Agrupar <- c("Prog", "Sub", "Proy", "Act", "Partida", input$AgruparDatosF13)
      
      DATA <- DATA %>% 
        filter(Prog %in% FiltroPrograma,
               Fuente %in% FiltroFuente,
               Partida %in% FiltroPartida) %>% 
        group_by(!!! rlang::syms(Agrupar)) %>% 
        summarise(EjecutadoBase = sum(Importe[Ejercicio == input$EjercicioDatosF13]),
                  EjecutadoAcumAnteior = sum(Importe) - EjecutadoBase,
                  PrimeraEjecucion = min(Fecha),
                  UltimaEjecucion = max(Fecha),
                  Avance = last(Avance, Fecha))
      
      DATA
      
    })
    
    
    
    #DataTable
    DataTableServer("DTDatos F13", DatosF13.df, DTServer =FALSE)

    
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