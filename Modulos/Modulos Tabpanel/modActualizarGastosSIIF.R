
ActualizarGastosSIIFUI <- function(id) {
  
  ns <- NS(id)
  
  tabPanel("Subsistema Gastos SIIF",
           useShinyjs(),
           sidebarLayout(
             sidebarPanel(id = ns("Sidebar"),
                          tabsetPanel(id = ns("switcher"), type = "hidden",
                            tabPanel("1",
                                     
                                     p(style="text-align: center;",
                                       strong("Comprobantes Gastos SIIF")),
                                     p(style="text-align: center;",
                                       strong("Reporte rcg01_uejp")),
                                    
                                     br(),
                                     
                                     p(style="text-align: justify;",
                                       "Función Importación de Reporte", 
                                       strong("en desarrollo")),
                                     
                                     hr(style = "border-top: 1px solid #000000;"),
                                     
                                     br(),
                                     # FileInputUI(ns("fileAutocargaObras")),
                                     
                                     hr(style = "border-top: 1px solid #000000;"),
                                     
                                     p(style="text-align: center;",
                                       strong("Exportar Base de Datos")),
                                     
                                     fluidRow(
                                       column(12, align = "center",
                                              ExportButtonUI(ns("SaveActualizaComprobantesGtosSIIF"),
                                                             "Guardar .xlsx"))
                                     )
                                     
                                     ),
                            
                            tabPanel("2",
                                     p(style="text-align: center;",
                                       strong("Comprobantes Gastos SIIF con Partida")),
                                     p(style="text-align: center;",
                                       strong("Reporte rcg01_par")),
                                     
                                     br(),
                                     
                                     p(style="text-align: justify;",
                                       "Función Importación de Reporte", 
                                       strong("en desarrollo")),
                                     
                                     hr(style = "border-top: 1px solid #000000;"),
                                     
                                     br(),
                                     # FileInputUI(ns("fileAutocargaObras")),
                                     
                                     hr(style = "border-top: 1px solid #000000;"),
                                     
                                     p(style="text-align: center;",
                                       strong("Exportar Base de Datos")),
                                     
                                     fluidRow(
                                       column(12, align = "center",
                                              ExportButtonUI(ns("SaveActualizaComprobantesGtosPorPartidaSIIF"),
                                                             "Guardar .xlsx"))
                                     )
                                     
                                     ),                 
                            
                            tabPanel("3",
                                     p(style="text-align: center;",
                                       strong("Comprobantes Gastos SIIF por Grupo Partida")),
                                     p(style="text-align: center;",
                                       strong("Reporte gto_rpa03g")),
                                     
                                     br(),
                                     
                                     p(style="text-align: justify;",
                                       "Función Importación de Reporte", 
                                       strong("en desarrollo")),
                                     
                                     hr(style = "border-top: 1px solid #000000;"),
                                     
                                     br(),
                                     # FileInputUI(ns("fileAutocargaObras")),
                                     
                                     hr(style = "border-top: 1px solid #000000;"),
                                     
                                     p(style="text-align: center;",
                                       strong("Exportar Base de Datos")),
                                     
                                     fluidRow(
                                       column(12, align = "center",
                                              ExportButtonUI(ns("SaveActualizaComprobantesGtoGpoPartidaSIIF"),
                                                             "Guardar .xlsx"))
                                     )
                                     
                                     ),
                            
                            tabPanel("4",
                                     p(style="text-align: center;",
                                       strong("Ejecución Presupuesto Anual + Fuente")),
                                     p(style="text-align: center;",
                                       strong("Reporte rf602")),
                                     
                                     hr(style = "border-top: 1px solid #000000;"),
                                     
                                     FileInputUI(ns("fileActualizaEjecPresPorFteSIIF")),
                                     
                                     hr(style = "border-top: 1px solid #000000;"),
                                     
                                     p(style="text-align: center;",
                                       strong("Exportar Base de Datos")),
                                     
                                     fluidRow(
                                       column(12, align = "center",
                                              ExportButtonUI(ns("SaveActualizaEjecPresPorFteSIIF"),
                                                             "Guardar .xlsx"))
                                     )
                                     
                            ),
                            
                            tabPanel("5",
                                     p(style="text-align: center;",
                                       strong("Ejecución Presupuesto Anual + Desc")),
                                     p(style="text-align: center;",
                                       strong("Reporte rf610")),
                                     
                                     br(),
                                     
                                     p(style="text-align: justify;",
                                       "Función Importación de Reporte", 
                                       strong("en desarrollo")),
                                     
                                     hr(style = "border-top: 1px solid #000000;"),
                                     
                                     br(),
                                     # FileInputUI(ns("fileAutocargaObras")),
                                     
                                     hr(style = "border-top: 1px solid #000000;"),
                                     
                                     p(style="text-align: center;",
                                       strong("Exportar Base de Datos")),
                                     
                                     fluidRow(
                                       column(12, align = "center",
                                              ExportButtonUI(ns("SaveActualizaEjecPresConDescSIIF"),
                                                             "Guardar .xlsx"))
                                     )
                                     
                            ),
                            
                            tabPanel("6",
                                     p(style="text-align: center;",
                                       strong("Deuda Flotante")),
                                     p(style="text-align: center;",
                                       strong("Reporte rdeu012")),
                                     
                                     br(),
                                     
                                     p(style="text-align: justify;",
                                       "Función Importación de Reporte", 
                                       strong("en desarrollo")),
                                     
                                     hr(style = "border-top: 1px solid #000000;"),
                                     
                                     br(),
                                     # FileInputUI(ns("fileAutocargaObras")),
                                     
                                     hr(style = "border-top: 1px solid #000000;"),
                                     
                                     p(style="text-align: center;",
                                       strong("Exportar Base de Datos")),
                                     
                                     fluidRow(
                                       column(12, align = "center",
                                              ExportButtonUI(ns("SaveActualizaDeudaFlotanteSIIF"),
                                                             "Guardar .xlsx"))
                                     )
                                     
                            ) 
                            
                            )
                          
             ),
             mainPanel(id = ns("Main"),
                       
                       fluidRow(
                         column(1, HideSidebarButtonUI(ns("showpanel"))),
                         column(11, 
                                tabsetPanel(id = ns("controller"),
                                            tabPanel("Gastos", value = "1", 
                                                     ReacTableUI(ns("DTActualizaComprobantesGtosSIIF"))),
                                            tabPanel("Gastos + Partida", value = "2", 
                                                     ReacTableUI(ns("DTActualizaComprobantesGtosPorPartidaSIIF"))),
                                            tabPanel("Gastos + Gpo Partida", value = "3",
                                                     ReacTableUI(ns("DTActualizaComprobantesGtoGpoPartidaSIIF"))),
                                            tabPanel("Presupuesto + Fte", value = "4",
                                                     ReacTableUI(ns("DTActualizaEjecPresPorFteSIIF"))),
                                            tabPanel("Presupuesto + Desc", value = "5",
                                                     ReacTableUI(ns("DTActualizaEjecPresConDescSIIF"))),
                                            tabPanel("Deuda", value = "6",
                                                     ReacTableUI(ns("DTActualizaDeudaFlotanteSIIF")))
                                            
                                            )
                                )
                       )
                       ) 
           )
           )
  
}

ActualizarGastosSIIFServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    FileInputServer("fileActualizaEjecPresPorFteSIIF", 
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
    
    #DataTable
    
    ReacTableServer("DTActualizaComprobantesGtosSIIF", ComprobantesGtosSIIF.df, 
                    selection = NULL)

    ReacTableServer("DTActualizaComprobantesGtosPorPartidaSIIF", ComprobantesGtosPorPartidaSIIF.df, 
                    selection = NULL)
    
    ReacTableServer("DTActualizaComprobantesGtoGpoPartidaSIIF", ComprobantesGtoGpoPartidaSIIF.df, 
                    selection = NULL)
    
    ReacTableServer("DTActualizaEjecPresPorFteSIIF", EjecPresPorFteSIIF.df, 
                    selection = NULL)
    
    ReacTableServer("DTActualizaEjecPresConDescSIIF", EjecPresConDescSIIF.df, 
                    selection = NULL)
    
    ReacTableServer("DTActualizaDeudaFlotanteSIIF", DeudaFlotanteSIIF.df, 
                    selection = NULL)
    
    #Buttons
    ExportButtonServer("SaveActualizaComprobantesGtosSIIF", ComprobantesGtosSIIF.df,
                       "ComprobantesGtosSIIF", "xlsx")
    ExportButtonServer("SaveActualizaComprobantesGtosPorPartidaSIIF", ComprobantesGtosPorPartidaSIIF.df,
                       "ComprobantesGtosPorPartidaSIIF", "xlsx")
    ExportButtonServer("SaveActualizaComprobantesGtoGpoPartidaSIIF", ComprobantesGtoGpoPartidaSIIF.df,
                       "ComprobantesGtoGpoPartidaSIIF", "xlsx")
    ExportButtonServer("SaveActualizaEjecPresPorFteSIIF", EjecPresPorFteSIIF.df,
                       "EjecPresPorFteSIIF", "xlsx")
    ExportButtonServer("SaveActualizaEjecPresConDescSIIF", EjecPresPorFteSIIF.df,
                       "EjecPresPorFteSIIF", "xlsx")
    ExportButtonServer("SaveActualizaDeudaFlotanteSIIF", DeudaFlotanteSIIF.df,
                       "DeudaFlotanteSIIF", "xlsx")
    
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