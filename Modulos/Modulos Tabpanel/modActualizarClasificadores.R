
ActualizarClasificadoresSIIFUI <- function(id) {
  
  ns <- NS(id)
  
  tabPanel("Clasificadores SIIF",
           useShinyjs(),
           sidebarLayout(
             sidebarPanel(id = ns("Sidebar"),
                          tabsetPanel(id = ns("switcher"), type = "hidden",
                            tabPanel("1",
                                     
                                     p(style="text-align: center;",
                                       strong("Partidas SIIF")),
                                     p(style="text-align: center;",
                                       strong("Reporte rog01")),
                                    
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
                                              ExportButtonUI(ns("SaveActualizaPartidasSIIF"),
                                                             "Guardar .xlsx"))
                                     )
                                     
                                     ),
                            
                            tabPanel("2",
                                     p(style="text-align: center;",
                                       strong("Fuentes SIIF")),
                                     p(style="text-align: center;",
                                       strong("Reporte rff01")),
                                     
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
                                              ExportButtonUI(ns("SaveActualizaFuentesSIIF"),
                                                             "Guardar .xlsx"))
                                     )
                                     
                                     ),                 
                            
                            tabPanel("3",
                                     p(style="text-align: center;",
                                       strong("Cuentas Bancarias SIIF")),
                                     p(style="text-align: center;",
                                       strong("Reporte rcu02")),
                                     
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
                                              ExportButtonUI(ns("SaveActualizaCuentasBancariasSIIF"),
                                                             "Guardar .xlsx"))
                                     )
                                     
                                     ),
                            
                            tabPanel("4",
                                     p(style="text-align: center;",
                                       strong("Tipo de Comprobantes SIIF")),
                                     p(style="text-align: center;",
                                       strong("Elaboración Personal")),
                                     
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
                                              ExportButtonUI(ns("SaveActualizaTipoComprobanteSIIF"),
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
                                            tabPanel("Partidas", value = "1", 
                                                     ReacTableUI(ns("DTActualizaPartidasSIIF"))),
                                            tabPanel("Fuentes", value = "2", 
                                                     ReacTableUI(ns("DTActualizaFuentesSIIF"))),
                                            tabPanel("Cuentas Bancarias", value = "3",
                                                     ReacTableUI(ns("DTActualizaCuentasBancariasSIIF"))),
                                            tabPanel("Tipo de Comprobantes", value = "4",
                                                     ReacTableUI(ns("DTActualizaTipoComprobanteSIIF")))
                                            
                                            )
                                )
                       )
                       ) 
           )
           )
  
}

ActualizarClasificadoresSIIFServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # FileInputServer("fileAutocargaObras", 
    #                 ArchivoImportar = "CertificadosObrasSGF",
    #                 ContenidoAyuda = paste0(
    #                   "<ol>",
    #                   "<li>Ingrese al <strong>SGF</strong>, y seleccione el menú ",
    #                   "<strong>Informes / Certificados de Obra / Informe para Contable [Certificados]</strong></li>",
    #                   "<li>Seleccione <strong>Mes</strong> y <strong>Año</strong> ",
    #                   "<li>Presione el botón <strong>Exportar</strong></li>",
    #                   "<li>En la ventana emergente, mantenga la opción <strong>Archivo...</strong> ",
    #                   "antes de presionar aceptar</li>",
    #                   "<li>Elija el destino del archivo a descargar y preste atención a que el tipo sea ",
    #                   "<strong>.csv</strong></li>",
    #                   "<li><strong>Importar</strong> el archivo descargado previamente</li>",
    #                   "</ol>")
    #                 )
    
    
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
    ReacTableServer("DTActualizaPartidasSIIF", PartidasSIIF.df,
                    selection = NULL)
    ReacTableServer("DTActualizaFuentesSIIF", FuentesSIIF.df,
                    selection = NULL)
    ReacTableServer("DTActualizaCuentasBancariasSIIF", CuentasBancariasSIIF.df,
                    selection = NULL)
    ReacTableServer("DTActualizaTipoComprobanteSIIF", TipoComprobanteSIIF.df,
                    selection = NULL)
    
    #Buttons
    ExportButtonServer("SaveActualizaPartidasSIIF", PartidasSIIF.df,
                       "PartidasSIIF", "xlsx")
    ExportButtonServer("SaveActualizaFuentesSIIF", FuentesSIIF.df,
                       "FuentesSIIF", "xlsx")
    ExportButtonServer("SaveActualizaCuentasBancariasSIIF", CuentasBancariasSIIF.df,
                       "CuentasBancariasSIIF", "xlsx")
    ExportButtonServer("SaveActualizaTipoComprobanteSIIF", TipoComprobanteSIIF.df,
                       "TipoComprobanteSIIF", "xlsx")
    
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