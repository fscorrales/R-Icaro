
ActualizarContabilidadSIIFUI <- function(id) {
  
  ns <- NS(id)
  
  tabPanel("Subsistema Contabilidad SIIF",
           useShinyjs(),
           sidebarLayout(
             sidebarPanel(id = ns("Sidebar"),
                          tabsetPanel(id = ns("switcher"), type = "hidden",
                            tabPanel("1",
                                     
                                     p(style="text-align: center;",
                                       strong("Mayores SIIF")),
                                     p(style="text-align: center;",
                                       strong("Reporte rcocc31")),
                                    
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
                                              ExportButtonUI(ns("SaveActualizaMayorPorCuentaSIIF"),
                                                             "Guardar .xlsx"))
                                     )
                                     
                                     ),
                            
                            tabPanel("2",
                                     p(style="text-align: center;",
                                       strong("Plan de Cuentas")),
                                     p(style="text-align: center;",
                                       strong("Reporte rcopc02")),
                                     
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
                                              ExportButtonUI(ns("SaveActualizaPlanDeCuentasSIIF"),
                                                             "Guardar .xlsx"))
                                     )
                                     
                                     ),                 
                            
                            tabPanel("3",
                                     p(style="text-align: center;",
                                       strong("Asientos Tipo")),
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
                                              ExportButtonUI(ns("SaveActualizaAsientosTipoSIIF"),
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
                                            tabPanel("Mayores", value = "1", 
                                                     ReacTableUI(ns("DTActualizaMayorPorCuentaSIIF"))),
                                            tabPanel("Plan de Cuentas", value = "2", 
                                                     ReacTableUI(ns("DTActualizaPlanDeCuentasSIIF"))),
                                            tabPanel("Asientos Tipos", value = "3",
                                                     ReacTableUI(ns("DTActualizaAsientosTipoSIIF")))
                                            
                                            )
                                )
                       )
                       ) 
           )
           )
  
}

ActualizarContabilidadSIIFServer <- function(id) {
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
    ReacTableServer("DTActualizaMayorPorCuentaSIIF", MayorPorCuentaSIIF.df,
                    selection = NULL)
    ReacTableServer("DTActualizaPlanDeCuentasSIIF", PlanDeCuentasSIIF.df,
                    selection = NULL)
    ReacTableServer("DTActualizaAsientosTipoSIIF", AsientosTipoSIIF.df,
                    selection = NULL)
    
    #Buttons
    ExportButtonServer("SaveActualizaMayorPorCuentaSIIF", MayorPorCuentaSIIF.df,
                       "MayorPorCuentaSIIF", "xlsx")
    ExportButtonServer("SaveActualizaPlanDeCuentasSIIF", PlanDeCuentasSIIF.df,
                       "PlanDeCuentasSIIF", "xlsx")
    ExportButtonServer("SaveActualizaAsientosTipoSIIF", AsientosTipoSIIF.df,
                       "AsientosTipoSIIF", "xlsx")
    
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