
ActualizarICAROUI <- function(id) {
  
  ns <- NS(id)
  
  tabPanel("ICARO",
           useShinyjs(),
           sidebarLayout(
             sidebarPanel(id = ns("Sidebar"),
                          p(style="text-align: center;",
                            strong("Tablas ICARO")),
                          p(style="text-align: center;",
                            strong("actualiza todo de una vez")),
                          
                          br(),
                          p(style="text-align: justify;",
                            "Función Importación de Base de Datos", 
                            strong("en desarrollo")),
                          hr(style = "border-top: 1px solid #000000;"),
                          br(),
                          hr(style = "border-top: 1px solid #000000;")
                          ),

             mainPanel(id = ns("Main"),
                       
                       fluidRow(
                         column(1, HideSidebarButtonUI(ns("showpanel"))),
                         column(11, 
                                tabsetPanel(id = ns("controller"),
                                            tabPanel("Prog", value = "1", 
                                                     DataTableUI(ns("DTActualizaProgramasICARO"))),
                                            tabPanel("Subprog", value = "2", 
                                                     DataTableUI(ns("DTActualizaSubprogramasICARO"))),
                                            tabPanel("Proy", value = "3",
                                                     DataTableUI(ns("DTActualizaProyectosICARO"))),
                                            tabPanel("Actividades", value = "4",
                                                     DataTableUI(ns("DTActualizaActividadesICARO"))),
                                            tabPanel("Obras", value = "5",
                                                     DataTableUI(ns("DTActualizaObrasICARO"))),
                                            tabPanel("Carga", value = "6",
                                                     DataTableUI(ns("DTActualizaCargaICARO"))),
                                            tabPanel("Retenciones", value = "7",
                                                     DataTableUI(ns("DTActualizaRetencionesICARO"))),
                                            tabPanel("Certificados", value = "8",
                                                     DataTableUI(ns("DTActualizaCertificadosICARO"))),
                                            tabPanel("EPAM", value = "9",
                                                     DataTableUI(ns("DTActualizaRendEPAMICARO")))
                                            
                                            )
                                )
                       )
                       ) 
           )
           )
  
}

ActualizarICAROServer <- function(id) {
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
    
    DataTableServer("DTActualizaProgramasICARO", ProgramasICARO.df)

    DataTableServer("DTActualizaSubprogramasICARO", SubprogramasICARO.df)
    
    DataTableServer("DTActualizaProyectosICARO", ProyectosICARO.df)
    
    DataTableServer("DTActualizaActividadesICARO", ActividadesICARO.df)
    
    DataTableServer("DTActualizaObrasICARO", ObrasICARO.df)
    
    DataTableServer("DTActualizaCargaICARO", CargaICARO.df)
    
    DataTableServer("DTActualizaRetencionesICARO", RetencionesICARO.df)
    
    DataTableServer("DTActualizaCertificadosICARO", CertificadosICARO.df)
    
    DataTableServer("DTActualizaRendEPAMICARO", RendEPAMICARO.df)
    
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