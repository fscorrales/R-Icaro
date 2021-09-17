
ProveedoresUI <- function(id) {
  
  ns <- NS(id)
  
  tabPanel("Proveedores",
    sidebarLayout(
      sidebarPanel(id = ns("Sidebar"),
                   p(style="text-align: justify;",
                     "En caso que desee", strong("actualizar"), "la", 
                     strong("Base de Datos de Contratistas / Proveedores de ICARO"),
                     ", deberá proceder a", strong("importar"),
                     "desde el", strong("Sistema de Gestión Financiera (SGF)")),
                   p(style="text-align: justify;",
                     "La opción de carga manual aún no está habilitada"),
                   hr(style = "border-top: 1px solid #000000;"),
                   FileInputUI(ns("modProveedores")),
                   hr(style = "border-top: 1px solid #000000;")
      ),
      mainPanel(id = ns("Main"),
        fluidRow(
          useShinyjs(),
          column(1, HideSidebarButtonUI(ns("showpanel"))),
          column(11, ReacTableUI(ns("DT")))
        )
      ) 
    )
  )

}

ProveedoresServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ReacTableServer("DT", reactive(ProveedoresSGF.df() %>%
                                     select(CUIT, Descripcion, CondicionIVA)),
                    ListColDef = list(CUIT = colDef(width = 120),
                                      Descripcion = colDef(minWidth = 300)),
                    selection = NULL)
    
    toogleSidebar <- reactive(HideSidebarButtonServer("showpanel"))
    
    observeEvent(toogleSidebar(), {

      if(toogleSidebar() == TRUE) {
        removeCssClass("Main", "col-sm-12")
        addCssClass("Main", "col-sm-8")
        shinyjs::show(id = session$ns("Sidebar"), asis = TRUE)
        shinyjs::enable(id = session$ns("Sidebar"), asis = TRUE)
      } else {
        removeCssClass("Main", "col-sm-8")
        addCssClass("Main", "col-sm-12")
        shinyjs::hide(id = session$ns("Sidebar"), asis = TRUE)
      }

    })
    
    FileInputServer("modProveedores", 
                    "ListadoProveedores", 
                    ContenidoAyuda = paste0(
                      "<ol>",
                      "<li>Ingrese al <strong>SGF</strong>, y seleccione el menú ",
                      "<strong>Archivo / Proveedores / Listado de Proveedores</strong></li>",
                      "<li>Presione el botón <strong>Exportar</strong></li>",
                      "<li>En la ventana emergente, mantenga la opción <strong>Archivo...</strong> ",
                      "antes de presionar aceptar</li>",
                      "<li>Elija el destino del archivo a descargar y preste atención a que el tipo sea ",
                      "<strong>.csv</strong></li>",
                      "<li><strong>Importar</strong> el archivo descargado previamente</li>",
                      "</ol>")
                    )
    
  })
}

ui <- fluidPage(
  ProveedoresUI(id = "Prueba")
)

server <- function(input, output) {
  ProveedoresServer(id = "Prueba")
}

shinyApp(ui, server)