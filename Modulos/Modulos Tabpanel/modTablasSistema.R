
BaseDeDatosUI <- function(id) {
  
  ns <- NS(id)
  
  tabPanel("Tablas Sistema",
           tabsetPanel(
             tabPanel("Carga ICARO", 
                      DataTableUI(ns("DTBaseDeDatosCargaICARO"))),
             tabPanel("Retenciones ICARO", 
                      DataTableUI(ns("DTBaseDeDatosRetencionesICARO"))),
             tabPanel("Certificados - ICARO", 
                      DataTableUI(ns("DTBaseDeDatosCertificadosICARO"))),
             tabPanel("Rendiciones EPAM - ICARO", 
                      DataTableUI(ns("DTBaseDeDatosRendicionesEPAMICARO"))),
             tabPanel("Estructura ICARO", 
                      DataTableUI(ns("DTBaseDeDatosEstructuraICARO"))),
             tabPanel("Proveedores", 
                      DataTableUI(ns("DTBaseDeDatosProveedores"))),
             tabPanel("Obras", 
                      DataTableUI(ns("DTBaseDeDatosObras")))
           )
  )
  
}

BaseDeDatosServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    DataTableServer("DTBaseDeDatosCargaICARO", CargaIcaro.df)
    
    DataTableServer("DTBaseDeDatosRetencionesICARO", RetencionesIcaro.df)
    
    DataTableServer("DTBaseDeDatosCertificadosICARO", CertificadosIcaroSGF.df)
    
    DataTableServer("DTBaseDeDatosRendicionesEPAMICARO", EPAMIcaroSGF.df)
        
    DataTableServer("DTBaseDeDatosEstructuraICARO", reactive(ProgramasIcaro.df() %>%
                      left_join(SubprogramasIcaro.df(), by = "Programa") %>%
                      left_join(ProyectosIcaro.df(), by = "Subprograma") %>%
                      left_join(ActividadesIcaro.df(), by = "Proyecto")))
    
    DataTableServer("DTBaseDeDatosProveedores", ProveedoresSGF.df)
    
    DataTableServer("DTBaseDeDatosObras", ObrasIcaro.df)

  })
}

ui <- fluidPage(
  BaseDeDatosUI(id = "Prueba")
)

server <- function(input, output) {
  
  BaseDeDatosServer(id = "Prueba")
}

shinyApp(ui, server)