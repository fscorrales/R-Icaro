# ICARO Layout
navbarPage("ICARO", id = "ICARO",
           theme = shinytheme("sandstone"),
           navbarMenu("Registro",
                      CargaUI("Carga"),
                      AutocargaUI("Autocarga")
           ),
           EstructuraUI("Estructura"),
           ObrasUI("Obras"),
           ProveedoresUI("Proveedores"),
           navbarMenu("Controles",
                      ControlCargaUI("ControlCarga")
                      # ProveedoresSPUI("Prueba")
           ),
           navbarMenu("Reportes",
                      BaseDeDatosUI("Tablas Sistema"),
                      ReporteModulosBasicosUI("ReporteMB"),
                      TablaDinamicaUI("TablaDinamica")
                      ),
           tabPanel("Configuración", shinythemes::themeSelector()) #More in https://bootswatch.com/
)