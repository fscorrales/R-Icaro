useShinyjs()

function(input, output, session) {
  
  CargaServer("Carga", parent_session = session)
  
  AutocargaServer("Autocarga")
  
  EstructuraServer("Estructura")
  
  ObrasServer("Obras")
  
  ProveedoresServer("Proveedores")
  
  ControlCargaServer("ControlCarga")
  
  BaseDeDatosServer("Tablas Sistema")
  
  ReporteModulosBasicosServer("ReporteMB")
  
  TablaDinamicaServer("TablaDinamica")
  
  # ProveedoresSPServer("Prueba")
  
  # session$onSessionEnded(stopApp)
  
}