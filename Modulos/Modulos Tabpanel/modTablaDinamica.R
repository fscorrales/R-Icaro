TablasAnexadas <- c("Carga Completa ICARO", "Estructura Completa ICARO")


TablaDinamicaUI <- function(id) {
  
  ns <- NS(id)
  
  ListaDeTablas <- c(TablasAnexadas, ListadoTablasBD("SIIF"), ListadoTablasBD("SSCC"),
                     ListadoTablasBD("SGF"), ListadoTablasBD("SGO"))
  
  tabPanel("Tabla Dinamica",
           sidebarLayout(
             sidebarPanel(id = ns("Sidebar"),
                          p(style="text-align: center;",
                            strong("Tabla Dinámica personalizable")),
                          
                          br(),
                          
                          p(style="text-align: justify;",
                           "Primero, deberá elegir una de las tablas del siguiente listado.",
                           "Luego, personalice la tabla en función de la información que",
                           "desea obtener. Puede ubicar los campos en filas o columnas (arrastrándolos),",
                           "filtrar los mismos y elegir una función de agregación",
                           "(suma, cuenta, etc.). Además, puede seleccionar distintos",
                           "tipos de salida (tablas o gráficos)."),
                          
                          hr(style = "border-top: 1px solid #000000;"),
                          
                          selectizeInput(ns("Tabla"), "Selección de Tabla",
                                         choices = ListaDeTablas, selected = "Carga Completa ICARO", multiple = F, 
                                         options = list(placeholder = "Elegir una opción")),
                          
                          hr(style = "border-top: 1px solid #000000;"),
                          
                          p(style="text-align: center;",
                            strong("Exportar Tabla Dinámica")),
                          fluidRow(
                            column(6, align = "center",
                                   downloadButton(ns("GuardarXLSX"),"Guardar .xlsx", width = "125px")),
                            column(6, align = "center",
                                   downloadButton(ns("GuardarCSV"),"Guardar .csv", width = "125px"))
                          ),
                          
                          br(),
                          
                          p(style="text-align: justify;",
                            "Tenga en cuenta que",
                            strong("solo prodrá exportar el tipo de salida tabla"))
                          
                          
                          ),
             mainPanel(id = ns("Main"),
                       fluidRow(
                         useShinyjs(),
                         tags$head(tags$style(
                           type = 'text/css',
                           paste0("#", ns("table"), " { overflow-x: scroll; }")
                         )),
                         column(1, HideSidebarButtonUI(ns("showpanel"))),
                         column(11, rpivotTableOutput(ns("table")))
                         )
                       )
             )
           )

}

TablaDinamicaServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
     
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
    
    Tabla.df <- reactive({
      
      req(input$Tabla)
      
      if (input$Tabla %in% TablasAnexadas) {
        
        if (input$Tabla == "Estructura Completa ICARO") {
          SQLstring <- paste0("SELECT P.Programa, P.DescProg, ",
                              "SP.Subprograma, SP.DescSubprog, ", 
                              "PY.Proyecto, PY.DescProy, ",
                              "A.Actividad, A.DescAct, O.* FROM PROGRAMAS P ",
                              "INNER JOIN SUBPROGRAMAS SP ON P.Programa = SP.Programa ",
                              "INNER JOIN PROYECTOS PY ON SP.Subprograma = PY.Subprograma ",
                              "INNER JOIN ACTIVIDADES A ON PY.Proyecto = A.Proyecto ",
                              "INNER JOIN OBRAS O ON A.Actividad = O.Imputacion"
                              )
          
          Ans <- FiltrarBD("ICARO", SQLstring)
          Ans <- Ans %>% 
            select(-Imputacion)
        }
        
        if (input$Tabla == "Carga Completa ICARO") {
          SQLstring <- paste0("SELECT P.Programa, P.DescProg, ",
                              "SP.Subprograma, SP.DescSubprog, ", 
                              "PY.Proyecto, PY.DescProy, ",
                              "A.DescAct , C.* , ",
                              "O.Localidad, O.InformacionAdicional FROM PROGRAMAS P ",
                              "INNER JOIN SUBPROGRAMAS SP ON P.Programa = SP.Programa ",
                              "INNER JOIN PROYECTOS PY ON SP.Subprograma = PY.Subprograma ",
                              "INNER JOIN ACTIVIDADES A ON PY.Proyecto = A.Proyecto ",
                              "INNER JOIN CARGA C ON A.Actividad = C.Imputacion ",
                              "INNER JOIN OBRAS O ON C.Obra = O.Descripcion"
          )
          
          Ans <- FiltrarBD("ICARO", SQLstring)
          Ans <- Ans %>% 
            mutate(Fecha = zoo::as.Date(Fecha),
                   Ejercicio = year(Fecha),
                   Mes = month(Fecha)) %>% 
            select(-Fecha)
            # 'Mes/Ejercicio' = str_c(month(Fecha), 
            #                         str_sub(Ejercicio, -2), sep = "/")) %>% 
            
        }
        
      } else {

        Ans <- eval(call(paste0(input$Tabla, ".df")))
        
      }
      
      Ans
      
    })
    
    output$table = renderRpivotTable({
      rpivotTable(Tabla.df(),
                  rendererName="Tabla Dinamica R INVICO",
                  onRefresh = htmlwidgets::JS(paste0("function(config) {Shiny.onInputChange('", ns("myData"), "', 
                                            document.getElementById('", ns("table"),"').innerHTML); 
  }")))
    })
    
    summarydf <- eventReactive(input$myData,{
      input$myData %>%
        read_html %>%
        html_table(fill = TRUE) %>%
        .[[2]]
    })
    
    output$GuardarXLSX <- downloadHandler(

      filename = function() {
        paste("R INVICO TD-", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        if(nrow(summarydf() )<1) return()
        openxlsx::write.xlsx(summarydf(), file)
      }
    )
    
    output$GuardarCSV <- downloadHandler(
      
      filename = function() {
        paste("R INVICO TD-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        if(nrow(summarydf() )<1) return()
        openxlsx::write.xlsx(summarydf(), file)
      }
    )
    
    # observeEvent( input$GuardarXLSX, {
    #   if(nrow(summarydf() )<1) return()
    #   
    #   openxlsx::write.xlsx(summarydf(), paste0(DATA_PATH, "R Output/Tablas Dinamicas/R INVICO TD.xlsx"))
    # })
    # 
    # observeEvent( input$GuardarCSV, {
    #   if(nrow(summarydf() )<1) return()
    #   write_excel_csv(summarydf(), paste0(DATA_PATH, "R Output/Tablas Dinamicas/R INVICO TD.csv"))
    # })
    
  })
}

# ui <- fluidPage(
#   ProveedoresUI(id = "Prueba")
# )
# 
# server <- function(input, output) {
#   ProveedoresServer(id = "Prueba")
# }
# 
# shinyApp(ui, server)