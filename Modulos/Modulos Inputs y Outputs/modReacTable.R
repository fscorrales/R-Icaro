
ReacTableUI <- function(id) {
  
  ns <- NS(id)

  reactableOutput(ns("table"))
  
}

ReacTableServer <- function(id, data, selection = "single", filterable = TRUE, 
                            defaultSortOrder = "asc", defaultSorted = NULL,
                            ListColDef = list(), height = 500, compact = FALSE,
                            defaultColDef = NULL, wrap = FALSE, striped = TRUE,
                            defaultPageSize = 10, pageSizeOptions = c(10, 25, 50, 100)) {
  
  stopifnot(is.reactive(data))
  
  moduleServer(id, function(input, output, session) {
    
    output$table <- renderReactable({
      reactable(data(), selection = selection, onClick = "select", 
                filterable = filterable, pagination = TRUE, highlight = TRUE,
                showPageInfo = FALSE, showPageSizeOptions = TRUE, paginationType = "simple",
                defaultPageSize = defaultPageSize, pageSizeOptions = pageSizeOptions,
                resizable = TRUE,  fullWidth = TRUE, wrap = wrap, height = height,
                columns = ListColDef, compact = compact,  striped = striped,
                defaultSortOrder = defaultSortOrder, defaultSorted = defaultSorted,
                defaultColDef = defaultColDef,
                theme = reactableTheme(
                  rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
                ),
                language = reactableLang(
                  pagePrevious = "Anterior",
                  pageNext = "Siguiente",
                  pageSizeOptions = "Mostrar {rows}",
                  noData = "No se encontraron Datos"
                )
                )
    })
    
    # NroFila <- reactive(getReactableState("table", "selected"))
    
    NroFila <- reactiveVal()
    
    observeEvent(getReactableState("table"), {
      req(getReactableState("table"))
      Selected <- getReactableState("table", name = "selected")
      if (!is.null(Selected)) {
        NroFila(Selected)
      } else {
        NroFila(0)
      }
      
    })
    
    
    return({
      list(
        NroFila = NroFila
        # n =  (isolate(i()))
      )
    })
    
    # return(reactive(input$modDT_rows_selected))
                                            
  })
}

# ui <- fluidPage(
#   
#   DataTableUI(id = "Prueba")
# )
# 
# server <- function(input, output) {
#   
#   ProgramasIcaro.df <- reactiveFileReader(1000, NULL, 
#                                           "ICARO Programas.csv",
#                                           read_csv, col_types = "cc")
#   DataTableServer(id = "Prueba", ProgramasIcaro.df)
#   
# }
# 
# shinyApp(ui, server)
