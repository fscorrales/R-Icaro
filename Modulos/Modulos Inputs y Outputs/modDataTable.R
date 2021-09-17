
DataTableUI <- function(id) {
  
  ns <- NS(id)

    DT::DTOutput(ns("modDT")) 
  
}

DataTableServer <- function(id, data, selection = "single",
                            DTServer = TRUE) {
  
  stopifnot(is.reactive(data))
  
  moduleServer(id, function(input, output, session) {
    
    # proxy <-DT::dataTableProxy('modDT')
    # DT::clearSearch(proxy)
    
    output$modDT <- DT::renderDT({DT::datatable(data(), style = 'bootstrap',
      extensions = c("Scroller", "Buttons"),
      filter = list(position = 'top', clear = FALSE, plain=T),
      options = list(pageLength = 100, deferRender = T,
                     scroller = T, stateSave=F, filter = "top",
                     searching = T, scrollY = '400px', scrollX = T,
                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                     language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                     dom = 'Brtip'),
      selection = selection
      )
      }, server = DTServer)
    
    NroFila <- reactiveVal()

    observeEvent(input$modDT_rows_selected, {
      req(input$modDT_rows_selected)
      if (length(input$modDT_rows_selected > 0)) {
        NroFila((input$modDT_rows_selected))
      }
      # print(NroFila())
    })
    
    
    # NroFila <- reactive({
    #   
    #   req(input$modDT_rows_selected)
    #   input$modDT_rows_selected
    #   
    # })
    
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
