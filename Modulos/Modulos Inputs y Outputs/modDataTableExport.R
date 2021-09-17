
DataTableExportUI <- function(id) {
  
  ns <- NS(id)
  tagList(
    downloadButton(ns("download1"), ""), # no label: this button will be hidden
    DT::DTOutput(ns("modDT")) 
  )
}

DataTableExportServer <- function(id, data, selection = "single",
                            DTServer = TRUE) {
  
  stopifnot(is.reactive(data))
  
  moduleServer(id, function(input, output, session) {
    
    ns = session$ns
    
    callback <- JS(paste0(
      "var a = document.createElement('a');",
      "$(a).addClass('dt-button');",
      "a.href = document.getElementById('", ns("download1"), "').href;",
      "a.download = '';",
      "$(a).attr('target', '_blank');",
      "$(a).text('Excel');",
      "$('div.", ns("dwnld"), "').append(a);",
      "$('#", ns("download1"), "').hide();"
    ))
    
    # proxy <-DT::dataTableProxy('modDT')
    # DT::clearSearch(proxy)
    
    output$modDT <- DT::renderDT({
      DT::datatable(data(), 
                    callback = callback,
                    extensions = 'Buttons',
                    options = list(
                      dom = paste0("'B<", ns("dwnld"), ">frtip'"),
                      buttons = list(
                        "copy"
                      )
                    )
      )
      }, server = DTServer)
    
    NroFila <- reactiveVal()

    output$download1 <- downloadHandler(
      filename = function() {
        paste("R INVICO-", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        openxlsx::write.xlsx(data(), file)
      }
    )
    
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
