library(shiny)
library(shinyFeedback)


FileInputUI <- function(id) {

  ns <- NS(id)
  
  tagList(
    
    useShinyFeedback(),
    fileInput(ns("file"), 
              tagList(
                tags$span("Importar archivo"), 
                tags$span(icon("info-circle"), id = ns("icon"), style = "color: blue;")
              ),
              multiple = FALSE,
              buttonLabel = "Importar", placeholder = "Archivo no seleccionado")
    # bsPopover("icon", "Ayuda de Importación",
    #           content = unlist(ContenidoAyuda, use.names = FALSE),
    #           placement = "right")
    
  )

  
}

FileInputServer <- function(id, ArchivoImportar, 
                            ContenidoAyuda = "", 
                            ExtensionArchivo = "csv") {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    shinyBS::addPopover(session, ns("icon"), "Ayuda Importación:", 
                        ContenidoAyuda, placement = "right")
    
    userFile <- reactive(req(input$file))
    
    ext <- reactive({

      tools::file_ext(userFile()$name)

    })
    
    data <- reactive({
      
      # ext <- tools::file_ext(userFile()$name)
      switch(ext(),
             csv = ImportarCSV(userFile()$datapath, ArchivoImportar),
             xls = ImportarXLS(userFile()$datapath, ArchivoImportar),
             FALSE
             # tsv = vroom::vroom(input$file$datapath, delim = "\t"),
             # validate("Invalid file; Please upload a .csv or .tsv file")
      )
    })
    
    observeEvent(data(), {
      
      hideFeedback(inputId = "file")
      
      if (ext() != ExtensionArchivo) {
        showFeedbackDanger(inputId = "file",
                           text = paste0("Archivo inválido;",
                                         "por favor cargar un archivo .",
                                         ExtensionArchivo))
      } else {
        if (data() == FALSE) {
          showFeedbackDanger(inputId = "file",
                             text = "Archivo incorrecto, VERIFICAR")
        } else {
          showFeedbackSuccess(inputId = "file",
                              text = "Carga COMPLETA")
        }
      }
      
    })

    # observe({
    #   msg <- sprintf("Comprobando contenido data(): %s", data())
    #   cat(msg, "\n")
    # })
    
  })
}

ui <- fluidPage(
  FileInputUI(id = "numberFoo")
)

server <- function(input, output) {
  FileInputServer(id = "numberFoo")
}

shinyApp(ui, server)

